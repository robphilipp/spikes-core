package com.digitalcipher.spiked.apputils

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.digitalcipher.spiked.apputils.RemotingPortManager.parseRemotingPortsFrom
import com.digitalcipher.spiked.apputils.SeriesRunner._
import com.digitalcipher.spiked.apputils.SpikesAppUtils.{buildNetwork, initializeSimulationTimes, shutdownSystemFunctionFactory}
import com.digitalcipher.spiked.construction.NetworkBuilder.RemoteGroupInfo
import com.digitalcipher.spiked.inputs.EnvironmentFactory
import com.digitalcipher.spiked.logging.EventLogger
import com.digitalcipher.spiked.logging.FileEventLogger.FileLoggingConfiguration
import com.digitalcipher.spiked.logging.KafkaEventLogger.KafkaConfiguration
import com.digitalcipher.spiked.neurons.SignalClock
import com.digitalcipher.spiked.topology.Network.{RetrieveNeurons, SimulationStart, SimulationStartResponse}
import com.typesafe.config.{Config, ConfigValueFactory}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.util.matching.Regex

/**
  * The series runner provides a set of utilities to facilitate construction and running a series of spiking neural
  * network simulations.
  * 
  * === SeriesRunner instance ===
  * The series runner performs a number of common tasks:
  *
  * 1. Sets up the application logging that logs mostly set-up and start-up of the networks
  * 2. Loads the configuration file that specifies whether to run locally or distributed
  * 3. Sets up the event logging based on the event loggers specified
  *
  * When constructing a series runner you must specify:
  *
  * 1. The time-factor which specifies how many seconds in real-time 1 second of simulation time takes.
  * 2. The name of the application logger
  * 3. The name of the config file in the resources directory
  * 4. The base name of the actor system. When creating a series, each actor system will have the
  *    actor system number appended to the base name.
  * 5. A list of event loggers that will capture the neural network events
  *
  * === creating the networks ===
  * Second, creating the neural network, whether local or distributed, requires
  * a call to the series runner's `createNetworks(...)` method. This method accepts two parameters: the
  * number of networks to create from the DNA file, and the location of the DNA file describing the network.
  * Running more than one network at a time produces a series of runs. Recall, that each run, even with
  * identical signals will differ because of configured noise, and scheduling noise (and this is a good thing.)
  *
  * === running the simulation series ===
  * Once the networks are created, a call to the series runner's `runSimulationSeries(...)` method starts
  * the simulation. The method's first argument is a list of the successful network creation attempts from
  * the first call. And the second argument is the factor for creating environments that send signals to the
  * neural networks. Each neural network has its own environment, and the environment factory defines how
  * the signals are sent.
  *
  * @param timeFactor     The simulation time-factor that defines how many seconds of real time yield 1 second of
  *                       simulation time. For example, setting the simulation time factor (a time-factor of N means
  *                       that 1 second of simulation time equals N seconds of real time). A time-factor of 1 means
  *                       the simulation is running in real-time.
  * @param appLoggerName  The name of the application logger. This is the logger for setting up the application, and
  *                       not the neural network event logger.
  * @param config The configuration for running the series of spikes networks
  * @param systemBaseName The base name of the actor system. When creating more than one actor system, the run
  *                       number will be appended to this name.
  * @param eventLogging   The neural network event loggers to use. By default will use file-event logging.
  */
class SeriesRunner(timeFactor: Int = 1,
                   appLoggerName: String,
                   config: Config,
                   systemBaseName: String = "spiked_system",
                   eventLogging: Seq[EventLogging] = Seq(FileEventLogging())) {

  val logger: Logger = LoggerFactory.getLogger(appLoggerName)

  // display the source of the configuration
  private val configSource = config.getConfig("config-source")
  logger.info("configuration source: {}", configSource)

  // display the configured scheduler
  private val schedulerConfig = config.getConfig("akka.scheduler")
  logger.info("scheduler; {}", schedulerConfig)

  // holds the system-name to port association
  private val portManager = RemotingPortManager(parseRemotingPortsFrom(config).getOrElse(List.empty))

  // the default port for remoting, in case no list of remoting ports was specified
  private val defaultRemotingPort = config.getInt("akka.remote.netty.tcp.port")

  /**
    * Creates a set of actor-systems and neural networks and returns a [[com.digitalcipher.spiked.apputils.SeriesRunner]]
    * holding all the information about the successfully created actor systems and neural networks. It also holds
    * the information about the failed create attempts. For example,
    * {{{
    *   // create the actor systems and the neural networks
    *   val results = createNetworks(10, "wta-simple.boo", true)
    *
    *   // if there are any failures, then print them out
    *   if(results.hasFailures) {
    *     println("failures occurred")
    *     results.failures.foreach(failure -> println(failure.mkString("; ")
    *   }
    *
    *   // run the simulation for all the neural networks that were created
    *   runSimulationSeries(results.successes)
    * }}}
    * @param num           The number of simulations to run in this series
    * @param dnaFile       The spikes DNA file holding the network description
    * @param reparseReport When set to `true` then emits a DNA string from the parsed DNA file and reparses it
    *                      as a test that the parsing and emitting are consistent.
    * @return For each actor system and network successfully created, returns
    *         `(actor_system, network_actor_ref, map(group_name -> group_info))` tuple.
    *         For each actor system or network that could not be built, returns a list of error messages.
    */
  def createNetworks(num: Int = 1,
                     dnaFile: String,
                     reparseReport: Boolean = true): CreateNetworkResults = {
    CreateNetworkResults(
      List.range(1, num+1).map(i => {
        // the actor system name
        val systemName = s"$systemBaseName-$i"

        // check out a remoting port (if remoting)
        portManager.checkOutRemotingPort(systemName).orElse(Option(defaultRemotingPort))
          .map(port => {
            logger.info("starting to build the network spiked_system-{}; time: {}", i, SignalClock.current())
            val systemConfig = config.withValue("akka.remote.netty.tcp.port", ConfigValueFactory.fromAnyRef(port))
            val system = ActorSystem(systemName, systemConfig)

            //
            // set-up logging, if specified
            eventLogging.foreach {
              case FileEventLogging(logFile) =>
                EventLogger.withLogging(FileLoggingConfiguration(system, logFile(i)))

              case KafkaEventLogging(configName, topic) =>
                EventLogger.withLogging(KafkaConfiguration(system, systemConfig.getConfig(configName), topic(i)))
            }

            //
            // build the network and get back information about any remote groups
            // this returns pair(actorRef -> map(remote-group-id, remote-group-info))
            val networkResult = buildNetwork(
              actorSystem = system,
              timeFactor = timeFactor,
              dnaFilename = dnaFile,
              reparseReport = reparseReport
            )

            networkResult match {
              case Right((network, remoteGroups)) => Right(CreateNetworkResult(system, network, remoteGroups))
              case Left(errors) => Left(errors)
            }
          })
          .getOrElse(Left(List("Unable to retrieve port from remoting port manager")))
      })
    )
  }

  /**
    * Runs the simulation series for each of the created actor systems and neural network. This call creates
    * and environment for each actor-system (i.e. neural network), and has the environment start sending
    * signals to the neurons specified as ''input neurons''.
    *
    * @param networkResults The list holding the results of the actor system and neural network creation.
    * @param environmentFactory The factory for creating an environment that sends signals to the neural network.
    * @param inputNeuronSelector The regular expression used to select the input neurons from the network
    */
  def runSimulationSeries(networkResults: List[CreateNetworkResult],
                          environmentFactory: EnvironmentFactory,
                          inputNeuronSelector: Regex): Unit = {
    networkResults.foreach(result => {
      runSimulation(
        actorSystem = result.system,
        remoteGroups = result.remoteGroups,
        network = result.network,
        environmentFactory = environmentFactory,
        clock = initializeSimulationTimes(timeFactor),
        inputNeuronSelector = inputNeuronSelector
      )
      logger.info(s"Simulation running; ${result.system.name}; network actor: ${result.network.path}")
    })
  }

  import scala.concurrent.duration._

  /**
    * Provides the set-up of the environment that sends signals to the network, and then sends the signals. Recall
    * that until the actor-system is shut down, as long as signals are sent to the network, the network will respond.
    *
    * @param actorSystem  The local actor-system in which the neural network is running
    * @param remoteGroups A map holding the remote group ID and the associated [[com.digitalcipher.spiked.construction.NetworkBuilder.RemoteGroupInfo]]
    * @param network      The neural network
    * @param environmentFactory The factory for creating an environment that sends signals to the neural network.
    * @param clock        The signal clock used to determine simulation timing. Generally, the clock represents the time
    *                     from the network's point of view. Neurons have offsets to this clock to manage their own time.
    * @param inputNeuronSelector The regular expression used to select the input neurons from the network
    * @param environmentSetupWaitTime The amount of time to wait for the run-environment to get set up
    * @return A reference to the environment actor
    */
  private def runSimulation(actorSystem: ActorSystem,
                            remoteGroups: Map[String, RemoteGroupInfo],
                            network: ActorRef,
                            environmentFactory: EnvironmentFactory,
                            clock: SignalClock,
                            inputNeuronSelector: Regex,
                            environmentSetupWaitTime: FiniteDuration = 5 seconds): ActorRef = {
    import akka.pattern.ask

    implicit val timeout: Timeout = Timeout(10 seconds)
    implicit val executionContext: ExecutionContextExecutor = actorSystem.getDispatcher

    // grab the neurons to which to send the initial signals
    logger.info("sending request to retrieve neurons")
    val runEnvironmentFuture: Future[ActorRef] = ask(network, RetrieveNeurons(inputNeuronSelector)).mapTo[List[ActorRef]]
      .flatMap(inputNeurons => {
        logger.info(s"injection neurons: ${inputNeurons.sortBy(ref => ref.path.name).map(neuron => neuron.path.name)}")

        // initialize the network giving it the start time from the signal clock
        ask(network, SimulationStart(clock.startTime)).mapTo[SimulationStartResponse]
          .map(initialized => {
            logger.info("initialized: {}", initialized.success)

            // create the environment actor that will send signals to the input neurons
            logger.info(s"signal time: ${clock.startTime}; send to: ${inputNeurons.map(neuron => neuron.path.name)}")
            environmentFactory.instance(
              system = actorSystem,
              inputNeurons = inputNeurons,
              remoteGroups = remoteGroups,
              clock = clock,
              cleanup = shutdownSystemFunctionFactory(actorSystem, remoteGroups, portManager)
            )
          })
      })

    // wait for it all to get set up
    Await.result(runEnvironmentFuture, environmentSetupWaitTime)
  }
}

/**
  * Companion object to the series runner class
  */
object SeriesRunner {

  /**
    * Marker for setting up event logging
    */
  trait EventLogging

  /**
    * Class for specifying a file-based event logging. Holds the function for creating the
    * log-file name based on the series number.
    * @param logFile The function that returns the log-file name based on the series identifier
    */
  case class FileEventLogging(logFile: Int => String = series => s"test-output/neuron-activity.$series.log") extends EventLogging

  /**
    * Class for specifying a kafka-based event logger. Holds the name of the configuration item defining the
    * kafka connection information, and a function the returns the name of the kafka topic, given the series
    * number.
    * @param configItemName The configuration item name that specifies the kafka connection information
    * @param topic The function that returns the kafka-topic name based on the series number.
    */
  case class KafkaEventLogging(configItemName: String = "kafka", topic: Int => String = series => s"spikes-$series") extends EventLogging

  /**
    * Holds the result of attempting to create a spikes neural network
    * @param system The name of the actor system in which the neural network was created
    * @param network The reference to the actor representing the neural network
    * @param remoteGroups A map of group-name -> remote-group-info associations
    */
  case class CreateNetworkResult(system: ActorSystem, network: ActorRef, remoteGroups: Map[String, RemoteGroupInfo])

  /**
    * Holds the results for the attempted creation of a series of neural networks and some helper
    * functions to query the result.
    * @param results The results of creating a series of networks as a list of either failure messages or
    *                a create-network-result instance.
    */
  case class CreateNetworkResults(results: List[Either[List[String], CreateNetworkResult]]) {
    /**
      * @return `true` if one or more network-creation attempts failed; `false` if all networks were
      *        successfully created
      */
    def hasFailures: Boolean = results.count(either => either.isLeft) > 0

    /**
      * @return A list of the failure messages
      */
    def failures: List[List[String]] = results.filter(either => either.isLeft).map(either => either.left.get)

    /**
      * @return A list of the neural-network-creation results.
      */
    def successes: List[CreateNetworkResult] = results.filter(either => either.isRight).map(either => either.right.get)
  }
}
