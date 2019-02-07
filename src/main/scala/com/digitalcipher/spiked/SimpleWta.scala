package com.digitalcipher.spiked

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.digitalcipher.spiked.logging.FileEventLogger.FileLoggingConfiguration
import com.digitalcipher.spiked.logging._
import com.digitalcipher.spiked.neurons._
import com.digitalcipher.spiked.topology.Network.{RetrieveNeurons, SimulationStart, SimulationStartResponse}
import com.digitalcipher.spiked.construction.NetworkBuilder.NetworkFromDescription
import com.digitalcipher.spiked.construction._
import com.digitalcipher.spiked.construction.description.NetworkDescription
import com.digitalcipher.spiked.construction.parsing.DnaParser
import com.typesafe.config.ConfigFactory
import squants.Time
import squants.electro.{ElectricPotential, Millivolts}
import squants.time.{Milliseconds, Seconds}

import scala.concurrent.{Await, Future}
import scala.io.Source

object SimpleWta extends App {

  // load the configuration file for the application
  val config = ConfigFactory.load()

  for (i <- 1 to 10) {
    //
    // set up the actor system
    val systemName = s"spiked_system-$i"
    println(s"starting to build the network spiked_system-$i; time: ${SignalClock.current()}")
    val system = ActorSystem(systemName, config)

    //
    // set-up logging
    //   EventLogger.withKafka(KafkaConfiguration(config.getConfig("kafka"), "spikes"))
    //   EventLogger.withLogging(LoggingConfiguration(system.eventStream))
    EventLogger.withLogging(FileLoggingConfiguration(system, s"test-output/neuron-activity.$i.log"))

    // set the simulation time factor (a time-factor of N means that 1 second of simulation time equals N seconds
    // of real time). A time-factor of 1 means the simulation is running in real-time.
    val timeFactor: Int = 1

    //
    // build the network
    val networkResult = buildNetwork(
      actorSystem = system,
      timeFactor = timeFactor,
      dnaFilename = "test-data/simple-wta.boo",
      reparseReport = true
    )

    //
    // run simulation or report errors
    networkResult match {
      case Right(network) => runSimulation(actorSystem = system, clock = initializeSimulationTimes(timeFactor),  network = network)
      case Left(errors) => println(s"Error parsing network DNA file and building network; $errors")
    }
  }

  /**
    * Called from the environment (specified in the constructor of the environment) to get a map(actor -> signal) that
    * holds the actors to which signals are sent at the current time
    *
    * @param neurons          The neurons that have been designated as input neurons
    * @param presentationTime The current time for which to send signals
    * @return A function that accepts a sequence of input neurons and a time and retuens a map of neurons that are
    *         to receive a signal and the strength of that signal
    */
  private def random(neurons: Seq[ActorRef], presentationTime: Time): (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential] = {
    var index: Int = 0
    var startTime: Time = Milliseconds(0)

    (neurons: Seq[ActorRef], time: Time) => {
      if (time - startTime > presentationTime) {
        index = (math.random() * neurons.length).toInt
        startTime = time
      }
      Map(neurons(index) -> Millivolts(1.05))
    }
  }

  /**
    * Builds the network from the specified DNA file, and constructs the network actor-reference. Before calling
    * this method, logging should already be configured so that the network-building events can be logged.
    *
    * @param actorSystem   The actor-system to which the network, builder, and all the neurons are added
    * @param dnaFilename   The DNA file describing the network and learning functions
    * @param reparseReport Whether or not to convert the network to a fragment and then reparse it, as a test.
    *                      This is mostly to ensure that the DNA file is getting parsed correctly.
    * @return Either a network actor-reference if successful, or a list of error strings if the network cannot be built
    */
  private def buildNetwork(actorSystem: ActorSystem,
                           timeFactor: Int,
                           dnaFilename: String,
                           reparseReport: Boolean = false): Either[List[String], ActorRef] = {
    // read the description file (over time this will need to be replaced by a streaming parser approach)
    val description = Source.fromFile(dnaFilename).mkString

    // attempt to parse the network. the return holds either the failure messages or the network
    // description instance. if the parsing fails, return the list of errors returned by the parser
    val parser = DnaParser(validateReference = true)
    parser.parseDna(description).map(description => {
      if (reparseReport) reportReparse(description, parser)

      // grab the run ID
      val runId: String = actorSystem.name

      // request the hidden neurons
      import scala.concurrent.duration._

      // todo for this to be a remote service, we have to grab the remote actor reference (or selection) so that
      //      we can send a request for it to build the network.
      // create the network-builder actor
      val networkBuilder = actorSystem.actorOf(NetworkBuilder.props(timeFactor = timeFactor), s"network_builder.$runId")

      import akka.pattern.ask
      // -------- debugging code
      // send test a couple of test messages and await their response
      //      var future = ask(networkBuilder, TestMessage())(Timeout(10 seconds))
      //      println("waiting for response from empty test message")
      //      println(Await.result(future, 10 seconds))
      //
      //      future = ask(networkBuilder, TestMessageWithArg("argument", 314))(Timeout(10 seconds))
      //      println("waiting for response from test message with content")
      //      println(Await.result(future, 10 seconds))
      //
      //      println(s"weight stickiness functions: ${description.weightStickinessFunctions}")
      //      println("sending serialization test message")
      //      future = ask(networkBuilder, SerializationTestMessage(description.weightStickinessFunctions))(Timeout(10 seconds))
      //      println("waiting for response from serialization test message")
      //      println(Await.result(future, 10 seconds))
      //--------

      // send message to build the network from its descriptions
      println("sending request to build network")
      val networkFuture = ask(networkBuilder, NetworkFromDescription(description, s"spikes.$runId"))(Timeout(10 seconds))
        .mapTo[Future[ActorRef]]
        .flatten

      val network: ActorRef = Await.result(networkFuture.mapTo[ActorRef], 10 seconds)
      println(networkFuture)
      println(s"network actor-ref: ${network.path.name}")
      Right(network)
    }).getOrElse(Left(List()))
  }

  /**
    * Initializes the simulation times and sets the simulation time factor.
    * //    * @param timeFactor The number of seconds in real-time that 1 second of simulation time takes. For example,
    * //    *                   a time-factor or 1 means that the simulation is running in "real time". A time-factor
    * //    *                   of 2 means that a second in simulation time takes 2 seconds in real-time. So a simulation
    * //    *                   duration of 10 seconds would take 20 seconds to run to completion.
    *
    * @return A pair where the first element (left) is the time since the beginning of the simulation in milliseconds,
    *         and the second element (right) is the actual time when the simulation started.
    */
  private def initializeSimulationTimes(timeFactor: Int): SignalClock = SignalClock(timeFactor, SignalClock.current())

  /**
    * Shows the DNA fragment created from the parsed network description, then parses that fragment again and shows
    * the fragment generated from that parse result, and they should hold the same information
    *
    * @param networkDescription The parsed network description
    */
  private def reportReparse(networkDescription: NetworkDescription, parser: DnaParser): Unit = {
    // create a dna fragment from the network description
    val dna = NetworkDescription.fragment(networkDescription)

    // test to ensure that the DNA parsed can be re-written and parsed
    println(s"DNA fragment: $dna")
    println(s"DNA frag rps: ${parser.parseDna(dna).map(description => NetworkDescription.fragment(description)).getOrElse("failed to reparse")}")
  }

  /**
    * Provides the set-up of the environment that sends signals to the network, and then sends the signals. Recall
    * that until the actor-system is shut down, as long as signals are sent to the network, the network will respond.
    *
    * @param actorSystem The actor-system in which the neural network is running
    * @param network     The neural network
    *                    //    * @param timeFactor The number of seconds in real-time that 1 second of simulation time takes. For example,
    *                    //    *                   a time-factor or 1 means that the simulation is running in "real time". A time-factor
    *                    //    *                   of 2 means that a second in simulation time takes 2 seconds in real-time. So a simulation
    *                    //    *                   duration of 10 seconds would take 20 seconds to run to completion.
    */
  //  private def runSimulation(actorSystem: ActorSystem, network: ActorRef, timeFactor: Int = 1): Unit = {
  private def runSimulation(actorSystem: ActorSystem, network: ActorRef, clock: SignalClock): Unit = {
    import akka.pattern.ask

    import scala.concurrent.duration._

    // grab the neurons to which to send the initial signals
    println("sending request to retrieve neurons")
    val retrieveNeuronsFuture = ask(network, RetrieveNeurons("""(in\-[1-7]{1}$)""".r))(Timeout(10 seconds))
    val inputNeurons = Await.result(retrieveNeuronsFuture.mapTo[List[ActorRef]], 10 seconds)
    println(s"injection neurons: ${inputNeurons.sortBy(ref => ref.path.name).map(neuron => neuron.path.name)}")

    val initializeFuture = ask(network, SimulationStart(clock.startTime))(Timeout(10 seconds))
    val initialized = Await.result(initializeFuture.mapTo[SimulationStartResponse], 10 seconds)
    println(s"initialized: ${initialized.success}")

    println(s"signal time: ${clock.startTime}; send to: ${inputNeurons.map(neuron => neuron.path.name)}")
    val duration = Seconds(50) * clock.timeFactor // real time
    val period = Milliseconds(50) * clock.timeFactor // real time
    val presentationTime = Milliseconds(25) // simulation time
    Environment.from(
      system = actorSystem,
      neurons = inputNeurons,
      clock = clock,
      initialDelay = 0 millis,
      period = period.toMilliseconds millis,
      duration = duration.toMilliseconds millis,
      signals = random(inputNeurons, presentationTime),
      cleanup = shutdownSystem(actorSystem)
    )
  }

  /**
    * Returns a function that is called by the environment (specified in the constructor of the environment) when the
    * environment has hit the specified duration, and has shut itself down. The returned function shuts down the actor
    * system and terminates the run.
    *
    * @param system The actor system
    * @return A function accepts two times: the time at which the environment started sending signals; the time at
    *         which the environment has shut down.
    */
  private def shutdownSystem(system: ActorSystem): (Time, Time) => Unit = (start: Time, end: Time) => {
    system.terminate()
    println(s"Stopped sending signals to input neurons: ${end - start}")
  }
}
