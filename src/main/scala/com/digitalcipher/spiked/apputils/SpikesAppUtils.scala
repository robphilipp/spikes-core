package com.digitalcipher.spiked.apputils

import java.io.IOException

import akka.actor.{ActorRef, ActorSystem, Address}
import akka.util.Timeout
import com.digitalcipher.spiked.construction.NetworkBuilder
import com.digitalcipher.spiked.construction.NetworkBuilder.{CreateActorSystem, DestroyActorSystem, NetworkFromDescription, RemoteGroupInfo}
import com.digitalcipher.spiked.construction.description.{GroupDescription, NetworkDescription, RemoteGroupParams}
import com.digitalcipher.spiked.construction.parsing.DnaParser
import com.digitalcipher.spiked.neurons.SignalClock
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory
import squants.Time

import scala.concurrent.{Await, Future}
import scala.io.{BufferedSource, Source}

/**
  * Utilities for constructing the network, setting the initial time, creating the remote actor systems, and
  * shutting down an actor system. These utilities are intended for use in building applications using the
  * spikes neural network framework.
  */
object SpikesAppUtils {

  /**
    * Loads the specified configuration file for the application, with overrides coming from the spikes-core
    * library's resources.
    *
    * @param configFilename The name of the application's configuration file
    * @return The [[com.typesafe.config.Config]] based on the specified config file name and the base config
    */
  def loadConfigFrom(configFilename: String = "application.conf"): Config =
    ConfigFactory.parseResources(configFilename).withFallback(ConfigFactory.load()).resolve()

  /**
    * Builds the network from the specified DNA file, and constructs the network actor-reference. Before calling
    * this method, logging should already be configured so that the network-building events can be logged.
    *
    * @param actorSystem   The actor-system to which the network, builder, and all the neurons are added
    * @param timeFactor    The simulation time factor. Each second of simulation time takes 'time-factor' seconds in
    *                      real time. For example, if time-factor is 10, then 1 second of simulated time takes
    *                      10 seconds in real time.
    * @param dnaFilename   The DNA file describing the network and learning functions
    * @param reparseReport Whether or not to convert the network to a fragment and then reparse it, as a test.
    *                      This is mostly to ensure that the DNA file is getting parsed correctly.
    * @return Either a (network actor-reference, remote groups) pair if successful, or a list of error strings if
    *         the network cannot be built. When the network is successfully built, the pair holds the actor-reference
    *         to the network actor, and a map(group_id -> remote_group_info)
    */
  def buildNetwork(actorSystem: ActorSystem,
                   timeFactor: Int,
                   dnaFilename: String,
                   reparseReport: Boolean = false): Either[List[String], (ActorRef, Map[String, RemoteGroupInfo])] = {
    val logger = LoggerFactory.getLogger("spikes-core-build-network")
    var source = None: Option[BufferedSource]
    try {
      source = Some(Source.fromFile(dnaFilename))
      // read the description file (over time this will need to be replaced by a streaming parser approach)
      source
        .map(src => src.mkString)
        .map(description => {
          // attempt to parse the network. the return holds either the failure messages or the network
          // description instance. if the parsing fails, return the list of errors returned by the parser
          val parser = DnaParser(validateReference = true)
          parser
            .parseDna(description)
            .map(description => {
              if (reparseReport) reportReparse(description, parser)

              // grab the run ID
              val runId = actorSystem.name

              // create a remote system for each remote group on the node specified in the
              // network description (.boo) file, and return a map that has the group ID and
              // the associated port for the new actor system on that node.
              val remoteGroups: Map[String, RemoteGroupInfo] = createActorSystemsForRemoteGroups(
                name = runId, groups = description.groups.values, actorSystem = actorSystem
              )

              // create the network-builder actor, which runs locally, calling the (possibly remote) neuron creator
              val networkBuilder = actorSystem.actorOf(
                NetworkBuilder.props(timeFactor = timeFactor, groupActorSystems = remoteGroups),
                s"network_builder.$runId"
              )

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
              import scala.concurrent.duration._
              logger.debug("sending request to build network")
              val networkFuture = ask(networkBuilder, NetworkFromDescription(description, s"spikes.$runId"))(Timeout(10 seconds))
                .mapTo[Future[ActorRef]]
                .flatten

              val network: ActorRef = Await.result(networkFuture.mapTo[ActorRef], 10 seconds)
              logger.debug("network future: {}", networkFuture)
              logger.info("created spikes neural network actor; actor name: {}", network.path.name)
              Right((network, remoteGroups))
            })
            .getOrElse(Left(List(s"Unable to parse the DNA file; filename: $dnaFilename")))
        })
        .getOrElse(Left(List()))
    }
    catch {
      case e: IOException => Left(List(s"Unable to open DNA file; filename: $dnaFilename"))
    }
    finally {
      source.foreach(src => src.close())
    }
  }

  /**
    * Creates the remote actor systems for each of the remote groups.
    *
    * @param name        The name of the remote actor system to create
    * @param groups      The group sequences from the network description (remote and local groups)
    * @param actorSystem The actor system in which this actor is running
    * @return
    */
  def createActorSystemsForRemoteGroups(name: String,
                                        groups: Iterable[GroupDescription],
                                        actorSystem: ActorSystem): Map[String, RemoteGroupInfo] = {
    val logger = LoggerFactory.getLogger("spikes-core-create-actor-system")

    import scala.concurrent.duration._
    groups
      // only need to deal with remote groups here
      .filter(group => group.params.isInstanceOf[RemoteGroupParams])
      .map(group => {
        val remoteGroup = group.params.asInstanceOf[RemoteGroupParams]
        val remoteAddress: Address = Address(
          protocol = NetworkBuilder.AKKA_PROTOCOL,
          system = "neuron_group",
          host = remoteGroup.host,
          port = remoteGroup.port
        )

        val actorPath = s"${remoteAddress.toString}/user/system_manager"
        logger.debug("requesting creation of remote actor system; actor-system name: {}", actorPath)
        val selection = actorSystem.actorSelection(actorPath)
        import akka.pattern.ask
        val actorRef = Await.result(selection.resolveOne(10 seconds), 10 seconds)
        val createFuture = ask(actorRef, CreateActorSystem(name))(Timeout(10 seconds))
        val port = Await.result(createFuture.mapTo[Int], 10 seconds)
        logger.info("created remote actor system; actor-system name: {}; bound port: {}", actorPath, port)
        group.groupId -> RemoteGroupInfo(actorRef, port)
      })
      .toMap
  }

  /**
    * Initializes the simulation times and sets the simulation time factor.
    *
    * @param timeFactor The number of seconds in real-time that 1 second of simulation time takes. For example,
    *                   a time-factor or 1 means that the simulation is running in "real time". A time-factor
    *                   of 2 means that a second in simulation time takes 2 seconds in real-time. So a simulation
    *                   duration of 10 seconds would take 20 seconds to run to completion.
    * @return A pair where the first element (left) is the time since the beginning of the simulation in milliseconds,
    *         and the second element (right) is the actual time when the simulation started.
    */
  def initializeSimulationTimes(timeFactor: Int): SignalClock = SignalClock(timeFactor, SignalClock.current())

  /**
    * Shows the DNA fragment created from the parsed network description, then parses that fragment again and shows
    * the fragment generated from that parse result, and they should hold the same information
    *
    * @param networkDescription The parsed network description
    */
  def reportReparse(networkDescription: NetworkDescription, parser: DnaParser): Unit = {
    // create a dna fragment from the network description
    val dna = NetworkDescription.fragment(networkDescription)

    // test to ensure that the DNA parsed can be re-written and parsed
    val logger = LoggerFactory.getLogger("spikes-core-reparse-report")
    logger.debug("DNA fragment from network description;\n----------\n{}\n----------", dna)
    logger.debug(
      "DNA fragment after parsing and generating a new network description;\n----------\n{}\n----------",
      parser.parseDna(dna).map(description => NetworkDescription.fragment(description)).getOrElse("failed to reparse")
    )
  }

  // todo need to add a jvm shut-down hook so that when the jvm shuts down the system can notify the
  //      the remote actors and let them know to shut down as well. What about 'kill -9' type shutdowns?
  //      What about restarts? Need to think through the strategy...
  /**
    * Returns a function that is called by the environment (specified in the constructor of the environment) when the
    * environment has hit the specified duration, and has shut itself down. The returned function shuts down the actor
    * system and terminates the run.
    *
    * @param system The local actor system
    * @return A function accepts two times: the time at which the environment started sending signals; the time at
    *         which the environment has shut down.
    */
  def shutdownSystemFunctionFactory(system: ActorSystem,
                                    remoteGroups: Map[String, RemoteGroupInfo],
                                    remotingPortManager: RemotingPortManager): (Time, Time) => Unit = (start: Time, end: Time) => {
    system.terminate()
    val logger = LoggerFactory.getLogger("spikes-core-system-shutdown")
    logger.info("Stopped sending signals to input neuron; actor-system name: {}; elapsed time: {}", system.name, (end - start).millis)

    remotingPortManager.returnRemotingPortAnd(
      system.name,
      () => remoteGroups.values.foreach(remoteInfo => remoteInfo.systemManager ! DestroyActorSystem(system.name))
    )
  }
}
