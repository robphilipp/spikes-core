package com.digitalcipher.spiked.construction

import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import com.digitalcipher.spiked.construction.NetworkBuilder.{NetworkFromDescription, neuronTypeCount}
import com.digitalcipher.spiked.logging._
import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.neurons.Neuron
import com.digitalcipher.spiked.neurons.learning.stdp.{SpikeTimingDependentPlasticity, StdpAlphaLearning, StdpHardLimit, StdpSoftLimit}
import com.digitalcipher.spiked.topology.Network._
import com.digitalcipher.spiked.construction.NeuronCreator.{CreateNeurons, CreateNeuronsResponse, SimulationTimeFactor}
import com.digitalcipher.spiked.construction.description.{NetworkDescription, NeuronDescription}
import com.digitalcipher.spiked.topology.NeuronType.NeuronType
import com.digitalcipher.spiked.topology.coords.spatial.Coordinates
import com.digitalcipher.spiked.construction.description.{GroupDescription, _}
import com.digitalcipher.spiked.topology.{Network, NeuronType}

import scala.concurrent.Future
import scala.concurrent.duration._


/**
  * Holds the location of the neurons (maybe this isn't such a good idea; would limit the network size)
  *
  * --> This should really be a network builder, that accepts a string of construction instructions
  *
  * For example, the DNA below defines a simple network of three neurons
  * {{{(
  * GRP=[
  *   (gid=group1, hst=192.168.1.153, prt=2552)
  * ],
  * NRN=[
  *   (nid=input-11, grp=group1, nty=mi, mst=5 mV, inh=f, rfp=20 ms, mnp=0 mV, mpd=250 ms, spp=1 mV, csp=1 m/s, WDF=(fnc=exp, dhl=100 s), WLF=(fnc=bnd, lwb=0.0, upb=5.0), LOC=(cst=ct, px1=-30, px2=-30, px3=0.)),
  *   (nid=input-12, grp=group1, nty=mi, mst=5 mV, inh=f, rfp=20 ms, mnp=0 mV, mpd=250 ms, spp=1 mV, csp=1 m/s, WDF=(fnc=exp, dhl=100 s), WLF=(fnc=bnd, lwb=0.0, upb=5.0), LOC=(cst=ct, px1=-30 µm, px2=-20 µm, px3=0 µm)),
  *   (nid=input-13, grp=group1, nty=mi, mst=5 mV, inh=f, rfp=20 ms, mnp=0 mV, mpd=250 ms, spp=1 mV, csp=1 m/s, WDF=(fnc=exp, dhl=100 s), WLF=(fnc=bnd, lwb=0.0, upb=5.0), LOC=(cst=ct, px1=-30 µm, px2=-10 µm, px3=0 µm))
  * ],
  * CON=[
  *   (prn=input-11, psn=input-12, cnw=1.5, eqw=1.5, lrn=stdp, cst=sig),
  *   (prn=input-11, psn=input-13, cnw=1.5, eqw=1.5, lrn=stdp, cst=sig)
  * ],
  * LRN=[
  *   (fnc=stdp, ina=0.035, inp=3 ms, exa=0.035, exp=3 ms),
  *   (fnc=flat)
  * ],
  * CST=[
  *   (fnc=sig, dhl=10 s),
  *   (fnc=none)
  * ]
  * )}}}
  * Needs to be constructed in such a way the a genetic algorithm could modify the DNA be "chunk" rather than through the
  * use of functions that set values. Does the even make sense?
  *
  * What about persisting a network? Need network to able to describe itself....Does that mean that the neurons need to
  * hold their location? Or, something needs to hold their location...the NetworkTopology? [(neuron_id, location), ..., ... ]
  *
  * Created by rob on 9/11/16.
  */
class NetworkBuilder(val timeFactor: Int) extends Actor {

  // send messages with futures requires an execution context, and by importing this, the implicit values are supplied
  // to the method (timeout, execution context (i.e. this actor's dispatcher))
  import context.dispatcher

  /**
    * Constructs a network based on the network description and the deployment name
    *
    * @param description    The network description
    * @param deploymentName The deployment name
    * @return A [[Future]] holding the [[ActorRef]] of the network
    */
  def networkFrom(description: NetworkDescription, deploymentName: String): Future[ActorRef] = {
    // create the neuron network (holds the topology of the network)
    val network = context.system.actorOf(Props[Network], deploymentName)

    // grab the groups to create the neuron-creators (locally and/or remote) that are used to create neurons
    // each group (key) is associated with its neuron creator actor-ref (value)
    val groups: Map[String, ActorRef] = description.groups.values.map(group => neuronCreatorActor(group)).toMap

    // batch the neurons into their respective groups, and then make a call to each group's neuron creation
    // actor to create the neurons for that group
    import akka.pattern.ask
    val creators: Map[ActorRef, List[NeuronDescription]] = description.neurons.values.toList.groupBy(nd => groups(nd.groupId))

    // send a message to the neuron creator to create the neuron
    val createdFuture: Future[List[CreateNeuronsResponse]] = Future.sequence(
      creators.map({ case (actorRef, descriptions) =>
        ask(context.actorSelection(actorRef.path), CreateNeurons(descriptions))(Timeout(10 seconds)).mapTo[CreateNeuronsResponse]
      }).toList
    )

    // send the network a message for each neuron requesting that the network add that neuron
    val addedFuture: Future[List[AddNeuronsResponse]] = createdFuture.map(neuronResponses => {
      neuronResponses.map(neuronGroup => ask(network, AddNeurons(neuronGroup.neurons.toList))(Timeout(10 seconds)).mapTo[AddNeuronsResponse])
    }).flatMap(futures => Future.sequence(futures))

    // send messages to connect the neurons (and do some logging of the network' topology, learning functions, and
    // weight stickiness functions)
    val connectionsFuture: Future[ConnectNeuronsResponse] = addedFuture
      .map(neuronResponses => {
        // log the topology
        val neurons = neuronResponses.flatMap(response => response.neurons)
          .map(response => (response._1.path.name, (response._1, response._2.cartesian))).toMap
        logNetworkTopology(neurons)

        // log all the learning functions
        logLearningFunctions(description.learningFunctions.map(entry => (entry._1, Neuron.convert(entry._2))))

        // send the network a request to connect neurons
        val connections: List[NeuronConnection] = description.connections
          .map(connection => convert(
            description = connection,
            neurons = neurons.mapValues(_._1), // convert map(string -> (actor-ref, coords)) to map(string -> actor-ref)
            learning = description.learningFunctions
          ))
        // Future[ConnectNeuronsResponse]
        ask(network, ConnectNeurons(connections))(Timeout(10 seconds)).mapTo[ConnectNeuronsResponse]
      }).flatten

    // return the network wrapped in a future
    connectionsFuture.map(_ => network)
  }

  /**
    * Logs the network topology
    *
    * @param neurons The map of neuron IDs to their location
    */
  private def logNetworkTopology(neurons: Map[String, (ActorRef, Coordinates.Cartesian)]): Unit = {
    val neuronTypes: Map[NeuronType, Int] = neuronTypeCount(neurons)

    // send the network summary to the log, and, optionally, kafka
    EventLogger.log(context.system.name, () => NetworkSummary(neuronTypes))

    // send the neuron topology to the log and, optionally, kafka
    neurons.foreach(entry => EventLogger.log(context.system.name, () => NetworkTopology(entry._1, entry._2._2.point)))
  }

  /**
    * Logs the adding of the learning function
    *
    * @param learningFunctions The map of learning function names to their learning function
    */
  private def logLearningFunctions(learningFunctions: Map[String, SpikeTimingDependentPlasticity]): Unit = {
    import com.digitalcipher.spiked.construction.description.LearningFunctionDescription.{NO_LEARNING, STDP_ALPHA, STDP_HARD, STDP_SOFT}
    learningFunctions.foreach(entry => entry._1 match {
      case STDP_HARD.name =>
        val params = entry._2.asInstanceOf[StdpHardLimit]
        EventLogger.log(context.system.name, () => StdpHardLimitLearningFunction(
          params.inhibitionAmplitude, params.inhibitionPeriod, params.excitationAmplitude, params.excitationPeriod
        ))

      case STDP_SOFT.name =>
        val params = entry._2.asInstanceOf[StdpSoftLimit]
        EventLogger.log(context.system.name, () => StdpSoftLimitLearningFunction(
          params.inhibitionAmplitude, params.inhibitionPeriod, params.excitationAmplitude, params.excitationPeriod
        ))

      case STDP_ALPHA.name =>
        val params = entry._2.asInstanceOf[StdpAlphaLearning]
        EventLogger.log(context.system.name, () => StdpAlphaLearningFunction(
          params.baseline, params.timeConstant, params.learningRate
        ))

      case NO_LEARNING.name => s""
    })
  }

  /**
    * Converts the group description to a (groupId, actor-ref) pair
    *
    * @param description The group description
    * @return A pair holding the group ID to the associated actor reference used to create the neuron
    */
  private def neuronCreatorActor(description: GroupDescription): (String, ActorRef) = {
    description.params match {
      case _: LocalGroupParams =>
        // create the local neuron creator
        val neuronCreator = context.actorOf(props = Props[NeuronCreator], name = description.groupId)

        // send the simulation time-factor to the newly created neuron
        neuronCreator ! SimulationTimeFactor(timeFactor)

        // return the (creator ID, creator) pair
        (description.groupId, neuronCreator)

      case remote: RemoteGroupParams =>
        // create the remote neuron creator
        import akka.actor.{Address, Deploy}
        import akka.remote.RemoteScope
        val remoteAddress = Address(
          protocol = NetworkBuilder.AKKA_PROTOCOL,
          system = context.system.name,
          host = remote.host,
          port = remote.port
        )
        val props = Props[NeuronCreator].withDeploy(Deploy(scope = RemoteScope(remoteAddress)))
        val neuronCreator = context.actorOf(props = props, name = description.groupId)

        // send the simulation time-factor to the newly created neuron
        neuronCreator ! SimulationTimeFactor(timeFactor)

        // return the (creator ID, creator) pair
        (description.groupId, neuronCreator)
    }
  }

  /**
    * Converts the connection description into a [[NeuronConnection]] message
    *
    * @param description The connection description
    * @param neurons     The neurons
    * @param learning    The learning functions
    * @return a [[NeuronConnection]] message
    */
  private def convert(description: ConnectionDescription,
                      neurons: Map[String, ActorRef],
                      learning: Map[String, LearningFunctionDescription]): NeuronConnection = {
    NeuronConnection(
      preSynaptic = neurons(description.preSynapticNeuronId),
      postSynaptic = neurons(description.postSynapticNeuronId),
      learningFunctionDescription = learning(description.learningFunctionName),
      initialWeight = description.initialWeight,
      equilibriumWeight = description.equilibriumWeight
    )
  }

  /**
    * Callback when a message is received for this actor
    *
    * @return A receive object
    */
  override def receive: Receive = {
    case NetworkFromDescription(description, deploymentName) =>
      sender() ! networkFrom(description, deploymentName)

    //    case TestMessage() =>
    //      println("empty test message received")
    //      sender() ! TestMessageWithArg("empty message received response", 0)
    //
    //    case TestMessageWithArg(content, argument2) =>
    //      println(s"test message received: $content")
    //      sender() ! TestMessageWithArg("content message received response", argument2)
    //
    //    case SerializationTestMessage(argument) =>
    //      println(s"received serialization test message: $argument")
    //      sender() ! TestMessageWithArg("response to serialization test message", 272)

    //    case _ => println("message received")
  }
}

object NetworkBuilder {

  val AKKA_PROTOCOL = "akka.tcp" // stable version
  //  val AKKA_PROTOCOL = "akka" // for artery

  /**
    * Message class for requesting that the network builder construct the network from the network description
    * and the with the specified deployment name
    *
    * @param description    The network description
    * @param deploymentName The deployment name for the network actor
    */
  case class NetworkFromDescription(description: NetworkDescription, deploymentName: String)

  /**
    * Pattern for creating network builder with arguments
    *
    * @param timeFactor The simulation time factor. Every N seconds of real-time is timeFactor * N seconds of
    *                   simulation (i.e. if timeFactor=10 then 10 seconds of simulation is 1 second of real time).
    * @return The [[Props]] for the actor
    */
  def props(timeFactor: Int): Props = Props(new NetworkBuilder(timeFactor))

  /**
    * Counts the number of neuron of each type
    *
    * @param neurons A map holding all the neuron IDs and their associated actor-reference and location
    * @return A [[Map]] holding the the [[NeuronType]] and its associated count
    */
  private def neuronTypeCount(neurons: Map[String, (ActorRef, Coordinates.Cartesian)]): Map[NeuronType, Int] = {
    neurons.map(entry => NeuronType.of(entry._1)).foldLeft(Map.empty[NeuronType, Int]) {
      (count, neuronType) => count + (neuronType -> (count.getOrElse(neuronType, 0) + 1))
    }
  }

  /**
    * Message for creating a remote actor system
    * @param name The name of the actor system to create
    */
  case class CreateActorSystem(name: String)

  /**
    * Message for destroying a remote actor system
    * @param name The name of the actor system to destroy
    */
  case class DestroyActorSystem(name: String)
}

//case class TestMessage()
//case class TestMessageWithArg(content: String, argument2: Int)
////case class SerializationTestMessage(weightStickiness: Map[String, WeightStickinessDescription]) // works
//case class SerializationTestMessage(coordinate: Coordinates.Cartesian) // works








