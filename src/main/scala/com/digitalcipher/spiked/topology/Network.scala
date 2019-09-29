package com.digitalcipher.spiked.topology

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import com.digitalcipher.spiked.logging.EventLogger
import com.digitalcipher.spiked.logging.messages.NetworkConnected
import com.digitalcipher.spiked.neurons.Neuron.{Connect, InitializeTime}
import com.digitalcipher.spiked.topology.Network._
import com.digitalcipher.spiked.topology.NeuronType.NeuronType
import com.digitalcipher.spiked.topology.coords.spatial.Coordinates
import com.digitalcipher.spiked.construction.description.{LearningFunctionDescription, LocationDescription}

import scala.collection.mutable
import scala.util.matching.Regex

// todo network should hold an actor-reference to the groups, and the groups should be what communicates for the
//      the individual neurons. That way, when sending a message to the network that is intended to be broadcast
//      to all the neurons, one message is sent to each group, and the group (which is local to the neurons in that
//      group) forwards, and possibly filters or enriches, the message to each neuron.
/**
  * Represents the network of neurons. Neurons must first be added to the network by sending a message to the network:
  * <ol>
  * <li>[[com.digitalcipher.spiked.topology.Network.AddInputNeuron]] to add an input/sensory neuron</li>
  * <li>[[com.digitalcipher.spiked.topology.Network.AddHiddenNeuron]] to add an hidden/computation neuron</li>
  * <li>[[com.digitalcipher.spiked.topology.Network.AddOutputNeuron]] to add an output/motor neuron</li>
  * </ol>
  *
  * Once a neurons are added to the network, they can be connected by sending a message to the network:
  * <ol>
  * <li>[[com.digitalcipher.spiked.topology.Network.NeuronConnection]] to connect two neurons</li>
  * </ol>
  *
  * Created by rob on 9/23/16.
  */
class Network extends Actor {

  val addNeuronLogger = Logging(context.system.eventStream, AddNeuron.getClass.getName)
  val connectNeuronsLogger = Logging(context.system.eventStream, NeuronConnection.getClass.getName)

  // map holding the neuron and its associated location and type
  val neurons: mutable.Map[ActorRef, (Coordinates.Cartesian, NeuronType)] = new mutable.HashMap[ActorRef, (Coordinates.Cartesian, NeuronType)]

  /**
    *
    * @param neurons A map holding the actor reference to the neuron as the key, and the location description as the value
    * @return The response that the neuron was added
    */
  def addNeurons(neurons: List[(ActorRef, LocationDescription)]): AddNeuronsResponse = {
    neurons.foreach(entry => {
      val neuron = entry._1
      val location = entry._2
      addByType(neuron, location.cartesian, NeuronType.of(neuron.path.name))
    })
    AddNeuronsResponse(neurons)
  }

  /**
    * Adds a neuron with it's associated location and type (i.e. input, hidden, output)
    *
    * @param neuron     The neuron to add to the network
    * @param location   The location in the network's coordinate system
    * @param neuronType The type of neuron (i.e. input, hidden, output)
    */
  def addByType(neuron: ActorRef, location: Coordinates.Cartesian, neuronType: NeuronType): Unit =
    neurons(neuron) = (location, neuronType)

  /**
    * Connects the neurons in the list of connections
    * @param connections The list of connections to make
    * @return A [[Network.ConnectNeuronsResponse]]
    */
  def connectNeurons(connections: List[NeuronConnection]): ConnectNeuronsResponse = {
    connections.foreach(connection =>
      connect(
        preSynaptic = connection.preSynaptic,
        postSynaptic = connection.postSynaptic,
        initialWeight = connection.initialWeight,
        equilibriumWeight = connection.equilibriumWeight,
        learningFunctionDescription = connection.learningFunctionDescription
      )
    )
    ConnectNeuronsResponse(true)
  }

  /**
    * Connects the pre-synaptic neuron to the post-synaptic neuron
    *
    * @param preSynaptic   The pre-synaptic neuron
    * @param postSynaptic  The post-synaptic neuron
    * @param initialWeight The initial synapse weight
    * @param equilibriumWeight The equilibrium weight to which the weight decays
    * @param learningFunctionDescription The description of the learning functions available to the neurons
    */
  def connect(preSynaptic: ActorRef,
              postSynaptic: ActorRef,
              initialWeight: Double,
              equilibriumWeight: Double,
              learningFunctionDescription: LearningFunctionDescription): Unit = {

    val preSynapticLocation = neurons(preSynaptic)._1
    val postSynapticLocation = neurons(postSynaptic)._1
    val distance = (preSynapticLocation - postSynapticLocation).norm

    // todo need to do this with futures as well
    // send the pre-synaptic neuron a message to connect to the specified post-synaptic neuron
    preSynaptic ! Connect(
      postSynaptic = postSynaptic,
      weight = initialWeight,
      equilibriumWeight = equilibriumWeight,
      distance = distance,
      learning = learningFunctionDescription
    )

    // log the connection
    EventLogger.log(context.system.name, () => NetworkConnected(
      preSynapticId = preSynaptic.path.name,
      postSynapticId = postSynaptic.path.name,
      initialWeight = initialWeight,
      equilibriumWeight = equilibriumWeight,
      preSynapticLocation = preSynapticLocation.point,
      postSynapticLocation = postSynapticLocation.point,
      distance = distance
    ))
  }

  private def neuronsOfType(neuronType: NeuronType): Seq[ActorRef] = {
    neurons.filter(entry => entry._2._2 == neuronType).keys.toSeq
  }

  private def neuronsOfPattern(pattern: Regex): Seq[ActorRef] = {
    neurons.keys.filter(ref => pattern.findFirstIn(ref.path.name).isDefined).toSeq
  }

  private def setInitialTime(time: Long): SimulationStartResponse = {
    // todo use futures
    neurons.keys.foreach(neuron => neuron ! InitializeTime(time))
    SimulationStartResponse(true)
  }

  /**
    * Processes messages received from akka dispatcher
    */
  override def receive: Receive = {
    case AddNeurons(neuronList) =>
      sender() ! addNeurons(neuronList)

    case ConnectNeurons(connections) =>
      sender() ! connectNeurons(connections)

      // send LocationDescription instead of locations
    case AddInputNeuron(preSynaptic, location) =>
      addByType(preSynaptic, location.cartesian, NeuronType.Input)

    case AddHiddenNeuron(preSynaptic, location) => addByType(preSynaptic, location.cartesian, NeuronType.Hidden)
    case AddOutputNeuron(preSynaptic, location) => addByType(preSynaptic, location.cartesian, NeuronType.Output)
    case NeuronConnection(preSynaptic, postSynaptic, initialWeight, equilibriumWeight, learning) =>
      connect(preSynaptic, postSynaptic, initialWeight, equilibriumWeight, learning)

    // retrieve the neuron references
    case RetrieveNeurons(pattern) =>
      sender() ! neuronsOfPattern(pattern).toList

    case RetrieveInputNeurons => sender() ! neuronsOfType(NeuronType.Input)
    case RetrieveHiddenNeurons => sender() ! neuronsOfType(NeuronType.Hidden)
    case RetrieveOutputNeurons => sender() ! neuronsOfType(NeuronType.Output)
    case RetrieveNeuronDetails => sender() ! neurons.toMap

    // retrieve a neuron's location
    case Location(neuron) => sender() ! neurons(neuron)._1

    // retrieve neuron's type
    case NeuronOfType(neuron) => sender() ! neurons(neuron)._2

    // sets the initial time on all the neurons
    case SimulationStart(time) =>
      sender() ! setInitialTime(time)

    //case Config() => returns all the current settings (neuron ID, connections/weights, threshold, spike potential, decay, refractory period, ... )
    //case Stats() => returns all the current settings (neuron ID, connections/weights, membrane potential, last spike time, ... )

//    case TestMessage() =>
//      println("empty test message received")
//      sender() ! TestMessageWithArg("empty message received response", 0)
//
//    case TestMessageWithArg(content, argument2) =>
//      println(s"test message received: $content")
//      sender() ! TestMessageWithArg("content message received response", argument2)
//
//    case SerializationTestMessage(argument) => println(s"received serialization test message: $argument")
  }
}

object Network {
  /**
    * Groups the neurons by type
    * @param neurons A map holding the neurons actor-reference to its associated (location, type) pair
    * @return A map of neuron type (i.e. hidden, input, output, etc) to a map(actor-ref -> location)
    */
  def groupByNeuronType(neurons: Map[ActorRef, (Coordinates.Cartesian, NeuronType)]): Map[NeuronType, Map[ActorRef, Coordinates.Cartesian]] = {
    // convert the map so that that neurons are grouped by neuron type
    neurons
      .groupBy(entry => entry._2._2)
      .map(entry => (entry._1, entry._2.map(ent => (ent._1, ent._2._1))))
  }

  /**
    * Message class for setting the simulation start time
    * @param initialTime The initial time for all the neurons in the network
    */
  case class SimulationStart(initialTime: Long)

  /**
    * Response to the request to set the simulation start time
    * @param success `true` if the request was processed
    */
  case class SimulationStartResponse(success: Boolean)

  /**
    * Message class for adding neurons to the network
    * @param neurons The list of neurons represented by (actor_ref, location) pairs
    */
  case class AddNeurons(neurons: List[(ActorRef, LocationDescription)])

  /**
    * Message response class for adding neurons
    * @param neurons The list of neurons represented by (actor_ref, location) pairs
    */
  case class AddNeuronsResponse(neurons: List[(ActorRef, LocationDescription)])

  /**
    * Case class representing the message to add a neuron to the network
    * @param neuron The neuron to add
    * @param location The location of the neuron
    */
  case class AddNeuron(neuron: ActorRef, location: LocationDescription)

  /**
    * Case class representing the message to add an input/sensory neuron to the network
    * @param neuron The input neuron to add
    * @param location The location of the neuron
    */
  case class AddInputNeuron(neuron: ActorRef, location: LocationDescription)

  /**
    * Case class representing the message to add a hidden/computational neuron to the network
    * @param neuron The hidden neuron to add
    * @param location The location of the neuron
    */
  case class AddHiddenNeuron(neuron: ActorRef, location: LocationDescription)

  /**
    * Case class representing the message to add an output/motor neuron to the network
    * @param neuron The motor neuron to add
    * @param location The location of the neuron
    */
  case class AddOutputNeuron(neuron: ActorRef, location: LocationDescription)

  /**
    * Case class representing the message to connect two neurons in the network
    * @param preSynaptic The pre-synaptic neuron
    * @param postSynaptic The post-synaptic neuron
    * @param initialWeight The initial weight
    * @param equilibriumWeight The equilibrium weight to which the weight decays
    * @param learningFunctionDescription The description of the learning function
    */
  case class NeuronConnection(preSynaptic: ActorRef,
                              postSynaptic: ActorRef,
                              initialWeight: Double,
                              equilibriumWeight: Double,
                              learningFunctionDescription: LearningFunctionDescription)

  /**
    * Message class for connecting neurons in the specified list
    * @param connections A list of neuron connections to make
    */
  case class ConnectNeurons(connections: List[NeuronConnection])

  /**
    * Message response class for connecting neurons
    * @param success `true` if connections were succesful; `false` otherwise
    */
  case class ConnectNeuronsResponse(success: Boolean)

  /**
    * Message class for retrieving neurons from the network whose neuron ID matches the regular expression
    * @param pattern The regular expression used to select the neurons to return
    */
  case class RetrieveNeurons(pattern: Regex)

  /**
    * Message class for retrieving all the neurons marked as input neurons
    */
  case class RetrieveInputNeurons()

  /**
    * Message class for retrieving all the neurons marked as hidden neurons
    */
  case class RetrieveHiddenNeurons()

  /**
    * Message class for retrieving all the neurons marked as output neurons
    */
  case class RetrieveOutputNeurons()

  /**
    * Message class for retrieving neurons details from all the neurons.
    */
  case class RetrieveNeuronDetails()

  /**
    * Message class for retrieving the location of the neuron (within the network) with the specified actor-ref
    * @param neuron The actor-ref of the neuron
    */
  case class Location(neuron: ActorRef)

  /**
    * Message class for retrieving the type of neuron (input, hidden, output) that is associated with the specified actor-ref
    * @param neuron The actor-ref of the neuron
    */
  case class NeuronOfType(neuron: ActorRef)

}


