package com.digitalcipher.spiked.inputs.sensors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.digitalcipher.spiked.construction.NetworkBuilder.RemoteGroupInfo
import com.digitalcipher.spiked.inputs.sensors.Sensor.{SendSignals, Stop}
import com.digitalcipher.spiked.neurons.{Signal, SignalClock, SignalReceiver}
import squants.Time
import squants.electro.ElectricPotential
import squants.time.Time

/**
  * A sensor accepts external signals and sends the signals to the requested neurons.
  *
  * @param neurons         A list of neurons that can receive sensor signals.
  * @param clock           The simulation clock that holds the current simulation time and the time-factor
  * @param cleanupFunction The callback function that gets called when this sensor shuts down
  */
class Sensor(neurons: Seq[ActorRef], clock: SignalClock, cleanupFunction: (Time, Time) => Unit) extends Actor {

  // holds the neuron ID strings and their associated actor references so that the lookup is
  // faster. the incoming messages contain a sequence of neuron IDs and the signal to send to
  // them.
  private val neuronIds: Map[String, ActorRef] = neurons.map(neuron => neuron.path.name -> neuron).toMap

  // capture the current time from which to reference elapsed time
  protected val start: Time = clock.now()

  /**
    * calls the specified clean-up function after this actor has been stopped
    */
  override def postStop(): Unit = cleanupFunction(start, clock.now())

  /**
    * Called when messages arrive to this actor
    *
    * @return The actor receive
    */
  override def receive: Receive = {
    // send the signals
    case SendSignals(neurons, signal) =>
      neurons
        // grab the actor reference for each of the neuron IDs, dropping any invalid IDs (i.e. those
        // that aren't listed in the valid input neurons
        .map(id => neuronIds.getOrElse(id, () => Nil))
        .filter(ref => !ref.equals(Nil))
        // send the signal to each of the neurons
        .foreach {
          case neuron: ActorRef => neuron ! Signal(clock.now(), SignalReceiver.world, signal)
        }

    // stop sending signals and shut-down this environment actor
    case Stop =>
      context.stop(self)
  }
}

/**
  * Holds factory methods for creating the sensor actor, and messages.
  */
object Sensor {

  /**
    * Constructs an instance of the environment actor
    *
    * @param system       The actor system for which to create an environment
    * @param neurons      the list of neurons that will receive the signal from the sensor.
    * @param clock        The neural network's signal clock
    * @param cleanup      The clean-up function, when called, should shut-down the actor system and then terminate the run
    * @return A reference to the environment actor
    */
  def from(system: ActorSystem, neurons: List[ActorRef], clock: SignalClock, cleanup: (Time, Time) => Unit): ActorRef =
    system.actorOf(Props(new Sensor(neurons = neurons, clock = clock, cleanupFunction = cleanup)))

  /**
    * Message to send a signal to the input neurons
    */
  case class SendSignals(neuronIds: Seq[String], signal: ElectricPotential)

  /**
    * Message to stop sending signals to the input neurons and shut down the environment actor
    */
  case object Stop

}

