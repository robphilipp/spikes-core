package com.digitalcipher.spiked.inputs.sensors

import akka.actor.{ActorRef, ActorSystem}
import com.digitalcipher.spiked.construction.NetworkBuilder.RemoteGroupInfo
import com.digitalcipher.spiked.inputs.EnvironmentFactory
import com.digitalcipher.spiked.neurons.SignalClock
import squants.Time

/**
  * Sensors for the input to the neural network. A sensor emits signals to a set of neurons in the
  * neural network based on external signals.
  */
object SensorFactory extends EnvironmentFactory {
  /**
    * Constructs an instance of the environment actor
    *
    * @param system       The actor system for which to create an environment
    * @param neurons      the list of neurons that will receive the signal from the sensor.
    * @param remoteGroups A `map(remote_group_name -> remote_group_info)` where the remote group info holds
    *                     the name of the remote actor system and the port on which it listens
    * @param clock        The neural network's signal clock
    * @param cleanup      The clean-up function, when called, should shut-down the actor system and then terminate the run
    * @return A reference to the environment actor
    */
  def instance(system: ActorSystem,
               neurons: List[ActorRef],
               remoteGroups: Map[String, RemoteGroupInfo],
               clock: SignalClock,
               cleanup: (Time, Time) => Unit
              ): ActorRef = Sensor.from(system, neurons, clock, cleanup)
}
