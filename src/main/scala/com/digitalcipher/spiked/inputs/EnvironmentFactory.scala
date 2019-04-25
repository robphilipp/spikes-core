package com.digitalcipher.spiked.inputs

import akka.actor.{ActorRef, ActorSystem}
import com.digitalcipher.spiked.construction.NetworkBuilder.RemoteGroupInfo
import com.digitalcipher.spiked.neurons.SignalClock
import squants.Time

/**
  * Defines the basic contract that an environment factory must obey in order to be used by the series runner.
  */
trait EnvironmentFactory {
  /**
    * Constructs an instance of the environment actor
    *
    * @param system       The actor system for which to create an environment
    * @param inputNeurons the list of input neurons (these are merely neurons that will receive signals
    *                     from the environment).
    * @param remoteGroups A `map(remote_group_name -> remote_group_info)` where the remote group info holds
    *                     the name of the remote actor system and the port on which it listens
    * @param clock        The neural network's signal clock
    * @param cleanup      The clean-up function, when called, should shut-down the actor system and then terminate the run
    * @return A reference to the environment actor
    */
  def instance(system: ActorSystem,
               inputNeurons: List[ActorRef],
               remoteGroups: Map[String, RemoteGroupInfo],
               clock: SignalClock,
               cleanup: (Time, Time) => Unit): ActorRef
}
