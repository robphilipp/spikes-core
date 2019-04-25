package com.digitalcipher.spiked.inputs

import akka.actor.{ActorRef, ActorSystem}
import com.digitalcipher.spiked.construction.NetworkBuilder.RemoteGroupInfo
import com.digitalcipher.spiked.neurons.SignalClock
import squants.Time
import squants.electro.ElectricPotential
import squants.time.Milliseconds

/**
  * Factory for creating a basic environment.
  *
  * @param initialDelay The time between the simulation start and the first signal
  * @param signalPeriod The period between calls to the signals function (i.e. 50 ms)
  * @param simulationDuration The duration of the simulation
  * @param signalsFunction The function that accepts a sequence of neuron actor references and a time and returns
  *                        a map that holds the neuron actor references to their associated signal. Note that there
  *                        is no requirement that all specified neurons receive a signal.
  */
case class PeriodicEnvironmentFactory(initialDelay: Time = Milliseconds(0),
                                      signalPeriod: Time,
                                      simulationDuration: Time,
                                      signalsFunction: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential]) extends EnvironmentFactory {

  import scala.concurrent.duration._

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
  override def instance(system: ActorSystem,
                        inputNeurons: List[ActorRef],
                        remoteGroups: Map[String, RemoteGroupInfo],
                        clock: SignalClock,
                        cleanup: (Time, Time) => Unit): ActorRef =
    BasicEnvironment.from(
      system = system,
      neurons = inputNeurons,
      clock = clock,
      initialDelay = (initialDelay * clock.timeFactor).toMilliseconds millis,
      period = (signalPeriod * clock.timeFactor).toMilliseconds millis, // period between calls to the signals function
      duration = (simulationDuration * clock.timeFactor).toMilliseconds millis, // duration of the simulation
      signals = signalsFunction, // function that generates a map(actor-ref -> signal)
      cleanup = cleanup
    )
}