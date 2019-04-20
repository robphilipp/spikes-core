package com.digitalcipher.spiked.inputs

import akka.actor.{Actor, ActorRef}
import com.digitalcipher.spiked.inputs.Environment.{SendSignal, Stop}
import com.digitalcipher.spiked.neurons.{Signal, SignalClock, SignalReceiver}
import squants.electro.ElectricPotential
import squants.time.Time

import scala.concurrent.duration.FiniteDuration

/**
  * Represents the environment that provides stimulus to the input neurons in ```simulation``` time.
  *
  * This environment class is a basic implementation that accepts a sequence of input neurons to which
  * it periodically sends signals through a provided signal function. At each period, calls the provided
  * signal function, handing it the input neurons and the current time. The signal function returns a
  * `map(neuron → signal)`, and for each neuron in the map, this class sends the specified signal.
  *
  * This class sends signals after an initial delay, periodically, for the specified duration. At the end of
  * the duration, calls the specified clean-up function.
  *
  * This basic environment actor can be used to to digitize signals by treating the `period` as the digitization
  * sampling period. Note however, that there is a lower limit of a few to 10 ms to that `period` and that there
  * will be noise associated with the akka schedule that schedules the repeated calls.
  *
  * @param neurons      The neurons to which to send the signal
  * @param clock        The signal clock for the simulation that holds the current simulation time and time-factor
  *                     that defines how many seconds in real-time are 1 second in simulation time.
  * @param initialDelay The initial delay before sending a signal (in real time)
  * @param period       The delay between periodic signals (in real time)
  * @param duration     The duration for which to send signals (in real time)
  * @constructor
  */
abstract class Environment(neurons: Seq[ActorRef],
                           clock: SignalClock,
                           initialDelay: FiniteDuration,
                           period: FiniteDuration,
                           duration: FiniteDuration) extends Actor {

  import context.dispatcher

  // set up the scheduler to send a signal after the initial delay, for the specified period. hold on
  // to a reference to the scheduler so that it can be cancelled at the end of the specified duration
  private val sendSignalSchedule = context.system.scheduler.schedule(
    initialDelay = initialDelay * clock.timeFactor,
    interval = period * clock.timeFactor,
    receiver = self,
    message = SendSignal
  )

  // capture the current time from which to reference elapsed time
  protected val start: Time = clock.now()

  // set up the scheduler to send the message to stop sending signals to the input neurons
  context.system.scheduler.scheduleOnce(
    delay = (initialDelay + duration) * clock.timeFactor,
    receiver = self,
    message = Stop
  )

  /**
    * Based on the input neurons, determines whether to send signal to a neuron, and if so, the strength
    * of the signal to send.
    * @param neurons The ``input`` neurons
    * @param time The current time, referenced to the beginning of the simulation
    * @return A `map(neuron-actor-ref -> signal)` that holds the neurons to which to send the signal
    *         and the signal strength
    */
  def signals(neurons: Seq[ActorRef], time: Time): Map[ActorRef, ElectricPotential]

  /**
    * Called when the environment actor has been stopped
    * @param start The time the environment was created
    * @param now The current time
    */
  def cleanup(start: Time, now: Time): Unit

  /**
    * calls the specified clean-up function after this actor has been stopped
    */
  override def postStop(): Unit = cleanup(start, clock.now())

  /**
    * Called when messages arrive to this actor
    *
    * @return The actor receive
    */
  override def receive: Receive = {
    // send the signals
    case SendSignal =>
      signals(neurons, clock.now() - start).foreach {
        case (neuron: ActorRef, signal: ElectricPotential) => neuron ! Signal(clock.now(), SignalReceiver.world, signal)
      }

    // stop sending signals and shut-down this environment actor
    case Stop =>
      sendSignalSchedule.cancel()
      context.stop(self)
  }
}

/**
  * Holds factory methods for creating the environment actor, and messages.
  */
object Environment {

  /**
    * Message to send a signal to the input neurons
    */
  case object SendSignal

  /**
    * Message to stop sending signals to the input neurons and shut down the environment actor
    */
  case object Stop

}


