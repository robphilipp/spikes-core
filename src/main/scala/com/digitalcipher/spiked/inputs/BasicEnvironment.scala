package com.digitalcipher.spiked.inputs

import akka.actor.{ActorRef, ActorSystem, Props}
import com.digitalcipher.spiked.neurons.SignalClock
import squants.Time
import squants.electro.ElectricPotential

import scala.concurrent.duration.FiniteDuration

/**
  * Represents the environment that provides stimulus to the input neurons.
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
  * @param neurons         The neurons to which to send the signal
  * @param initialDelay    The initial delay before sending a signal (in real time)
  * @param period          The delay between periodic signals (in real time)
  * @param duration        The duration for which to send signals (in real time)
  * @param signalsFunction The signal function that accepts a neuron and returns the strength of the signal to send
  *                        to that neuron (function (neuron, time) => signal, where the time is in simulation time)
  * @param cleanupFunction A function that is called after the environment actor is stopped
  * @constructor
  */
class BasicEnvironment(neurons: Seq[ActorRef],
                       clock: SignalClock,
                       initialDelay: FiniteDuration,
                       period: FiniteDuration,
                       duration: FiniteDuration,
                       signalsFunction: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential],
                       cleanupFunction: (Time, Time) => Unit)
  extends Environment(
    neurons = neurons,
    clock = clock,
    initialDelay = initialDelay,
    period = period,
    duration = duration
  ) {

  /**
    * Determines the signal to send to the neurons based on the signals-function specified in the constructor
    *
    * @param neurons The ``input`` neurons
    * @param time    The current time, referenced to the beginning of the simulation
    * @return A `map(neuron-actor-ref -> signal)` that holds the neurons to which to send the signal
    *         and the signal strength
    */
  override def signals(neurons: Seq[ActorRef], time: Time): Map[ActorRef, ElectricPotential] = signalsFunction(neurons, time)

  /**
    * Calls the cleanup-function specified in the constructor
    *
    * @param start The time the environment was created
    * @param now   The current time
    */
  override def cleanup(start: Time, now: Time): Unit = cleanupFunction(start, now)
}

/**
  * Holds factory methods for creating the environment actor, and messages.
  */
object BasicEnvironment {

  /**
    * Creates the environment or sending a signals after the specified delay, periodically, for the specified
    * duration.
    *
    * @param system       The actor system to which this environment actor will belong
    * @param neurons      The neurons to which to send the signal
    * @param clock        The signal clock used be the network for timing events
    * @param initialDelay The initial delay before sending a signal
    * @param period       The delay between periodic signals (in real time)
    * @param duration     The duration for which to send signals (in real time)
    * @param signals      The signal function that accepts a neuron and returns the strength of the signal to send
    *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
    * @param cleanup      An optional function that is called after the environment actor is stopped. If not specified,
    *                     then does nothing.
    * @return A reference to the environment actor
    */
  def from(system: ActorSystem,
           neurons: Seq[ActorRef],
           clock: SignalClock,
           initialDelay: FiniteDuration,
           period: FiniteDuration,
           duration: FiniteDuration,
           signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential] = (_, _) => Map(),
           cleanup: (Time, Time) => Unit = (_, _) => Unit): ActorRef = {
    system.actorOf(props(
      neurons = neurons,
      clock = clock,
      initialDelay = initialDelay,
      period = period,
      duration = duration,
      signals = signals,
      cleanup = cleanup
    ))
  }

  /**
    * Creates the actor properties for the environment actor
    *
    * @param neurons      The neurons to which to send the signal
    * @param clock        The signal clock used be the network for timing events
    * @param initialDelay The initial delay before sending a signal
    * @param period       The delay between periodic signals (in real time)
    * @param duration     The duration for which to send signals (in real time)
    * @param signals      The signal function that accepts a neuron and returns the strength of the signal to send
    *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
    * @param cleanup      An optional function that is called after the environment actor is stopped. If not specified,
    *                     then does nothing.
    * @return A set of actor properties from which to create a the environment actor
    */
  def props(neurons: Seq[ActorRef],
            clock: SignalClock,
            initialDelay: FiniteDuration,
            period: FiniteDuration,
            duration: FiniteDuration,
            signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential],
            cleanup: (Time, Time) => Unit): Props =
    Props(new BasicEnvironment(
      neurons = neurons,
      clock = clock,
      initialDelay = initialDelay,
      period = period,
      duration = duration,
      signalsFunction = signals,
      cleanupFunction = cleanup
    ))
}

//package com.digitalcipher.spiked.inputs
//
//import akka.actor.{Actor, ActorRef, ActorSystem, Props}
//import com.digitalcipher.spiked.inputs.BasicEnvironment.{SendSignal, Stop}
//import com.digitalcipher.spiked.neurons.{Signal, SignalClock, SignalReceiver}
//import squants.Time
//import squants.electro.ElectricPotential
//
//import scala.concurrent.duration.FiniteDuration
//
///**
//  * Represents the environment that provides stimulus to the input neurons.
//  *
//  * This environment class is a basic implementation that accepts a sequence of input neurons to which
//  * it periodically sends signals through a provided signal function. At each period, calls the provided
//  * signal function, handing it the input neurons and the current time. The signal function returns a
//  * `map(neuron → signal)`, and for each neuron in the map, this class sends the specified signal.
//  *
//  * This class sends signals after an initial delay, periodically, for the specified duration. At the end of
//  * the duration, calls the specified clean-up function.
//  *
//  * This basic environment actor can be used to to digitize signals by treating the `period` as the digitization
//  * sampling period. Note however, that there is a lower limit of a few to 10 ms to that `period` and that there
//  * will be noise associated with the akka schedule that schedules the repeated calls.
//  *
//  * @param neurons      The neurons to which to send the signal
//  * @param initialDelay The initial delay before sending a signal (in real time)
//  * @param period       The delay between periodic signals (in real time)
//  * @param duration     The duration for which to send signals (in real time)
//  * @param signals      The signal function that accepts a neuron and returns the strength of the signal to send
//  *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
//  * @param cleanup      A function that is called after the environment actor is stopped
//  * @constructor
//  */
//class BasicEnvironment(neurons: Seq[ActorRef],
//                       clock: SignalClock,
//                       initialDelay: FiniteDuration,
//                       period: FiniteDuration,
//                       duration: FiniteDuration,
//                       signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential],
//                       cleanup: (Time, Time) => Unit) extends Actor {
//
//  import context.dispatcher
//
//  // set up the scheduler to send a signal after the initial delay, for the specified period. hold on
//  // to a reference to the scheduler so that it can be cancelled at the end of the specified duration
//  private val sendSignalSchedule = context.system.scheduler.schedule(initialDelay, period, self, SendSignal)
//
//  // capture the current time from which to reference elapsed time
//  private val start = clock.now()
//
//  // set up the scheduler to stop sending signals to the input neurons
//  context.system.scheduler.scheduleOnce(initialDelay + duration, self, Stop)
//
//  /**
//    * calls the specified clean-up function after this actor has been stopped
//    */
//  override def postStop(): Unit = cleanup(start, clock.now())
//
//  /**
//    * Called when messages arrive to this actor
//    *
//    * @return The actor receive
//    */
//  override def receive: Receive = {
//    // send the signals
//    case SendSignal =>
//      signals(neurons, clock.now() - start).foreach {
//        case (neuron: ActorRef, signal: ElectricPotential) => neuron ! Signal(clock.now(), SignalReceiver.world, signal)
//      }
//
//    // stop sending signals and shut-down this environment actor
//    case Stop =>
//      sendSignalSchedule.cancel()
//      context.stop(self)
//  }
//}
//
///**
//  * Holds factory methods for creating the environment actor, and messages.
//  */
//object BasicEnvironment {
//
//  /**
//    * Creates the environment or sending a signals after the specified delay, periodically, for the specified
//    * duration.
//    *
//    * @param system       The actor system to which this environment actor will belong
//    * @param neurons      The neurons to which to send the signal
//    * @param clock        The signal clock used be the network for timing events
//    * @param initialDelay The initial delay before sending a signal
//    * @param period       The delay between periodic signals (in real time)
//    * @param duration     The duration for which to send signals (in real time)
//    * @param signals      The signal function that accepts a neuron and returns the strength of the signal to send
//    *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
//    * @param cleanup      An optional function that is called after the environment actor is stopped. If not specified,
//    *                     then does nothing.
//    * @return A reference to the environment actor
//    */
//  def from(system: ActorSystem,
//           neurons: Seq[ActorRef],
//           clock: SignalClock,
//           initialDelay: FiniteDuration,
//           period: FiniteDuration,
//           duration: FiniteDuration,
//           signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential] = (_, _) => Map(),
//           cleanup: (Time, Time) => Unit = (_, _) => Unit): ActorRef = {
//    system.actorOf(props(
//      neurons = neurons,
//      clock = clock,
//      initialDelay = initialDelay,
//      period = period,
//      duration = duration,
//      signals = signals,
//      cleanup = cleanup
//    ))
//  }
//
//  /**
//    * Creates the actor properties for the environment actor
//    * @param neurons      The neurons to which to send the signal
//    * @param clock        The signal clock used be the network for timing events
//    * @param initialDelay The initial delay before sending a signal
//    * @param period       The delay between periodic signals (in real time)
//    * @param duration     The duration for which to send signals (in real time)
//    * @param signals      The signal function that accepts a neuron and returns the strength of the signal to send
//    *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
//    * @param cleanup      An optional function that is called after the environment actor is stopped. If not specified,
//    *                     then does nothing.
//    * @return A set of actor properties from which to create a the environment actor
//    */
//  def props(neurons: Seq[ActorRef],
//            clock: SignalClock,
//            initialDelay: FiniteDuration,
//            period: FiniteDuration,
//            duration: FiniteDuration,
//            signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential],
//            cleanup: (Time, Time) => Unit): Props =
//    Props(new BasicEnvironment(
//      neurons = neurons,
//      clock = clock,
//      initialDelay = initialDelay,
//      period = period,
//      duration = duration,
//      signals = signals,
//      cleanup = cleanup
//    ))
//
//  /**
//    * Message to send a signal to the input neurons
//    */
//  case object SendSignal
//
//  /**
//    * Message to stop sending signals to the input neurons and shut down the environment actor
//    */
//  case object Stop
//
//}
