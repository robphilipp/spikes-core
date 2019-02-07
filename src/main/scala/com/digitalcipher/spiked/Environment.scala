package com.digitalcipher.spiked

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.digitalcipher.spiked.neurons.{Signal, SignalClock, SignalReceiver}
import squants.Time
import squants.electro.ElectricPotential

import scala.concurrent.duration.FiniteDuration

/**
  * Signal sender from the environment. Sends a signal after the specified delay, periodically, for the specified
  * duration.
  *
  * @param neurons      The neurons to which to send the signal
  * @param initialDelay The initial delay before sending a signal (in real time)
  * @param period       The delay between periodic signals (in real time)
  * @param duration     The duration for which to send signals (in real time)
  * @param signals      The signal function that accepts a neuron and returns the strength of the signal to send
  *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
  * @param cleanup      A function that is called after the environment actor is stopped
  */
class Environment(neurons: Seq[ActorRef],
                  clock: SignalClock,
                  initialDelay: FiniteDuration,
                  period: FiniteDuration,
                  duration: FiniteDuration,
                  signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential],
                  cleanup: (Time, Time) => Unit) extends Actor {

  import context.dispatcher

  private val tick = context.system.scheduler.schedule(initialDelay, period, self, "tick")
  private val start = clock.now()

  context.system.scheduler.scheduleOnce(initialDelay + duration, self, "stop")

  override def postStop(): Unit = cleanup(start, clock.now())

  override def receive: Receive = {
    case "tick" =>
      val time = clock.now() - start
      signals(neurons, time).foreach {
        case (neuron: ActorRef, signal: ElectricPotential) => neuron ! Signal(clock.now(), SignalReceiver.world, signal)
      }

    case "stop" =>
      tick.cancel()
      context.stop(self)
  }
}

object Environment {
  /**
    * Creates the environment or sending a signals after the specified delay, periodically, for the specified
    * duration.
    *
    * @param neurons      The neurons to which to send the signal
    * @param initialDelay The initial delay before sending a signal
    * @param period       The delay between periodic signals (in real time)
    * @param duration     The duration for which to send signals (in real time)
    * @param signals       The signal function that accepts a neuron and returns the strength of the signal to send
    *                     to that neuron (function (neuron, time) => signal, where the time is in simulation time)
    * @param cleanup      An optional function that is called after the environment actor is stopped. If not specified,
    *                     then does nothing.
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
      cleanup = cleanup)
    )
  }

  def props(neurons: Seq[ActorRef],
            clock: SignalClock,
            initialDelay: FiniteDuration,
            period: FiniteDuration,
            duration: FiniteDuration,
            signals: (Seq[ActorRef], Time) => Map[ActorRef, ElectricPotential],
            cleanup: (Time, Time) => Unit): Props = Props(new Environment(
    neurons = neurons,
    clock = clock,
    initialDelay = initialDelay,
    period = period,
    duration = duration,
    signals = signals,
    cleanup = cleanup
  ))
}
