package com.digitalcipher.spiked.neurons

import java.time.{Clock, Instant}

import squants.Time
import squants.time.Milliseconds

/**
  * The signal clock used to determine simulation timing. Generally, the clock represents the time
  * from the network's point of view. Neurons have offsets to this clock to manage their own time.
  * @param timeFactor The number of seconds in real time that it takes to simulate 1 second in the
  *                   simulation. Specifically, a time-factor of N means that it takes N seconds
  *                   in real-time to simulate 1 second.
  * @param startTime The number of milliseconds from epoch at which time the simulation was started.
  *                  The start time is used as an offset from the current time to determine the
  *                  number of milliseconds that the simulation has been running.
  */
case class SignalClock(timeFactor: Int, startTime: Long) {

  /**
    * Resets the simulation start-time to the current number of milliseconds from epoch and returns
    * a new signal clock with the same time-factor and the new start time.
    * @return A new clock with the same time-factor, but the start time is set to the
    *         current time.
    */
  def reset(): SignalClock = SignalClock(timeFactor, SignalClock.current())

  /**
    * @return the number of milliseconds from the start of the simulation, in simulation time.
    */
  def now(): Time = Milliseconds((SignalClock.current() - startTime) / timeFactor)

  /**
    * Creates a new clock with the specified start time and the same time-factor
    * @param time The new start time
    * @return A new clock with the specified start time and the same time-factor
    */
  def withUpdatedStartTime(time: Long): SignalClock = SignalClock(timeFactor, time)
}

case object SignalClock {

  /**
    * Creates a signal clock with the specified time factor and a start time set to the current time
    * @param timeFactor The number of seconds in real time that it takes to simulate 1 second in the
    *                   simulation. Specifically, a time-factor of N means that it takes N seconds
    *                   in real-time to simulate 1 second.
    * @return a signal clock with the specified time factor and a start time set to the current time
    */
  def nowWithTimeFactor(timeFactor: Int): SignalClock = SignalClock(timeFactor, current())

  /**
    * @return The current number of milliseconds from epoch in real time (as opposed to simulation time)
    */
  def current(): Long = Instant.now(Clock.systemUTC()).toEpochMilli

  /**
    * Represents a query for the neuron's time-factor
    */
  case class TimeFactorQuery()
}
