package com.digitalcipher.spiked.neurons

import akka.actor.ActorRef
import squants.Time
import squants.electro.ElectricPotential

/**
  * Case class representing a signal that has a timestamp and an intensity (i.e. the value)
  *
  * @param timestamp The timestamp of the signals creation
  * @param value     The intensity of the signal
  */
case class Signal(timestamp: Time, preSynaptic: ActorRef, value: ElectricPotential) {

  /**
    * Creates a new signal from this one, with the updated timestamp
    *
    * @param timestamp The new timestamp
    * @return A [[Signal]] with the same value as this one, but with the new timestamp
    */
  def update(timestamp: Time): Signal = Signal.updateTimestamp(timestamp, this)

  /**
    * @return A string representation of the [[Signal]]
    */
  override def toString: String = s"( $timestamp, $value, ${if (preSynaptic == SignalReceiver.world) "[world]" else preSynaptic.path.name} )"
}

/**
  * The companion to the [[Signal]] class
  */
object Signal {
  /**
    * Constructs a new [[Signal]] instance that has the specified timestamp, but the [[Signal]]'s value
    *
    * @param timestamp The new timestamp for the signal
    * @param signal    The [[Signal]] whose intensity (value) to copy
    * @return A [[Signal]] instance with the specified timestamp and the specified [[Signal]]'s value
    */
  def updateTimestamp(timestamp: Time, signal: Signal): Signal = Signal(timestamp, signal.preSynaptic, signal.value)
}
