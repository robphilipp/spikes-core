package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time
import squants.electro.ElectricPotential

/**
  * Created by rob on 3/26/17.
  */
case class SignalReceived(neuronId: String,
                          sourceId: String,
                          timestamp: Time,
                          lastEventTime: Time,
                          lastFireTime: Time,
                          signalIntensity: ElectricPotential) {

  import MessageNames.SIGNAL_RECEIVED
  override def toString: String = {
    s"${SIGNAL_RECEIVED.name}; " +
      s"id: $neuronId; " +
      s"source: $sourceId; " +
      s"timestamp: $timestamp; " +
      s"last_event: $lastEventTime; " +
      s"last_fire: $lastFireTime; " +
      s"signal_intensity: $signalIntensity"
  }
}
