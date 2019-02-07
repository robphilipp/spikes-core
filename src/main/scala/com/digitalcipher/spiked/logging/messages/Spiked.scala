package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time
import squants.electro.ElectricPotential

/**
  * Created by rob on 3/26/17.
  */
case class Spiked(neuronId: String,
                  timestamp: Time,
                  signalIntensity: ElectricPotential,
                  lastFireTime: Time) {

  override def toString: String = {
    s"${MessageNames.SPIKED.name}; " +
      s"id: $neuronId; " +
      s"timestamp: $timestamp; " +
      s"signal_intensity: $signalIntensity; " +
      s"last_fire: $lastFireTime"
  }
}
