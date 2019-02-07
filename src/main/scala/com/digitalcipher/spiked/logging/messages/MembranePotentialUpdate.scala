package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time
import squants.electro.ElectricPotential

/**
  * Created by rob on 3/26/17.
  */
case class MembranePotentialUpdate(neuronId: String,
                                   timestamp: Time,
                                   lastEventTime: Time,
                                   lastFireTime: Time,
                                   membranePotential: ElectricPotential) {

  import MessageNames.MEMBRANE_POTENTIAL_UPDATE
  override def toString: String = {
    s"${MEMBRANE_POTENTIAL_UPDATE.name}; " +
      s"id: $neuronId; " +
      s"signal_timestamp: $timestamp; " +
      s"last_event: $lastEventTime; " +
      s"last_fire: $lastFireTime; " +
      s"potential: $membranePotential"
  }
}
