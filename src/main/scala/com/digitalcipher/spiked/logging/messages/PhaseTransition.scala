package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time
import squants.electro.ElectricPotential
import squants.time.Frequency

/**
  * Created by rob on 3/26/17.
  */
case class PhaseTransition(neuronId: String,
                          transitionType: String,
                          timestamp: Time,
                          membranePotential: ElectricPotential,
                          firingRate: Frequency) {

  override def toString: String = {
    s"${MessageNames.PHASE_TRANSITION.name}; " +
      s"id: $neuronId; " +
      s"transition_type: $transitionType; " +
      s"transition_timestamp: $timestamp; " +
      s"potential: $membranePotential; " +
      s"firing_rate: $firingRate"
  }
}
