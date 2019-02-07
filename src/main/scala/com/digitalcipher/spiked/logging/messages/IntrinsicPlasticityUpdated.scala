package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time
import squants.electro.ElectricPotential

/**
  * Message for the updated intrinsic plasticity
 *
  * @param neuronId The ID of the neuron
  * @param timestamp The timestamp of the update
  * @param intrinsicPlasticity The value of the intrinsic plasticity
  */
case class IntrinsicPlasticityUpdated(neuronId: String,
                                      timestamp: Time,
                                      intrinsicPlasticity: ElectricPotential) {

  import MessageNames.INTRINSIC_PLASTICITY_UPDATE
  override def toString: String = {
    s"${INTRINSIC_PLASTICITY_UPDATE.name}; " +
      s"id: $neuronId; " +
      s"timestamp: $timestamp; " +
      s"intrinsic_plasticity: $intrinsicPlasticity"
  }
}
