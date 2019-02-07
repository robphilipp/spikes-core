package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames

/**
  * Created by rob on 3/19/17.
  */
case class PreSynapticRegistration(neuronId: String, preSynapticId: String, weight: Double) {
  import MessageNames.REGISTER
  override def toString: String = s"${REGISTER.name}; id: $neuronId; pre_synaptic: $preSynapticId; weight: $weight"
}
