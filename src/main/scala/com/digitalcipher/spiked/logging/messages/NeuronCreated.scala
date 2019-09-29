package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import com.digitalcipher.spiked.topology.coords.spatial.Points

/**
  * Created by rob on 3/19/17.
  */
case class NeuronCreated(neuronId: String,
                         neuronType: String,
                         inhibitory: Boolean,
                         location: Points.Cartesian
                        ) {
  import MessageNames._
  override def toString: String = s"${NEURON_CREATED.name}; neuron_id: $neuronId; neuron_type: $neuronType; inhibitory: $inhibitory; location: $location"
}
