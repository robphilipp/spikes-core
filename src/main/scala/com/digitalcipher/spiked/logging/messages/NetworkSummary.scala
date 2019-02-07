package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import com.digitalcipher.spiked.topology.NeuronType
import com.digitalcipher.spiked.topology.NeuronType.NeuronType

/**
  * Created by rob on 3/19/17.
  */
case class NetworkSummary(counts: Map[NeuronType, Int]) {
  import MessageNames._
  override def toString: String = s"${SUMMARY.name}; " +
    s"input_neurons: ${counts.getOrElse(NeuronType.Input, 0)}; " +
    s"hidden_neurons: ${counts.getOrElse(NeuronType.Hidden, 0)}; " +
    s"output_neurons: ${counts.getOrElse(NeuronType.Output, 0)}"
}
