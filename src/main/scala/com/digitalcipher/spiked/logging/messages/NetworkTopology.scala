package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import com.digitalcipher.spiked.topology.coords.spatial.Points

/**
  * Created by rob on 3/19/17.
  */
case class NetworkTopology(neuronId: String, location: Points.Cartesian) {
  import MessageNames._
  override def toString: String = s"${TOPOLOGY.name}; neuron_id: $neuronId; location: $location"
}
