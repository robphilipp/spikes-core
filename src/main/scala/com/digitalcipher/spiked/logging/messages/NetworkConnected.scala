package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import com.digitalcipher.spiked.topology.coords.spatial.Points
import squants.space.Length

/**
  * Created by rob on 3/19/17.
  */
case class NetworkConnected(preSynapticId: String,
                            postSynapticId: String,
                            initialWeight: Double,
                            equilibriumWeight: Double,
                            preSynapticLocation: Points.Cartesian,
                            postSynapticLocation: Points.Cartesian,
                            distance: Length) {
  import MessageNames.NETWORK_CONNECTED
  override def toString: String = s"${NETWORK_CONNECTED.name}; " +
    s"pre_synaptic: $preSynapticId; " +
    s"post_synaptic: $postSynapticId; " +
    s"initial_weight: $initialWeight; " +
    s"equilibrium_weight: $equilibriumWeight; " +
    s"pre_synaptic_location: $preSynapticLocation; " +
    s"post_synaptic_location: $postSynapticLocation; " +
    s"distance: ${f"${distance.toMicrons}%.2f µm"}"
}
