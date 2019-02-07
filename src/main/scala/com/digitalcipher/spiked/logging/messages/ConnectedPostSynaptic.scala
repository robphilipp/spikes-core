package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time

/**
  * Created by rob on 6/17/17.
  */
case class ConnectedPostSynaptic(preSynapticId: String, postSynapticId: String, signalDelay: Time) {
  import MessageNames.CONNECT
  override def toString: String = s"${CONNECT.name}; " +
    s"pre_synaptic: $preSynapticId; " +
    s"post_synaptic: $postSynapticId; " +
    s"signal_delay: ${f"${signalDelay.toMilliseconds}%.2f"} ms"
}
