package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames

case class NetworkCreated(networkId: String) {
  import MessageNames._
  override def toString: String = s"${NETWORK_CREATED.name}; network_id: $networkId"

}
