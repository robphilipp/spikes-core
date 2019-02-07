package com.digitalcipher.spiked.logging

/**
  * Created by rob on 3/19/17.
  */
case object MessageNames {
  val SUMMARY: Symbol = 'summary
  val TOPOLOGY: Symbol = 'topology
  val LEARNING: Symbol = 'learning
  val CONNECT: Symbol = 'connect
  val NETWORK_CONNECTED: Symbol = 'network_connected
  val REGISTER: Symbol = 'register
  val WEIGHT_UPDATE: Symbol = 'learn
  val INTRINSIC_PLASTICITY_UPDATE: Symbol = 'intrinsic_plasticity
  val SIGNAL_RECEIVED: Symbol = 'receive
  val MEMBRANE_POTENTIAL_UPDATE: Symbol = 'update
  val PHASE_TRANSITION = 'transition
  val SPIKED: Symbol = 'fire
}
