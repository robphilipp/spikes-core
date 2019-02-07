package com.digitalcipher.spiked.neurons

import akka.actor.{Actor, ActorRef}

/**
  * A receiver of [[Signal]]s
  */
trait SignalReceiver extends Actor {
  def id: String
}

/**
  * Companion object to the [[SignalReceiver]] class. Manages the simulation timing information
  * for each of the actor systems running.
  */
object SignalReceiver {
  final val world: ActorRef = Actor.noSender

  /**
    * Represents a query to a neuron. For example, a neuron may report back information about itself, such
    * as its ID, etc
    *
    * @param request The request string
    */
  case class Query(request: String)

  /**
    * Represents a query to a neuron for its ID
    */
  case class IdQuery()

  /**
    * Represents a query for the neuron's time-factor
    */
  case class TimeFactorQuery()
}
