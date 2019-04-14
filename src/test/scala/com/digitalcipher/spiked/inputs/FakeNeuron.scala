package com.digitalcipher.spiked.inputs

import akka.actor.{Actor, Props}
import com.digitalcipher.spiked.neurons.Signal

class FakeNeuron extends Actor {
  override def receive: Receive = {
    case Signal(time, from, signal) => sender() ! s"${signal.toMillivolts}; ${from}; ${time.toMilliseconds}"
    case _ => "shit"
  }
}

object FakeNeuron {
  def props(): Props = Props(new FakeNeuron)
}

