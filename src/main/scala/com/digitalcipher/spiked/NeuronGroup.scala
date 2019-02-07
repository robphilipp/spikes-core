package com.digitalcipher.spiked

import akka.actor.{Actor, ActorSystem, Props}
import com.digitalcipher.spiked.NeuronGroup.{args, system}
import com.digitalcipher.spiked.construction.NetworkBuilder
import com.digitalcipher.spiked.logging.EventLogger
import com.digitalcipher.spiked.logging.FileEventLogger.FileLoggingConfiguration
import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.mutable

case class CreateActorSystem(name: String)
case class DestroyActorSystem(name: String)

class Manager(neuronGroupConfig: Config) extends Actor {
  private val systems = new mutable.HashMap[String, ActorSystem]

  override def receive: Receive = {
    case CreateActorSystem(name) => {
      val system = ActorSystem(name, neuronGroupConfig)
      systems.put(name, system)
      EventLogger.withLogging(FileLoggingConfiguration(actorSystem = system, filename = s"test-output/neuron-activity.$name.log"))
      println(s"Started neuron group runner; system name: $name")
    }
    case DestroyActorSystem(name) => systems.get(name).foreach(system => system.terminate())
  }
}

object Manager {
  def props(neuronGroupConfig: Config): Props = Props(new Manager(neuronGroupConfig))
}

/**
  * Created by rob on 7/16/17.
  */
object NeuronGroup extends App {
  if (args.isEmpty || args(0) == "--help" || args.length < 2) {
    println("neuron_group [config file] [time factor]\"")
    System.exit(0)
  }

  val config = ConfigFactory.load(args(1));
  val system = ActorSystem("neuron_group_manager", config)
  val manager = system.actorOf(Manager.props(config))

  // set-up logging
//  val config = ConfigFactory.load()
  //
  // set-up logging
  EventLogger.withLogging(FileLoggingConfiguration(actorSystem = system, filename = s"test-output/manager.log"))
//  EventLogger.withLogging(KafkaConfiguration(config = config.getConfig("kafka"), topic = "spikes"))

  println(s"Started neuron group runner; system name: ${args(0)}; config file: ${args(1)}")
//  if (args.isEmpty || args(0) == "--help" || args.length < 3) {
//    println("neuron_group [actor system] [config file] [time factor]\"")
//    System.exit(0)
//  }
//
//  val system = ActorSystem(args(0), ConfigFactory.load(args(1)))
//  // set-up logging
//  val config = ConfigFactory.load()
//  //
//  // set-up logging
//  EventLogger.withLogging(FileLoggingConfiguration(actorSystem = system, filename = s"test-output/neuron-activity.314.log"))
////  EventLogger.withLogging(KafkaConfiguration(config = config.getConfig("kafka"), topic = "spikes"))
//
//  println(s"Started neuron group runner; system name: ${args(0)}; config file: ${args(1)}")
}
