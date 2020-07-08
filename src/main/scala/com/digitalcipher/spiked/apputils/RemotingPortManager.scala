package com.digitalcipher.spiked.apputils

import com.typesafe.config.Config
import org.slf4j.LoggerFactory

/**
  * Immutable.
  * Holds the ports currently available for an actor system that is using remoting to connect to the
  * spikes node.
  *
  * @param ports A list of the available ports
  */
case class RemotingPortManager(ports: List[Int]) {
  private val logger = LoggerFactory.getLogger("spikes-core-remoting-port-manager")

  var availablePorts: List[Int] = ports
  var usedPorts: Map[String, Int] = Map()

  /**
    * Retrieves an available port, if one is available.
    *
    * @param systemName The name of the actor system requesting the port
    * @return Some port; or None in no ports are available
    */
  def checkOutRemotingPort(systemName: String): Option[Int] = {
    val port = availablePorts
      .diff(usedPorts.values.toList)
      .headOption
      .map(port => {
        usedPorts += (systemName -> port)
        availablePorts = availablePorts.tail
        port
      })

    logger.info(
      "checked out remoting port; actor-system name: {}; assigned port: {}; used ports: {}; available ports: {}",
      systemName, port.map(p => s"$p").getOrElse("[none]"), usedPorts.size.toString, availablePorts.size.toString
    )

    port
  }

  /**
    * Return the remoting port to the available ports and remove it from the used ones
    *
    * @param systemName The name of the system for which to return the port
    * @return
    */
  def returnRemotingPort(systemName: String): Unit = {
    val port = usedPorts.get(systemName)
    port.foreach(port => {
        availablePorts = availablePorts :+ port
        usedPorts -= systemName
    })
    logger.info(
      "returned remoting port; actor-system name: {}; port: {}; used ports: {}; available ports: {}",
      systemName, port.map(p => s"$p").getOrElse("[none]"), usedPorts.size.toString, availablePorts.size.toString
    )
  }

  /**
    * Returns the port for the system name and executes the specified action
    * @param systemName The name of the actor system for which to return the port
    * @param action The action to perform
    */
  def returnRemotingPortAnd(systemName: String, action: () => Unit): Unit = {
    usedPorts.get(systemName)
      .foreach(port => {
        // perform the requested action
        action()

        returnRemotingPort(systemName)
        logger.info(
          "returned remoting port and performed requested action; actor-system name: {}; port: {}; used ports: {}; available ports: {}",
          systemName, port.toString, usedPorts.size.toString, availablePorts.size.toString
        )
      })
  }
}

/**
  * Companion object that hos a method to parse the remoting-ports configuration item.
  */
object RemotingPortManager {
  /**
    * Pulls the remote ports from the configuration file when the actor-provider is set to remote
    *
    * @param config The configuration
    * @return An Option holding an Iterable collection of ports; or an empty Option
    *         when the actor-provider is not set to remote
    */
  def parseRemotingPortsFrom(config: Config): Option[List[Int]] = {
    if (config.getString("akka.actor.provider") == "remote" && config.hasPath("remoting-ports")) {
      import scala.collection.JavaConverters._
      val portRange = raw"([0-9]+)\.\.([0-9]+)".r
      val singlePort = raw"([0-9]+)".r
      Some(
        asScalaBuffer(config.getStringList("remoting-ports"))
          .flatMap {
            case portRange(start, end) => (start.toInt until end.toInt + 1).toList
            case singlePort(port) => List(port.toInt)
            case _ => List.empty
          }
          .toList
      )
    }
    else {
      None
    }
  }
}
