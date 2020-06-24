package com.digitalcipher.spiked.logging

import cakesolutions.kafka.{KafkaProducer, KafkaProducerRecord}
import cakesolutions.kafka.KafkaProducer.Conf
import MessageNames._
import akka.actor.ActorSystem
import com.digitalcipher.spiked.logging.messages._
import com.typesafe.config.Config
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

/**
  * Singleton. Requires configuration.
  * <p>Logs network events to Kafka.
  * <p>Singleton that must be configured once when the application starts up, and then can be used throughout the application.
  * <p>To use, first configure the Kafka Event Logger by calling it "constructor", ```KafkaEventLogger(ConfigFactory.load,"topic")```
  * and then call the logging function, handing it a support class type. For example, ```KafkaEventLogger.log(summary)```
  * <p>Note that the [[KafkaEventLogger]] can only be configured once
  * <p>Created by rob on 3/18/17.
  */
private[logging] class KafkaEventLogger {
  private lazy val producer: KafkaProducer[String, String] = configuredProducer.get
  private var configuredProducer: Option[KafkaProducer[String, String]] = None

  private lazy val topic: String = configuredTopic.get
  private var configuredTopic: Option[String] = None

  /**
    * Configures the Kafka event logger (Kafka cluster locations, and the Kafka topic)
    * @param config The configuration, from, for example, [[com.typesafe.config.ConfigFactory#load]]
    * @param topic  The topic name
    */
  def withLogging(config: Config, topic: String): KafkaEventLogger = {
    if (configuredProducer.isDefined) throw new IllegalStateException("KafkaProducer configuration already specified")
    configuredProducer = Some(KafkaProducer(Conf(config, new StringSerializer, new StringSerializer)))
    configuredTopic = Some(topic)
    this
  }

  /**
    * @return `true` when the configuration and topic have been specified
    */
  def isConfigured: Boolean = configuredProducer.isDefined && configuredTopic.isDefined

  /*
   |  In the logging method below, the use of the `logIfConfigured` and the function allow calls to these methods
   |  when the KafkaEventLogger hasn't been configured (in which case no message is sent and there is no error)
   */

  import spray.json._
  import com.digitalcipher.spiked.logging.json.SpikeEventsJsonProtocol._

  /**
    * Logs the serialized version of the message that is returned by the specified supplier
    *
    * @param supplier The serializable message object supplier
    */
  def log(supplier: () => Any): Unit = {
    val message = supplier()
    logIfConfigured(message match {
      case summary: NetworkSummary => producerRecord(SUMMARY.name, summary.toJson.compactPrint)
      case topology: NetworkTopology => producerRecord(TOPOLOGY.name, topology.toJson.compactPrint)
      case neuronCreated: NeuronCreated => producerRecord(NEURON_CREATED.name, neuronCreated.toJson.compactPrint)
      case learning: StdpHardLimitLearningFunction => producerRecord(LEARNING.name, learning.toJson.compactPrint)
      case learning: StdpSoftLimitLearningFunction => producerRecord(LEARNING.name, learning.toJson.compactPrint)
      case learning: StdpAlphaLearningFunction => producerRecord(LEARNING.name, learning.toJson.compactPrint)
      case connection: NetworkConnected => producerRecord(NETWORK_CONNECTED.name, connection.toJson.compactPrint)
      case postSynapticConnection: ConnectedPostSynaptic => producerRecord(CONNECT.name, postSynapticConnection.toJson.compactPrint)
      case registration: PreSynapticRegistration => producerRecord(REGISTER.name, registration.toJson.compactPrint)
      case networkCreated: NetworkCreated => producerRecord(NETWORK_CREATED.name, networkCreated.toJson.compactPrint)
      case weightUpdated: StdpWeightUpdated => producerRecord(WEIGHT_UPDATE.name, weightUpdated.toJson.compactPrint)
      case intrinsicPlasticityUpdated: IntrinsicPlasticityUpdated => producerRecord(INTRINSIC_PLASTICITY_UPDATE.name, intrinsicPlasticityUpdated.toJson.compactPrint)
      case signalReceived: SignalReceived => producerRecord(SIGNAL_RECEIVED.name, signalReceived.toJson.compactPrint)
      case update: MembranePotentialUpdate => producerRecord(MEMBRANE_POTENTIAL_UPDATE.name, update.toJson.compactPrint)
      case phaseTransition: PhaseTransition => producerRecord(PHASE_TRANSITION.name, phaseTransition.toJson.compactPrint)
      case spiked: Spiked => producerRecord(SPIKED.name, spiked.toJson.compactPrint)
    })
  }

  /**
    * Sends a message, from the message supplier, to the kafka cluster if the [[KafkaEventLogger]] is configured
    *
    * @param message The message to log
    */
  private def logIfConfigured(message: ProducerRecord[String, String]): Unit =
    if (isConfigured) producer.send(message)

  /**
    * Constructs the message in the format needed for Kafka (i.e. [[ProducerRecord]]). The topic is configured.
    *
    * @param key The key associated with the message that is added to the producer record
    * @param message   The message to send
    * @return The [[ProducerRecord]] for the topic, partition, and message
    */
  private def producerRecord(key: String, message: String): ProducerRecord[String, String] =
    KafkaProducerRecord(topic, Some(key), message)
}

object KafkaEventLogger {
  case class KafkaConfiguration(actorSystem: ActorSystem, config: Config, topic: String) {
    def runId: String = actorSystem.name
  }
}
