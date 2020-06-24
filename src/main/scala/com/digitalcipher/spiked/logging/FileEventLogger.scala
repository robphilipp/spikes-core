package com.digitalcipher.spiked.logging

import akka.actor.ActorSystem
import akka.event.{EventStream, Logging}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.{Appender, FileAppender}
import com.digitalcipher.spiked.logging.FileEventLogger.{EVENTS, FileLoggingConfiguration, LoggingConfiguration, loggerName}
import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.neurons._

/**
  * Created by rob on 4/1/17.
  */
private[logging] class FileEventLogger(val runId: String) {
  private lazy val eventStream: EventStream = configuredEventStream.get
  private var configuredEventStream: Option[EventStream] = None

  private lazy val networkInfoLogger = Logging(eventStream, s"${NetworkSetupEvent.getClass.getName}.$runId")
  private lazy val connectionLogger = Logging(eventStream, s"${ConnectionEvent.getClass.getName}.$runId")
  private lazy val registerPreSynapticLogger = Logging(eventStream, s"${RegisterPreSynapticEvent.getClass.getName}.$runId")
  private lazy val networkCreatedLogger = Logging(eventStream, s"${NetworkCreated.getClass.getName}.${runId}")
  private lazy val signalReceivedLogger = Logging(eventStream, s"${ReceiveSignalEvent.getClass.getName}.$runId")
  private lazy val stdpWeightUpdateLogger = Logging(eventStream, s"${StdpWeightUpdateEvent.getClass.getName}.$runId")
  private lazy val intrinsicPlasticityUpdatedLogger = Logging(eventStream, s"${IntrinsicPlasticityUpdateEvent.getClass.getName}.$runId")
  private lazy val updatePotentialLogger = Logging(eventStream, s"${UpdateMembranePotentialEvent.getClass.getName}.$runId")
  private lazy val phaseTransitionLogger = Logging(eventStream, s"${PhaseTransitionEvent.getClass.getName}.$runId")
  private lazy val spikeLogger = Logging(eventStream, s"${FireEvent.getClass.getName}.$runId")

  /**
    * Sets up the logger with the specified name, with the specified file-appender, and the specified logger context
    * @param loggerName The name of the logger (typically this is the class name appended with the run ID)
    * @param fileAppender The file-appender
    * @param loggerContext The logger context
    */
  private def setUpLogger(loggerName: String, fileAppender: FileAppender[String], loggerContext: LoggerContext): Unit = {
    val logger = loggerContext.getLogger(loggerName)
    logger.setAdditive(false)
    logger.setLevel(Level.DEBUG)
    logger.addAppender(fileAppender.asInstanceOf[Appender[ILoggingEvent]])
  }

  def stopLogger(): Unit = {
    val loggerContext: LoggerContext = org.slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    EVENTS.foreach(event => loggerContext.getLogger(loggerName(runId, event)).detachAndStopAllAppenders())
  }

  /**
    * Sets up logging programmatically instead of using a `logback` configuration file
    *
    * @param loggingConfiguration The logging configuration
    */
  def withLogging(loggingConfiguration: FileLoggingConfiguration): FileEventLogger = {
    val loggerContext: LoggerContext = org.slf4j.LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val fileAppender: FileAppender[String] = new FileAppender[String]()
    fileAppender.setContext(loggerContext)
    fileAppender.setName(loggingConfiguration.runId)
    fileAppender.setFile(loggingConfiguration.filename)
    fileAppender.setAppend(false)

    val encoder: PatternLayoutEncoder = new PatternLayoutEncoder()
    encoder.setContext(loggerContext)
    encoder.setPattern(loggingConfiguration.runId + " - %msg%n")
    encoder.start()

    fileAppender.setEncoder(encoder.asInstanceOf[Encoder[String]])
    fileAppender.start()

    EVENTS.foreach(event => setUpLogger(loggerName(runId, event), fileAppender, loggerContext))

    withLogging(LoggingConfiguration(loggingConfiguration.eventStream))
    this
  }

  /**
    * @param loggingConfig The configuration for the Akka logger
    */
  def withLogging(loggingConfig: LoggingConfiguration): FileEventLogger = {
    if (loggingConfig != null) {
      if (configuredEventStream.isDefined) throw new IllegalStateException("Event stream already specified")
      configuredEventStream = Some(loggingConfig.eventStream)
    }
    this
  }

  /**
    * @return `true` when the event stream has been specified
    */
  def isConfigured: Boolean = configuredEventStream.isDefined

  /**
    * Logs the serialized version of the message that is returned by the specified supplier
    *
    * @param supplier The serializable message object supplier
    */
  def log(runId: String, supplier: () => Any): Unit = {
    // if either or both of the loggers are configured, then grab the message from the supplier
    // we only want to create the message once
    if (isConfigured && this.runId.equals(runId)) {
      supplier() match {
          // todo change the loggers to either 'info', or allow the log-level to be set in the config file so that
          //      the logging level of the events can be set
        case summary: NetworkSummary => networkInfoLogger.debug(summary.toString)
        case topology: NetworkTopology => networkInfoLogger.debug(topology.toString)
        case learning: StdpHardLimitLearningFunction => networkInfoLogger.debug(learning.toString)
        case learning: StdpSoftLimitLearningFunction => networkInfoLogger.debug(learning.toString)
        case learning: StdpAlphaLearningFunction => networkInfoLogger.debug(learning.toString)
        case connection: NetworkConnected => connectionLogger.debug(connection.toString)
        case postSynapticConnection: ConnectedPostSynaptic => connectionLogger.debug(postSynapticConnection.toString)
        case registration: PreSynapticRegistration => registerPreSynapticLogger.debug(registration.toString)
        case networkCreated: NetworkCreated => networkCreatedLogger.debug(networkCreated.toString)
        case weightUpdated: StdpWeightUpdated => stdpWeightUpdateLogger.debug(weightUpdated.toString)
        case intrinsicPlasticityUpdated: IntrinsicPlasticityUpdated => intrinsicPlasticityUpdatedLogger.debug(intrinsicPlasticityUpdated.toString)
        case signalReceived: SignalReceived => signalReceivedLogger.debug(signalReceived.toString)
        case update: MembranePotentialUpdate => updatePotentialLogger.debug(update.toString)
        case phaseTransition: PhaseTransition => phaseTransitionLogger.debug(phaseTransition.toString)
        case spiked: Spiked => spikeLogger.debug(spiked.toString)
      }
    }
  }
}

object FileEventLogger {
  private val EVENTS: Set[Any] = Set(
    NetworkSetupEvent,
    ConnectionEvent,
    RegisterPreSynapticEvent,
    ReceiveSignalEvent,
    StdpWeightUpdateEvent,
    IntrinsicPlasticityUpdateEvent,
    UpdateMembranePotentialEvent,
    PhaseTransitionEvent,
    FireEvent
  )

  def loggerName(runId: String, event: Any): String = s"${event.getClass.getName}.$runId"

  case class LoggingConfiguration(eventStream: EventStream)

  case class FileLoggingConfiguration(actorSystem: ActorSystem, filename: String, logEvents: Set[Any] = EVENTS) {
    def runId: String = actorSystem.name
    def eventStream: EventStream = actorSystem.eventStream
  }
}