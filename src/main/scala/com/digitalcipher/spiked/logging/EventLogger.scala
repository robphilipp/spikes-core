package com.digitalcipher.spiked.logging

import com.digitalcipher.spiked.logging.FileEventLogger.{FileLoggingConfiguration, LoggingConfiguration}
import com.digitalcipher.spiked.logging.KafkaEventLogger.KafkaConfiguration

import scala.collection.immutable.HashMap

/**
  * Holds the loggers for each run ID and provides a unified log method to log to all configured loggers
  * with matching run ID at the same time
  */
object EventLogger {
  private val FALLBACK_LOGGER_NAME: String = "fallback-event-logger-3141592653"
  private var fileEventLoggers: Map[String, FileEventLogger] = new HashMap[String, FileEventLogger]()
  private var kafkaEventLoggers: Map[String, KafkaEventLogger] = new HashMap[String, KafkaEventLogger]()

  /**
    * Adds a logger that is configured programmatically instead of through a `logback` config file
    * @param loggingConfiguration The logging configuratin
    */
  def withLogging(loggingConfiguration: FileLoggingConfiguration): Unit = {
    fileEventLoggers += (loggingConfiguration.runId -> new FileEventLogger(loggingConfiguration.runId).withLogging(loggingConfiguration))
  }

  /**
    * @param loggingConfig The configuration for the Akka logger
    */
  def withLogging(loggingConfig: LoggingConfiguration): Unit = {
    fileEventLoggers += (FALLBACK_LOGGER_NAME -> new FileEventLogger(FALLBACK_LOGGER_NAME).withLogging(loggingConfig))
  }

  /**
    * @param kafkaConfiguration The configuration for the logging events to Kafka
    */
  def withLogging(kafkaConfiguration: KafkaConfiguration): Unit = {
    kafkaEventLoggers += (kafkaConfiguration.topic -> new KafkaEventLogger().withLogging(kafkaConfiguration.config, kafkaConfiguration.topic))
  }

  /**
    * Logs the message returned by the specified supplier function. The log destination depends on the configuration
    * specified in one of the `withLogging(...)` methods
    * @param runId The ID of the run that is being logged
    * @param messageSupplier The message supplier
    * @see withLogging(loggingConfiguration: FileLoggingConfiguration),
    *      withLogging(loggingConfiguration: LoggingConfiguration)
    *      withLogging(loggingConfiguration: KafkaConfiguration)
    */
  def log(runId: String, messageSupplier: () => Any): Unit = {
    fileEventLoggers.get(runId).foreach(logger => logger.log(runId, messageSupplier))
    kafkaEventLoggers.get(runId).foreach(logger => logger.log(messageSupplier))
  }
}