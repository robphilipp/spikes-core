package com.digitalcipher.spiked.logging.json

import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.topology.NeuronType
import com.digitalcipher.spiked.topology.coords.spatial.Points
import com.digitalcipher.spiked.topology.coords.spatial.Points.Cartesian
import spray.json.DefaultJsonProtocol
import squants.Time
import squants.electro.{ElectricPotential, Microvolts, Millivolts, Volts}
import squants.space.{Centimeters, Microns, Millimeters}
import squants.time._

/**
  * JSON converter objects for classes that need to be converted to JSON
  */
object SpikeEventsJsonProtocol extends DefaultJsonProtocol {

  private val MESSAGE: String = "message"
  private val TYPE: String = "type"

  private val NEURON_ID = "neuronId"
  private val LOCATION = "location"
  private val PRE_SYNAPTIC = "preSynaptic"
  private val POST_SYNAPTIC = "postSynaptic"
  private val SIGNAL_DELAY = "signalDelay"
  private val INHIBITORY_AMPLITUDE = "inhibitoryAmplitude"
  private val INHIBITORY_PERIOD = "inhibitoryPeriod"
  private val EXCITATION_AMPLITUDE = "excitationAmplitude"
  private val EXCITATION_PERIOD = "excitationPeriod"
  private val BASELINE = "baseline"
  private val LEARNING_RATE = "learningRate"
  private val TIME_CONSTANT = "timeConstant"
  private val INITIAL_WEIGHT = "initialWeight"
  private val EQUILIBRIUM_WEIGHT = "equilibriumWeight"
  private val PRE_SYNAPTIC_LOCATION = "preSynapticLocation"
  private val POST_SYNAPTIC_LOCATION = "postSynapticLocation"
  private val DISTANCE = "distance"
  private val SOURCE_ID = "sourceId"
  private val PREVIOUS_WEIGHT = "previousWeight"
  private val NEW_WEIGHT = "newWeight"
  private val ADJUSTMENT = "adjustment"
  private val TIME_WINDOW = "timeWindow"
  private val STDP_TIME = "stdpTime"
  private val SIGNAL_TIME = "signalTime"
  private val TIMESTAMP = "timestamp"
  private val PLASTICITY = "plasticity"
  private val LAST_EVENT_TIME = "lastEventTime"
  private val LAST_FIRE_TIME = "lastFireTime"
  private val SIGNAL_INTENSITY = "signalIntensity"
  private val MEMBRANE_POTENTIAL = "membranePotential"
  private val TRANSITION_TYPE = "transitionType"
  private val FIRING_RATE = "firingRate"

  import spray.json._

  /**
    * Network summary holds the number if input, hidden, and output neurons. Recall that
    * the labels are arbitrary and a hidden neuron could send output signals.
    */
  implicit object NetworkSummaryJsonFormat extends RootJsonFormat[NetworkSummary] {

    import com.digitalcipher.spiked.logging.MessageNames.SUMMARY

    private implicit val jsonFormat: JsonFormat[NeuronType.Value] = EnumJsonConverter.jsonEnum(NeuronType)

    override def write(summary: NetworkSummary) = JsObject(
      TYPE -> JsString(SUMMARY.name),
      MESSAGE -> summary.counts.toJson
    )

    override def read(value: JsValue): NetworkSummary = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (SUMMARY.name, countMap) => countMap.asJsObject match {
          case Seq(JsObject(counts)) =>
            NetworkSummary(counts.map(entry => NeuronType.from(entry._1) -> entry._2.convertTo[Int]))
          case _ => deserializationError("map of neuron-type to counts expected")
        }
        case _ => deserializationError(s"(type: ${SUMMARY.name}, ...) message expected")
      }
      case _ => deserializationError("NetworkSummary expected")
    }
  }

  /**
    * Spatial cartesian coordinates holding the (x, y, z) position of a neuron
    */
  implicit object CartesianPointJsonFormat extends RootJsonFormat[Points.Cartesian] {
    override def write(point: Cartesian): JsValue = JsObject(
      "x" -> JsNumber(point._1.toMicrons),
      "y" -> JsNumber(point._2.toMicrons),
      "z" -> JsNumber(point._3.toMicrons),
      "units" -> JsString("µm")
    )

    override def read(value: JsValue): Cartesian = value.asJsObject.getFields("x", "y", "z", "units") match {
      case Seq(JsNumber(x), JsNumber(y), JsNumber(z), JsString(units)) => units match {
        case "µm" => Cartesian((Microns(x), Microns(y), Microns(z)))
        case "mm" => Cartesian(Millimeters(x), Millimeters(y), Millimeters(z))
        case "cm" => Cartesian(Centimeters(x), Centimeters(y), Centimeters(z))
        case _ => deserializationError(s"Distance units must be µm, mm, cm; specified: $units")
      }
      case _ => deserializationError("Cartesian point (spatial) expected")
    }
  }

  /**
    * Time, holding the dimension (value) and the units (i.e. ms)
    */
  implicit object TimeJsonFormat extends RootJsonFormat[Time] {
    override def write(time: Time): JsValue = JsObject(
      "value" -> JsNumber(time.toMilliseconds),
      "units" -> JsString(time.unit.symbol)
    )

    override def read(value: JsValue): Time = value.asJsObject.getFields("value", "units") match {
      case Seq(JsNumber(time), JsString(units)) => units match {
        case Microseconds.symbol => Microseconds(time)
        case Milliseconds.symbol => Milliseconds(time)
        case Seconds.symbol => Seconds(time)
        case _ => deserializationError("(value, units) expected")
      }
    }
  }

  /**
    * Electrical potential, holding the dimension and the units (i.e. mV)
    */
  implicit object ElectricPotentialJsonFormat extends RootJsonFormat[ElectricPotential] {
    override def write(time: ElectricPotential): JsValue = JsObject(
      "value" -> JsNumber(time.toMillivolts),
      "units" -> JsString(time.unit.symbol)
    )

    override def read(value: JsValue): ElectricPotential = value.asJsObject.getFields("value", "units") match {
      case Seq(JsNumber(time), JsString(units)) => units match {
        case Microseconds.symbol => Microvolts(time)
        case Milliseconds.symbol => Millivolts(time)
        case Seconds.symbol => Volts(time)
        case _ => deserializationError("(value, units) expected")
      }
    }
  }

  /**
    * Frequency, holding the dimension and the units (i.e. Hz)
    */
  implicit object FrequencyJsonFormat extends RootJsonFormat[Frequency] {
    override def write(frequency: Frequency): JsValue = JsObject(
      "value" -> JsNumber(frequency.toKilohertz),
      "units" -> JsString(frequency.unit.symbol)
    )

    override def read(value: JsValue): Frequency = value.asJsObject.getFields("value", "units") match {
      case Seq(JsNumber(time), JsString(units)) => units match {
        case Hertz.symbol => Hertz(time)
        case Kilohertz.symbol => Kilohertz(time)
        case Megahertz.symbol => Megahertz(time)
        case _ => deserializationError("(value, units) expected")
      }
    }
  }

  /**
    * The network topology holding the neuron ID and it's location
    */
  implicit object NetworkTopologyJsonFormat extends RootJsonFormat[NetworkTopology] {

    import com.digitalcipher.spiked.logging.MessageNames.TOPOLOGY

    override def write(topology: NetworkTopology): JsValue = JsObject(
      TYPE -> JsString(TOPOLOGY.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(topology.neuronId),
        LOCATION -> topology.location.toJson
      )
    )

    override def read(value: JsValue): NetworkTopology = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (TOPOLOGY.name, neuron) => neuron.asJsObject.getFields(NEURON_ID, LOCATION) match {
          case Seq(JsString(neuronId), location) => NetworkTopology(neuronId, location.convertTo[Cartesian])
          case _ => deserializationError(s"($NEURON_ID, $LOCATION) expected")
        }
        case _ => deserializationError(s"(type: ${TOPOLOGY.name}, ...) message expected")
      }
      case _ => deserializationError("NetworkTopology expected")
    }
  }

  /**
    * Post-synaptic neuron connected
    */
  implicit object ConnectedPostSynapticJsonFormat extends RootJsonFormat[ConnectedPostSynaptic] {

    import com.digitalcipher.spiked.logging.MessageNames.CONNECT

    override def write(connection: ConnectedPostSynaptic): JsValue = JsObject(
      TYPE -> JsString(CONNECT.name),
      MESSAGE -> JsObject(
        PRE_SYNAPTIC -> JsString(connection.preSynapticId),
        POST_SYNAPTIC -> JsString(connection.postSynapticId),
        SIGNAL_DELAY -> connection.signalDelay.toJson
      ))

    override def read(value: JsValue): ConnectedPostSynaptic = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (CONNECT.name, connection) => connection.asJsObject.getFields(PRE_SYNAPTIC, POST_SYNAPTIC, SIGNAL_DELAY) match {
          case Seq(JsString(preSynapticId), JsString(postSynapticId), JsNumber(signalDelay)) =>
            ConnectedPostSynaptic(
              preSynapticId = preSynapticId,
              postSynapticId = postSynapticId,
              signalDelay = Milliseconds(signalDelay)
            )
          case _ => deserializationError(s"($PRE_SYNAPTIC, $POST_SYNAPTIC, $SIGNAL_DELAY) expected")
        }
        case _ => deserializationError(s"(type: ${CONNECT.name}, ...) message expected")
      }
      case _ => deserializationError("ConnectedPostSynaptic expected")
    }
  }

  /**
    * STDP hard-limit learning
    */
  implicit object StdpHardLimitLearningFunctionJsonFormat extends RootJsonFormat[StdpHardLimitLearningFunction] {

    import com.digitalcipher.spiked.logging.MessageNames.LEARNING

    override def write(learning: StdpHardLimitLearningFunction): JsValue = JsObject(
      TYPE -> JsString(LEARNING.name),
      MESSAGE -> JsObject(
        INHIBITORY_AMPLITUDE -> JsNumber(learning.inhibitionAmplitude),
        INHIBITORY_PERIOD -> learning.inhibitionPeriod.toJson,
        EXCITATION_AMPLITUDE -> JsNumber(learning.excitationAmplitude),
        EXCITATION_PERIOD -> learning.excitationPeriod.toJson
      ))

    override def read(value: JsValue): StdpHardLimitLearningFunction = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (LEARNING.name, learning) => learning.asJsObject.getFields(INHIBITORY_AMPLITUDE, INHIBITORY_PERIOD, EXCITATION_AMPLITUDE, EXCITATION_PERIOD) match {
          case Seq(neuron) => neuron.asJsObject.getFields(INHIBITORY_AMPLITUDE, INHIBITORY_PERIOD, EXCITATION_AMPLITUDE, EXCITATION_PERIOD) match {
            case Seq(JsNumber(inhibitAmplitude), inhibitPeriod, JsNumber(exciteAmplitude), excitePeriod) =>
              StdpHardLimitLearningFunction(
                inhibitAmplitude.doubleValue(),
                inhibitPeriod.convertTo[Time],
                exciteAmplitude.doubleValue(),
                excitePeriod.convertTo[Time]
              )
            case _ => deserializationError(s"($INHIBITORY_AMPLITUDE, $INHIBITORY_PERIOD, $EXCITATION_AMPLITUDE, $EXCITATION_PERIOD) expected")
          }
          case _ => deserializationError(s"(type: ${LEARNING.name}, ...) message expected")
        }
        case _ => deserializationError("StdpHardLimitLearningFunction expected")
      }
    }
  }

  /**
    * STDP soft-limit learning
    */
  implicit object StdpSoftLimitLearningFunctionJsonFormat extends RootJsonFormat[StdpSoftLimitLearningFunction] {

    import com.digitalcipher.spiked.logging.MessageNames.LEARNING

    override def write(learning: StdpSoftLimitLearningFunction): JsValue = JsObject(
      TYPE -> JsString(LEARNING.name),
      MESSAGE -> JsObject(
        INHIBITORY_AMPLITUDE -> JsNumber(learning.inhibitionAmplitude),
        INHIBITORY_PERIOD -> learning.inhibitionPeriod.toJson,
        EXCITATION_AMPLITUDE -> JsNumber(learning.excitationAmplitude),
        EXCITATION_PERIOD -> learning.excitationPeriod.toJson
      ))

    override def read(value: JsValue): StdpSoftLimitLearningFunction = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (LEARNING.name, learning) => learning.asJsObject.getFields(INHIBITORY_AMPLITUDE, INHIBITORY_PERIOD, EXCITATION_AMPLITUDE, EXCITATION_PERIOD) match {
          case Seq(neuron) => neuron.asJsObject.getFields(INHIBITORY_AMPLITUDE, INHIBITORY_PERIOD, EXCITATION_AMPLITUDE, EXCITATION_PERIOD) match {
            case Seq(JsNumber(inhibitAmplitude), inhibitPeriod, JsNumber(exciteAmplitude), excitePeriod) =>
              StdpSoftLimitLearningFunction(
                inhibitAmplitude.doubleValue(),
                inhibitPeriod.convertTo[Time],
                exciteAmplitude.doubleValue(),
                excitePeriod.convertTo[Time]
              )
            case _ => deserializationError(s"($INHIBITORY_AMPLITUDE, $INHIBITORY_PERIOD, $EXCITATION_AMPLITUDE, $EXCITATION_PERIOD) expected")
          }
          case _ => deserializationError(s"(type: ${LEARNING.name}, ...) message expected")
        }
        case _ => deserializationError("StdpSoftLimitLearningFunction expected")
      }
    }
  }

  /**
    * STDP alpha learning
    */
  implicit object StdpAlphaLearningFunctionJsonFormat extends RootJsonFormat[StdpAlphaLearningFunction] {

    import com.digitalcipher.spiked.logging.MessageNames.LEARNING

    override def write(learning: StdpAlphaLearningFunction): JsValue = JsObject(
      TYPE -> JsString(LEARNING.name),
      MESSAGE -> JsObject(
        BASELINE -> JsNumber(learning.baseline),
        LEARNING_RATE -> JsNumber(learning.learningRate),
        TIME_CONSTANT -> learning.timeConstant.toJson
      ))

    override def read(value: JsValue): StdpAlphaLearningFunction = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (LEARNING.name, learning) => learning.asJsObject.getFields(BASELINE, LEARNING_RATE, TIME_CONSTANT) match {
          case Seq(neuron) => neuron.asJsObject.getFields(BASELINE, LEARNING_RATE, TIME_CONSTANT) match {
            case Seq(JsNumber(baseline), JsNumber(learningRate), timeConstant) =>
              StdpAlphaLearningFunction(
                baseline.doubleValue(),
                timeConstant.convertTo[Time],
                learningRate.doubleValue()
              )
            case _ => deserializationError(s"($BASELINE, $LEARNING_RATE, $TIME_CONSTANT) expected")
          }
          case _ => deserializationError(s"(type: ${LEARNING.name}, ...) message expected")
        }
        case _ => deserializationError("StdpAlphaLearningFunction expected")
      }
    }
  }

  /**
    * Neuron connection
    */
  implicit object NeuronConnectionJsonFormat extends RootJsonFormat[NetworkConnected] {

    import com.digitalcipher.spiked.logging.MessageNames.CONNECT

    override def write(connected: NetworkConnected): JsValue = JsObject(
      TYPE -> JsString(CONNECT.name),
      MESSAGE -> JsObject(
        PRE_SYNAPTIC -> JsString(connected.preSynapticId),
        POST_SYNAPTIC -> JsString(connected.postSynapticId),
        INITIAL_WEIGHT -> JsNumber(connected.initialWeight),
        EQUILIBRIUM_WEIGHT -> JsNumber(connected.equilibriumWeight),
        PRE_SYNAPTIC_LOCATION -> connected.preSynapticLocation.toJson,
        POST_SYNAPTIC_LOCATION -> connected.postSynapticLocation.toJson,
        DISTANCE -> JsNumber(connected.distance.toMicrons)
      ))

    override def read(value: JsValue): NetworkConnected = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (CONNECT.name, connection) => connection.asJsObject.getFields(PRE_SYNAPTIC, POST_SYNAPTIC, INITIAL_WEIGHT, EQUILIBRIUM_WEIGHT, PRE_SYNAPTIC_LOCATION, POST_SYNAPTIC_LOCATION, DISTANCE) match {
          case Seq(connection) => connection.asJsObject.getFields(PRE_SYNAPTIC, POST_SYNAPTIC, INITIAL_WEIGHT, EQUILIBRIUM_WEIGHT, PRE_SYNAPTIC_LOCATION, POST_SYNAPTIC_LOCATION, DISTANCE) match {
            case Seq(JsString(preSynapticId), JsString(postSynapticId), JsNumber(initialWeight), JsNumber(equilibriumWeight), preSynapticLocation, postSynapticLocation, JsNumber(distance)) => NetworkConnected(
              preSynapticId = preSynapticId,
              postSynapticId = postSynapticId,
              initialWeight = initialWeight.doubleValue(),
              equilibriumWeight = equilibriumWeight.doubleValue(),
              preSynapticLocation = preSynapticLocation.convertTo[Cartesian],
              postSynapticLocation = postSynapticLocation.convertTo[Cartesian],
              distance = Microns(distance.doubleValue())
            )
            case _ => deserializationError(s"($PRE_SYNAPTIC, $POST_SYNAPTIC, $INITIAL_WEIGHT, $EQUILIBRIUM_WEIGHT, $PRE_SYNAPTIC_LOCATION, $POST_SYNAPTIC_LOCATION, $DISTANCE) expected")
          }
          case _ => deserializationError(s"(type: ${CONNECT.name}, ...) message expected")
        }
        case _ => deserializationError("NetworkConnected expected")
      }
    }
  }

  /**
    * Registration of the pre-synaptic neuron with the post-synaptic neuron for STDP and plasticity
    */
  implicit object PreSynapticRegistrationJsonFormat extends RootJsonFormat[PreSynapticRegistration] {

    import com.digitalcipher.spiked.logging.MessageNames.REGISTER

    override def write(registration: PreSynapticRegistration): JsValue = JsObject(
      TYPE -> JsString(REGISTER.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(registration.neuronId),
        PRE_SYNAPTIC -> JsString(registration.preSynapticId),
        INITIAL_WEIGHT -> JsNumber(registration.weight)
      ))

    override def read(value: JsValue): PreSynapticRegistration = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (REGISTER.name, registration) => registration.asJsObject.getFields(NEURON_ID, PRE_SYNAPTIC, INITIAL_WEIGHT) match {
          case Seq(JsString(neuronId), JsString(preSynapticId), JsNumber(weight)) =>
            PreSynapticRegistration(neuronId, preSynapticId, weight.doubleValue())
          case _ => deserializationError(s"($NEURON_ID, $PRE_SYNAPTIC, $INITIAL_WEIGHT) expected")
        }
        case _ => deserializationError(s"(type: ${REGISTER.name}, ...) message expected")
      }
      case _ => deserializationError("PreSynapticRegistration expected")
    }
  }

  /**
    * Connection weights updated from STDP learning
    */
  implicit object StdpWeightUpdatedJsonFormat extends RootJsonFormat[StdpWeightUpdated] {

    import com.digitalcipher.spiked.logging.MessageNames.WEIGHT_UPDATE

    override def write(updated: StdpWeightUpdated): JsValue = JsObject(
      TYPE -> JsString(WEIGHT_UPDATE.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(updated.neuronId),
        SOURCE_ID -> JsString(updated.sourceId),
        PREVIOUS_WEIGHT -> JsNumber(updated.previousWeight),
        NEW_WEIGHT -> JsNumber(updated.newWeight),
        ADJUSTMENT -> JsNumber(updated.adjustment),
        TIME_WINDOW -> updated.timeWindow.toJson,
        STDP_TIME -> updated.stdpTime.toJson,
        SIGNAL_TIME -> updated.timeWindow.toJson
      ))

    override def read(value: JsValue): StdpWeightUpdated = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (WEIGHT_UPDATE.name, update) => update.asJsObject.getFields(NEURON_ID, SOURCE_ID, PREVIOUS_WEIGHT, NEW_WEIGHT, ADJUSTMENT, TIME_WINDOW, STDP_TIME, SIGNAL_TIME) match {
          case Seq(updated) => updated.asJsObject.getFields(NEURON_ID, SOURCE_ID, PREVIOUS_WEIGHT, NEW_WEIGHT, ADJUSTMENT, TIME_WINDOW, STDP_TIME, SIGNAL_TIME) match {
            case Seq(JsString(neuronId), JsString(sourceId), JsNumber(previousWeight), JsNumber(newWeight), JsNumber(adjustment), timeWindow, stdpTime, signalTime) =>
              StdpWeightUpdated(
                neuronId = neuronId,
                sourceId = sourceId,
                previousWeight = previousWeight.doubleValue(),
                newWeight = newWeight.doubleValue(),
                adjustment = adjustment.doubleValue(),
                timeWindow = timeWindow.convertTo[Time],
                stdpTime = stdpTime.convertTo[Time],
                signalTime = signalTime.convertTo[Time]
              )
            case _ => deserializationError(s"($NEURON_ID, $SOURCE_ID, $PREVIOUS_WEIGHT, $NEW_WEIGHT, $ADJUSTMENT, $TIME_WINDOW, $STDP_TIME, $SIGNAL_TIME) expected")
          }
          case _ => deserializationError(s"(type: ${WEIGHT_UPDATE.name}, ...) message expected")
        }
        case _ => deserializationError("StdpWeightUpdated expected")
      }
    }
  }

  /**
    * Intrinsic plasticity model of learning and weight updates
    */
  implicit object IntrinsicPlasticityUpdatedJsonFormat extends RootJsonFormat[IntrinsicPlasticityUpdated] {

    import com.digitalcipher.spiked.logging.MessageNames.INTRINSIC_PLASTICITY_UPDATE

    override def write(plasticity: IntrinsicPlasticityUpdated): JsValue = JsObject(
      TYPE -> JsString(INTRINSIC_PLASTICITY_UPDATE.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(plasticity.neuronId),
        TIMESTAMP -> plasticity.timestamp.toJson,
        PLASTICITY -> plasticity.intrinsicPlasticity.toJson
      ))

    override def read(value: JsValue): IntrinsicPlasticityUpdated = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (INTRINSIC_PLASTICITY_UPDATE.name, update) => update.asJsObject.getFields(NEURON_ID, TIMESTAMP, PLASTICITY) match {
          case Seq(JsString(neuronId), JsNumber(timestamp), JsNumber(plasticity)) =>
            IntrinsicPlasticityUpdated(
              neuronId = neuronId,
              timestamp = Milliseconds(timestamp),
              intrinsicPlasticity = Millivolts(plasticity)
            )
          case _ => deserializationError(s"($NEURON_ID, $TIMESTAMP, $PLASTICITY)")
        }
        case _ => deserializationError(s"(type: ${INTRINSIC_PLASTICITY_UPDATE.name}, ...) message expected")
      }
      case _ => deserializationError("IntrinsicPlasticityUpdated expected")
    }
  }

  /**
    * A signal was received from a pre-synaptic neurons
    */
  implicit object SignalReceivedJsonFormat extends RootJsonFormat[SignalReceived] {

    import com.digitalcipher.spiked.logging.MessageNames.SIGNAL_RECEIVED

    override def write(signal: SignalReceived): JsValue = JsObject(
      TYPE -> JsString(SIGNAL_RECEIVED.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(signal.neuronId),
        SOURCE_ID -> JsString(signal.sourceId),
        TIMESTAMP -> signal.timestamp.toJson,
        LAST_EVENT_TIME -> signal.lastEventTime.toJson,
        LAST_FIRE_TIME -> signal.lastFireTime.toJson,
        SIGNAL_INTENSITY -> signal.signalIntensity.toJson
      ))

    override def read(value: JsValue): SignalReceived = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (SIGNAL_RECEIVED.name, signal) => signal.asJsObject.getFields(NEURON_ID, SOURCE_ID, TIMESTAMP, LAST_EVENT_TIME, LAST_FIRE_TIME, SIGNAL_INTENSITY) match {
          case Seq(signal) => signal.asJsObject.getFields(NEURON_ID, SOURCE_ID, TIMESTAMP, LAST_EVENT_TIME, LAST_FIRE_TIME, SIGNAL_INTENSITY) match {
            case Seq(JsString(neuronId), JsString(sourceId), timestamp, lastEventTime, lastFireTime, signalIntensity) =>
              SignalReceived(
                neuronId = neuronId,
                sourceId = sourceId,
                timestamp = timestamp.convertTo[Time],
                lastEventTime = lastEventTime.convertTo[Time],
                lastFireTime = lastFireTime.convertTo[Time],
                signalIntensity = signalIntensity.convertTo[ElectricPotential]
              )
            case _ => deserializationError(s"($NEURON_ID, $SOURCE_ID, $TIMESTAMP, $LAST_EVENT_TIME, $LAST_FIRE_TIME, $SIGNAL_INTENSITY) expected")
          }
          case _ => deserializationError(s"(type: ${SIGNAL_RECEIVED.name}, ...) message expected")
        }
        case _ => deserializationError("SignalReceived expected")
      }
    }
  }

  /**
    * An update to the membrane potential
    */
  implicit object MembranePotentialUpdateJsonFormat extends RootJsonFormat[MembranePotentialUpdate] {

    import com.digitalcipher.spiked.logging.MessageNames.MEMBRANE_POTENTIAL_UPDATE

    override def write(update: MembranePotentialUpdate): JsValue = JsObject(
      TYPE -> JsString(MEMBRANE_POTENTIAL_UPDATE.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(update.neuronId),
        TIMESTAMP -> update.timestamp.toJson,
        LAST_EVENT_TIME -> update.lastEventTime.toJson,
        LAST_FIRE_TIME -> update.lastFireTime.toJson,
        MEMBRANE_POTENTIAL -> update.membranePotential.toJson
      ))

    override def read(value: JsValue): MembranePotentialUpdate = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (MEMBRANE_POTENTIAL_UPDATE.name, neuron) => neuron.asJsObject.getFields(NEURON_ID, TIMESTAMP, LAST_EVENT_TIME, LAST_FIRE_TIME, MEMBRANE_POTENTIAL) match {
          case Seq(signal) => signal.asJsObject.getFields() match {
            case Seq(JsString(neuronId), timestamp, lastEventTime, lastFireTime, membranePotential) =>
              MembranePotentialUpdate(
                neuronId = neuronId,
                timestamp = timestamp.convertTo[Time],
                lastEventTime = lastEventTime.convertTo[Time],
                lastFireTime = lastFireTime.convertTo[Time],
                membranePotential = membranePotential.convertTo[ElectricPotential]
              )
            case _ => deserializationError(s"($NEURON_ID, $TIMESTAMP, $LAST_EVENT_TIME, $LAST_FIRE_TIME, $MEMBRANE_POTENTIAL) expected")
          }
          case _ => deserializationError(s"(type: ${MEMBRANE_POTENTIAL_UPDATE.name}, ...) message expected")
        }
        case _ => deserializationError("MembranePotentialUpdate expected")
      }
    }
  }

  /**
    * When a neuron fires a spike
    */
  implicit object SpikedJsonFormat extends RootJsonFormat[Spiked] {

    import com.digitalcipher.spiked.logging.MessageNames.SPIKED

    override def write(spike: Spiked): JsValue = JsObject(
      TYPE -> JsString(SPIKED.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(spike.neuronId),
        TIMESTAMP -> spike.timestamp.toJson,
        LAST_FIRE_TIME -> spike.lastFireTime.toJson,
        SIGNAL_INTENSITY -> spike.signalIntensity.toJson
      ))

    override def read(value: JsValue): Spiked = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (SPIKED.name, neuron) => neuron.asJsObject.getFields(NEURON_ID, TIMESTAMP, LAST_FIRE_TIME, SIGNAL_INTENSITY) match {
          case Seq(spike) => spike.asJsObject.getFields(NEURON_ID, TIMESTAMP, LAST_FIRE_TIME, SIGNAL_INTENSITY) match {
            case Seq(JsString(neuronId), timestamp, lastFireTime, signalIntensity) => Spiked(
              neuronId = neuronId,
              timestamp = timestamp.convertTo[Time],
              lastFireTime = lastFireTime.convertTo[Time],
              signalIntensity = signalIntensity.convertTo[ElectricPotential]
            )
            case _ => deserializationError(s"($NEURON_ID, $TIMESTAMP, $LAST_FIRE_TIME, $SIGNAL_INTENSITY) expected")
          }
        }
        case _ => deserializationError(s"(type: ${SPIKED.name}, ...) message expected")
      }
      case _ => deserializationError("Spiked expected")
    }
  }

  /**
    * When a bistable neuron has a phase transition
    */
  implicit object PhaseTransitionJsonFormat extends RootJsonFormat[PhaseTransition] {

    import com.digitalcipher.spiked.logging.MessageNames.PHASE_TRANSITION

    override def write(transition: PhaseTransition): JsValue = JsObject(
      TYPE -> JsString(PHASE_TRANSITION.name),
      MESSAGE -> JsObject(
        NEURON_ID -> JsString(transition.neuronId),
        TIMESTAMP -> transition.timestamp.toJson,
        TRANSITION_TYPE -> JsString(transition.transitionType),
        MEMBRANE_POTENTIAL -> transition.membranePotential.toJson,
        FIRING_RATE -> transition.firingRate.toJson
      ))

    override def read(value: JsValue): PhaseTransition = value.asJsObject.getFields(TYPE, MESSAGE) match {
      case Seq(JsString(messageType), message) => (messageType, message) match {
        case (PHASE_TRANSITION.name, neuron) => neuron.asJsObject.getFields(NEURON_ID, TIMESTAMP, TRANSITION_TYPE, MEMBRANE_POTENTIAL, FIRING_RATE) match {
          case Seq(transition) => transition.asJsObject.getFields(NEURON_ID, TIMESTAMP, TRANSITION_TYPE, MEMBRANE_POTENTIAL, FIRING_RATE) match {
            case Seq(JsString(neuronId), timestamp, JsString(transitionType), membranePotential, firingRate) =>
              PhaseTransition(
                neuronId = neuronId,
                timestamp = timestamp.convertTo[Time],
                transitionType = transitionType,
                membranePotential = membranePotential.convertTo[ElectricPotential],
                firingRate = firingRate.convertTo[Frequency]
              )
            case _ => deserializationError(s"($NEURON_ID, $TIMESTAMP, $TRANSITION_TYPE, $MEMBRANE_POTENTIAL, $FIRING_RATE) expected")
          }
          case _ => deserializationError(s"(type: ${PHASE_TRANSITION.name}, ...) message expected")
        }
        case _ => deserializationError("PhaseTransition expected")
      }
    }
  }
}
