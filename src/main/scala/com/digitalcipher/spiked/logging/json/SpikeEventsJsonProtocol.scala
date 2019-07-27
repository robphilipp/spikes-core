package com.digitalcipher.spiked.logging.json

import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.topology.NeuronType
import com.digitalcipher.spiked.topology.coords.spatial.Points
import com.digitalcipher.spiked.topology.coords.spatial.Points.Cartesian
import spray.json.DefaultJsonProtocol
import squants.Time
import squants.electro.{ElectricPotential, Microvolts, Millivolts, Volts}
import squants.space.Microns
import squants.time._

/**
  * JSON converter objects for classes that need to be converted to JSON
  */
object SpikeEventsJsonProtocol extends DefaultJsonProtocol {

  private val MESSAGE: String = "message"
  private val TYPE: String = "type"

  private val NEURON_ID = "neuron_id"
  private val LOCATION = "location"
  private val PRE_SYNAPTIC = "pre_synaptic"
  private val POST_SYNAPTIC = "post_synaptic"
  private val SIGNAL_DELAY = "signal_delay"
  private val INHIBITORY_AMPLITUDE = "inhibitory_amplitude"
  private val INHIBITORY_PERIOD = "inhibitory_period"
  private val EXCITATION_AMPLITUDE = "excitation_amplitude"
  private val EXCITATION_PERIOD = "excitation_period"
  private val BASELINE = "baseline"
  private val LEARNING_RATE = "learning_rate"
  private val TIME_CONSTANT = "time_constant"
  private val INITIAL_WEIGHT = "initial_weight"
  private val EQUILIBRIUM_WEIGHT = "equilibrium_weight"
  private val PRE_SYNAPTIC_LOCATION = "pre_synaptic_location"
  private val POST_SYNAPTIC_LOCATION = "post_synaptic_location"
  private val DISTANCE = "distance"


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
      "z" -> JsNumber(point._3.toMicrons)
    )

    override def read(value: JsValue): Cartesian = value.asJsObject.getFields("x", "y", "z") match {
      case Seq(JsNumber(x), JsNumber(y), JsNumber(z)) => Cartesian((Microns(x), Microns(y), Microns(z)))
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

  implicit object PreSynapticRegistrationJsonFormat extends RootJsonFormat[PreSynapticRegistration] {

    import com.digitalcipher.spiked.logging.MessageNames.REGISTER

    override def write(registration: PreSynapticRegistration): JsValue = JsObject(REGISTER.name -> JsObject(
      "neuron_id" -> JsString(registration.neuronId),
      "pre_synaptic" -> JsString(registration.preSynapticId),
      "initial_weight" -> JsNumber(registration.weight)
    ))

    override def read(value: JsValue): PreSynapticRegistration = value.asJsObject.getFields(REGISTER.name) match {
      case Seq(connection) => connection.asJsObject.getFields("neuron_id", "pre_synaptic", "initial_weight") match {
        case Seq(JsString(neuronId), JsString(preSynapticId), JsNumber(weight)) =>
          PreSynapticRegistration(neuronId, preSynapticId, weight.doubleValue())
        case _ => deserializationError("(neuron_id, pre_synaptic, initial_weight) expected")
      }
      case _ => deserializationError("PreSynapticRegistration expected")
    }
  }

  implicit object StdpWeightUpdatedJsonFormat extends RootJsonFormat[StdpWeightUpdated] {

    import com.digitalcipher.spiked.logging.MessageNames.WEIGHT_UPDATE

    override def write(updated: StdpWeightUpdated): JsValue = JsObject(WEIGHT_UPDATE.name -> JsObject(
      "neuron_id" -> JsString(updated.neuronId),
      "source_id" -> JsString(updated.sourceId),
      "previous_weight" -> JsNumber(updated.previousWeight),
      "new_weight" -> JsNumber(updated.newWeight),
      "adjustment" -> JsNumber(updated.adjustment),
      "time_window" -> updated.timeWindow.toJson,
      "stdp_time" -> updated.stdpTime.toJson,
      "signal_time" -> updated.timeWindow.toJson
    ))

    override def read(value: JsValue): StdpWeightUpdated = value.asJsObject.getFields(WEIGHT_UPDATE.name) match {
      case Seq(updated) => updated.asJsObject.getFields(
        "neuron_id",
        "source_id",
        "previous_weight",
        "new_weight",
        "adjustment",
        "time_window",
        "stdp_time",
        "signal_time"
      ) match {
        case Seq(
        JsString(neuronId),
        JsString(sourceId),
        JsNumber(previousWeight),
        JsNumber(newWeight),
        JsNumber(adjustment),
        timeWindow,
        stdpTime,
        signalTime
        ) =>
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
        case _ => deserializationError("(neuron_id, source_id, previous_weight, new_weight, adjustment, time_window, previous_weight, stdp_time, signal_time) expected")
      }
      case _ => deserializationError("StdpWeightUpdated expected")
    }
  }

  implicit object IntrinsicPlasticityUpdatedJsonFormat extends RootJsonFormat[IntrinsicPlasticityUpdated] {

    import com.digitalcipher.spiked.logging.MessageNames.INTRINSIC_PLASTICITY_UPDATE

    override def write(plasticity: IntrinsicPlasticityUpdated): JsValue = JsObject(INTRINSIC_PLASTICITY_UPDATE.name -> JsObject(
      "neuron_id" -> JsString(plasticity.neuronId),
      "timestamp" -> plasticity.timestamp.toJson,
      "plasticity" -> plasticity.intrinsicPlasticity.toJson
    ))

    override def read(value: JsValue): IntrinsicPlasticityUpdated = {
      value.asJsObject.getFields("neuron_id", "timestamp", "plasticity") match {
        case Seq(JsString(neuronId), JsNumber(timestamp), JsNumber(plasticity)) =>
          IntrinsicPlasticityUpdated(
            neuronId = neuronId,
            timestamp = Milliseconds(timestamp),
            intrinsicPlasticity = Millivolts(plasticity)
          )
        case _ => deserializationError("(neuron_id, timestamp, plasticity)")
      }
    }
  }

  implicit object SignalReceivedJsonFormat extends RootJsonFormat[SignalReceived] {

    import com.digitalcipher.spiked.logging.MessageNames.SIGNAL_RECEIVED

    override def write(signal: SignalReceived): JsValue = JsObject(SIGNAL_RECEIVED.name -> JsObject(
      "neuron_id" -> JsString(signal.neuronId),
      "source_id" -> JsString(signal.sourceId),
      "timestamp" -> signal.timestamp.toJson,
      "last_event_time" -> signal.lastEventTime.toJson,
      "last_fire_time" -> signal.lastFireTime.toJson,
      "signal_intensity" -> signal.signalIntensity.toJson
    ))

    override def read(value: JsValue): SignalReceived = value.asJsObject.getFields(SIGNAL_RECEIVED.name) match {
      case Seq(signal) => signal.asJsObject.getFields(
        "neuron_id",
        "source_id",
        "timestamp",
        "last_event_time",
        "last_fire_time",
        "signal_intensity"
      ) match {
        case Seq(
        JsString(neuronId),
        JsString(sourceId),
        timestamp,
        lastEventTime,
        lastFireTime,
        signalIntensity
        ) =>
          SignalReceived(
            neuronId = neuronId,
            sourceId = sourceId,
            timestamp = timestamp.convertTo[Time],
            lastEventTime = lastEventTime.convertTo[Time],
            lastFireTime = lastFireTime.convertTo[Time],
            signalIntensity = signalIntensity.convertTo[ElectricPotential]
          )
        case _ => deserializationError("(neuron_id, source_id, timestamp, last_event_time, last_fire_time, signal_intensity")
      }
      case _ => deserializationError("SignalReceived expected")
    }
  }

  implicit object MembranePotentialUpdateJsonFormat extends RootJsonFormat[MembranePotentialUpdate] {

    import com.digitalcipher.spiked.logging.MessageNames.MEMBRANE_POTENTIAL_UPDATE

    override def write(update: MembranePotentialUpdate): JsValue = JsObject(MEMBRANE_POTENTIAL_UPDATE.name -> JsObject(
      "neuron_id" -> JsString(update.neuronId),
      "timestamp" -> update.timestamp.toJson,
      "last_event_time" -> update.lastEventTime.toJson,
      "last_fire_time" -> update.lastFireTime.toJson,
      "membrane_potential" -> update.membranePotential.toJson
    ))

    override def read(value: JsValue): MembranePotentialUpdate = value.asJsObject.getFields(MEMBRANE_POTENTIAL_UPDATE.name) match {
      case Seq(signal) => signal.asJsObject.getFields(
        "neuron_id",
        "timestamp",
        "last_event_time",
        "last_fire_time",
        "membrane_potential"
      ) match {
        case Seq(
        JsString(neuronId),
        timestamp,
        lastEventTime,
        lastFireTime,
        membranePotential
        ) =>
          MembranePotentialUpdate(
            neuronId = neuronId,
            timestamp = timestamp.convertTo[Time],
            lastEventTime = lastEventTime.convertTo[Time],
            lastFireTime = lastFireTime.convertTo[Time],
            membranePotential = membranePotential.convertTo[ElectricPotential]
          )
        case _ => deserializationError("(neuron_id, timestamp, last_event_time, last_fire_time, membrane_potential) expected")
      }
      case _ => deserializationError("MembranePotentialUpdate expected")
    }
  }

  implicit object SpikedJsonFormat extends RootJsonFormat[Spiked] {

    import com.digitalcipher.spiked.logging.MessageNames.SPIKED

    override def write(spike: Spiked): JsValue = JsObject(SPIKED.name -> JsObject(
      "neuron_id" -> JsString(spike.neuronId),
      "timestamp" -> spike.timestamp.toJson,
      "last_fire_time" -> spike.lastFireTime.toJson,
      "signal_intensity" -> spike.signalIntensity.toJson
    ))

    override def read(value: JsValue): Spiked = value.asJsObject.getFields(SPIKED.name) match {
      case Seq(spike) => spike.asJsObject.getFields("neuron_id", "timestamp", "last_fire_time", "signal_intensity") match {
        case Seq(JsString(neuronId), timestamp, lastFireTime, signalIntensity) => Spiked(
          neuronId = neuronId,
          timestamp = timestamp.convertTo[Time],
          lastFireTime = lastFireTime.convertTo[Time],
          signalIntensity = signalIntensity.convertTo[ElectricPotential]
        )
        case _ => deserializationError("(neuron_id, timestamp, last_fire_time, signal_intensity) expected")
      }
      case _ => deserializationError("Spiked expected")
    }
  }

  implicit object PhaseTransitionJsonFormat extends RootJsonFormat[PhaseTransition] {

    import com.digitalcipher.spiked.logging.MessageNames.PHASE_TRANSITION

    override def write(transition: PhaseTransition): JsValue = JsObject(PHASE_TRANSITION.name -> JsObject(
      "neuron_id" -> JsString(transition.neuronId),
      "timestamp" -> transition.timestamp.toJson,
      "transition_type" -> JsString(transition.transitionType),
      "membrane_potential" -> transition.membranePotential.toJson,
      "firing_rate" -> transition.firingRate.toJson
    ))

    override def read(value: JsValue): PhaseTransition = value.asJsObject.getFields(PHASE_TRANSITION.name) match {
      case Seq(transition) => transition.asJsObject.getFields(
        "neuron_id",
        "timestamp",
        "transition_type",
        "membrane_potential",
        "firing_rate"
      ) match {
        case Seq(JsString(neuronId), timestamp, JsString(transitionType), membranePotential, firingRate) => PhaseTransition(
          neuronId = neuronId,
          timestamp = timestamp.convertTo[Time],
          transitionType = transitionType,
          membranePotential = membranePotential.convertTo[ElectricPotential],
          firingRate = firingRate.convertTo[Frequency]
        )
        case _ => deserializationError("(neuron_id, timestamp, transition_type, membrane_potential, firing_rate) expected")
      }
      case _ => deserializationError("PhaseTransition expected")
    }
  }

}
