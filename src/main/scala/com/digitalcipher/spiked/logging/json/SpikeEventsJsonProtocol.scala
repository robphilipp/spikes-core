package com.digitalcipher.spiked.logging.json

import akka.actor.ActorRef
import com.digitalcipher.spiked.construction.description.LearningFunctionDescription
import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.neurons.Neuron.Connect
import com.digitalcipher.spiked.topology.NeuronType
import com.digitalcipher.spiked.topology.coords.spatial.Points
import com.digitalcipher.spiked.topology.coords.spatial.Points.Cartesian
import spray.json.DefaultJsonProtocol
import squants.Time
import squants.electro.{ElectricPotential, Microvolts, Millivolts, Volts}
import squants.space.{Length, Microns}
import squants.time._

/**
  * JSON converter objects for classes that need to be converted to JSON
  */
object SpikeEventsJsonProtocol extends DefaultJsonProtocol {

  import spray.json._

  implicit object NetworkSummaryJsonFormat extends RootJsonFormat[NetworkSummary] {

    import com.digitalcipher.spiked.logging.MessageNames.SUMMARY

    private implicit val jsonFormat: JsonFormat[NeuronType.Value] = EnumJsonConverter.jsonEnum(NeuronType)

    override def write(summary: NetworkSummary) = JsObject(SUMMARY.name -> summary.counts.toJson)

    override def read(value: JsValue): NetworkSummary = value.asJsObject.getFields(SUMMARY.name) match {
      case Seq(JsObject(counts)) =>
        NetworkSummary(counts.map(entry => NeuronType.from(entry._1) -> entry._2.convertTo[Int]))
      case _ => deserializationError("NetworkSummary expected")
    }
  }

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

  implicit object NetworkTopologyJsonFormat extends RootJsonFormat[NetworkTopology] {

    import com.digitalcipher.spiked.logging.MessageNames.TOPOLOGY

    override def write(topology: NetworkTopology): JsValue = JsObject(TOPOLOGY.name -> JsObject(
      "neuron_id" -> JsString(topology.neuronId),
      "location" -> topology.location.toJson
    ))

    override def read(value: JsValue): NetworkTopology = value.asJsObject.getFields(TOPOLOGY.name) match {
      case Seq(neuron) => neuron.asJsObject.getFields("neuron_id", "location") match {
        case Seq(JsString(neuronId), location) => NetworkTopology(neuronId, location.convertTo[Cartesian])
        case _ => deserializationError("(neuron ID, location) expected")
      }
      case _ => deserializationError("NetworkTopology expected")
    }
  }

  implicit object ConnectedPostSynapticJsonFormat extends RootJsonFormat[ConnectedPostSynaptic] {

    import com.digitalcipher.spiked.logging.MessageNames.CONNECT

    override def write(connection: ConnectedPostSynaptic): JsValue = JsObject(CONNECT.name -> JsObject(
      "pre_synaptic" -> JsString(connection.preSynapticId),
      "post_synaptic" -> JsString(connection.postSynapticId),
      "signal_delay" -> connection.signalDelay.toJson
    ))

    override def read(value: JsValue): ConnectedPostSynaptic = {
      value.asJsObject.getFields("pre_synaptic", "post_synaptic", "signal_delay") match {
        case Seq(JsString(preSynapticId), JsString(postSynapticId), JsNumber(signalDelay)) =>
          ConnectedPostSynaptic(
            preSynapticId = preSynapticId,
            postSynapticId = postSynapticId,
            signalDelay = Milliseconds(signalDelay)
          )
        case _ => deserializationError("(neuron_id, timestamp, plasticity)")
      }
    }
  }

  implicit object StdpHardLimitLearningFunctionJsonFormat extends RootJsonFormat[StdpHardLimitLearningFunction] {

    import com.digitalcipher.spiked.logging.MessageNames.LEARNING

    override def write(learning: StdpHardLimitLearningFunction): JsValue = JsObject(LEARNING.name -> JsObject(
      "inhibitory_amplitude" -> JsNumber(learning.inhibitionAmplitude),
      "inhibitory_period" -> learning.inhibitionPeriod.toJson,
      "excitation_amplitude" -> JsNumber(learning.excitationAmplitude),
      "excitation_period" -> learning.excitationPeriod.toJson
    ))

    override def read(value: JsValue): StdpHardLimitLearningFunction = value.asJsObject.getFields(LEARNING.name) match {
      case Seq(neuron) => neuron.asJsObject.getFields(
        "inhibitory_amplitude",
        "inhibitory_period",
        "excitation_amplitude",
        "excitation_period"
      ) match {
        case Seq(JsNumber(inhibitAmplitude), inhibitPeriod, JsNumber(exciteAmplitude), excitePeriod) =>
          StdpHardLimitLearningFunction(
            inhibitAmplitude.doubleValue(),
            inhibitPeriod.convertTo[Time],
            exciteAmplitude.doubleValue(),
            excitePeriod.convertTo[Time]
          )
        case _ => deserializationError("(neuron ID, location) expected")
      }
      case _ => deserializationError("StdpHardLimitLearningFunction expected")
    }
  }

  implicit object StdpSoftLimitLearningFunctionJsonFormat extends RootJsonFormat[StdpSoftLimitLearningFunction] {

    import com.digitalcipher.spiked.logging.MessageNames.LEARNING

    override def write(learning: StdpSoftLimitLearningFunction): JsValue = JsObject(LEARNING.name -> JsObject(
      "inhibitory_amplitude" -> JsNumber(learning.inhibitionAmplitude),
      "inhibitory_period" -> learning.inhibitionPeriod.toJson,
      "excitation_amplitude" -> JsNumber(learning.excitationAmplitude),
      "excitation_period" -> learning.excitationPeriod.toJson
    ))

    override def read(value: JsValue): StdpSoftLimitLearningFunction = value.asJsObject.getFields(LEARNING.name) match {
      case Seq(neuron) => neuron.asJsObject.getFields(
        "inhibitory_amplitude",
        "inhibitory_period",
        "excitation_amplitude",
        "excitation_period"
      ) match {
        case Seq(JsNumber(inhibitAmplitude), inhibitPeriod, JsNumber(exciteAmplitude), excitePeriod) =>
          StdpSoftLimitLearningFunction(
            inhibitAmplitude.doubleValue(),
            inhibitPeriod.convertTo[Time],
            exciteAmplitude.doubleValue(),
            excitePeriod.convertTo[Time]
          )
        case _ => deserializationError("(neuron ID, location) expected")
      }
      case _ => deserializationError("StdpSoftLimitLearningFunction expected")
    }
  }

  implicit object StdpAlphaLearningFunctionJsonFormat extends RootJsonFormat[StdpAlphaLearningFunction] {

    import com.digitalcipher.spiked.logging.MessageNames.LEARNING

    override def write(learning: StdpAlphaLearningFunction): JsValue = JsObject(LEARNING.name -> JsObject(
      "baseline" -> JsNumber(learning.baseline),
      "learning_rate" -> JsNumber(learning.learningRate),
      "time_constant" -> learning.timeConstant.toJson
    ))

    override def read(value: JsValue): StdpAlphaLearningFunction = value.asJsObject.getFields(LEARNING.name) match {
      case Seq(neuron) => neuron.asJsObject.getFields(
        "baseline",
        "learning_rate",
        "time_constant"
      ) match {
        case Seq(JsNumber(baseline), JsNumber(learningRate), timeConstant) =>
          StdpAlphaLearningFunction(
            baseline.doubleValue(),
            timeConstant.convertTo[Time],
            learningRate.doubleValue()
          )
        case _ => deserializationError("(neuron ID, location) expected")
      }
      case _ => deserializationError("StdpAlphaLearningFunction expected")
    }
  }

  implicit object NeuronConnectionJsonFormat extends RootJsonFormat[NetworkConnected] {

    import com.digitalcipher.spiked.logging.MessageNames.CONNECT

    override def write(connected: NetworkConnected): JsValue = JsObject(CONNECT.name -> JsObject(
      "pre_synaptic" -> JsString(connected.preSynapticId),
      "post_synaptic" -> JsString(connected.postSynapticId),
      "initial_weight" -> JsNumber(connected.initialWeight),
      "equilibrium_weight" -> JsNumber(connected.equilibriumWeight),
      "pre_synaptic_location" -> connected.preSynapticLocation.toJson,
      "post_synaptic_location" -> connected.postSynapticLocation.toJson,
      "distance" -> JsNumber(connected.distance.toMicrons)
    ))

    override def read(value: JsValue): NetworkConnected = value.asJsObject.getFields(CONNECT.name) match {
      case Seq(connection) => connection.asJsObject.getFields(
        "pre_synaptic",
        "post_synaptic",
        "initial_weight",
        "equilibrium_weight",
        "pre_synaptic_location",
        "post_synaptic_location",
        "distance"
      ) match {
        case Seq(
        JsString(preSynapticId),
        JsString(postSynapticId),
        JsNumber(initialWeight),
        JsNumber(equilibriumWeight),
        preSynapticLocation,
        postSynapticLocation,
        JsNumber(distance)
        ) => NetworkConnected(
          preSynapticId = preSynapticId,
          postSynapticId = postSynapticId,
          initialWeight = initialWeight.doubleValue(),
          equilibriumWeight = equilibriumWeight.doubleValue(),
          preSynapticLocation = preSynapticLocation.convertTo[Cartesian],
          postSynapticLocation = postSynapticLocation.convertTo[Cartesian],
          distance = Microns(distance.doubleValue())
        )
        case _ => deserializationError("(pre_synaptic, post_synaptic) expected")
      }
      case _ => deserializationError("NeuronConnection expected")
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
