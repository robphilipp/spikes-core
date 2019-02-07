package com.digitalcipher.spiked.construction

import akka.actor.{Actor, ActorRef}
import com.digitalcipher.spiked.construction.NeuronCreator._
import com.digitalcipher.spiked.construction.description.NeuronDescription
import com.digitalcipher.spiked.neurons.weights.decay.{Exponential, NoDecay}
import com.digitalcipher.spiked.neurons.weights.limit.{Bounded, Unbounded, WeightLimiterFunction}
import com.digitalcipher.spiked.neurons.{BistableIntegrator, MonostableIntegrator, Neuron, SignalReleaseProbability}
import com.digitalcipher.spiked.construction.description._
import squants.Time

/**
  * Actor for creating a neuron actor based on the neuron description
  * Created by rob on 7/16/17.
  */
class NeuronCreator extends Actor {

  var timeFactor: Int = 1

  /**
    * Constructs the neurons for each neuron-description in the list
    * @param descriptions a list of neuron descriptions
    * @return A [[CreateNeuronResponse]] response with the result of the creation request
    */
  private def createNeurons(descriptions: List[NeuronDescription]): CreateNeuronsResponse = {
    val neurons = descriptions.map(description => createNeuron(description)).toMap
    CreateNeuronsResponse(neurons)
  }

  /**
    * Converts the weight-decay description into the weight-decay function that takes a time since the last signal and
    * returns a decay factor f,,d,, where 0 ≤ f,,d,, ≤ 1
    * @param description The weight-decay description
    * @return The weight-decay function that takes a time and returns the decay factor
    */
  private def convert(description: WeightDecayDescription): Time => Double = {
    import com.digitalcipher.spiked.construction.description.WeightDecayDescription._
    description.decayParams.decayType match {
      case EXPONENTIAL.name =>
        val params = description.decayParams.asInstanceOf[ExponentialDecayParams]
        Exponential(params.decayHalfLife).decayFunction

      case ZERO.name => NoDecay().decayFunction
    }
  }

  /**
    * Converts the weight-limit description into a weight limit function that takes a weight and returns the weight that
    * is in the interval [W,,min,,, W,,max,,]
    * @param description The weight limit description
    * @return The weight-limit function
    */
  private def convert(description: WeightLimitDescription): WeightLimiterFunction = {
    import com.digitalcipher.spiked.construction.description.WeightLimitDescription._
    description.limiterParams.limiterType match {
      case BOUNDED.name =>
        val params = description.limiterParams.asInstanceOf[BoundedParams]
        Bounded(params.lowerBound, params.upperBound)
      case UNBOUNDED.name => Unbounded()
    }
  }

  /**
    * Converts the signal-release probability description into a [[SignalReleaseProbability]] instance
    * @param description The description of the signal release probability function
    * @return a [[SignalReleaseProbability]] instance
    */
  private def convert(description: SignalReleaseProbabilityDescription): SignalReleaseProbability = {
    SignalReleaseProbability.from(
      facilitatorBase = description.facilitator.base,
      facilitatorMagnitude = description.facilitator.magnitude,
      facilitationTimeConstant = description.facilitator.timeConstant,
      depletionBase = description.depletion.base,
      depletionMagnitude = description.depletion.magnitude,
      depletionTimeConstant = description.depletion.timeConstant
    )
  }

  /**
    * Converts the [[NeuronDescription]] into a [[Neuron]] of the type specified as the neuron type
    *
    * @param description The description of the neuron
    * @return A [[Neuron]] constructed from the description
    */
  private def convert(description: NeuronDescription): CreateNeuronResponse = {
    val neuron = createNeuron(description)
    CreateNeuronResponse(neuron._1, neuron._2)
  }

  private def createNeuron(description: NeuronDescription): (ActorRef, LocationDescription) = {
    // the additional, neuron-type-specific parameters encode the type of neuron dynamics
    description.neuronSpecificParams match {
      case MonostableIntegratorParams(spikeThreshold) =>
        val neuronProps = MonostableIntegrator.props(
          timeFactor = timeFactor,
          id = description.neuronId,
          threshold = spikeThreshold,
          refractoryPeriod = description.refractoryPeriod,
          baseRefractoriness = description.baseRefractoriness,
          minMembranePotential = description.minMembranePotential,
          spikePotential = description.spikePotential,
          decayHalfLife = description.membranePotentialDecayHalfLife,
          riseHalfLife = description.membranePotentialRiseHalfLife,
          weightDecayFunction = convert(description.weightDecayDescription),
          weightLimitFunction = convert(description.weightLimitDescription),
          synapseTiming = convert(description.synapseTimingDescription),
          conductanceSpeed = description.conductanceSpeed,
          inhibitory = description.inhibitor,
          membranePotentialNoiseMagnitude = description.membranePotentialNoise,
          weightNoiseMagnitude = description.weightNoiseMagnitude,
          intrinsicPlasticityBase = description.intrinsicPlasticityBase,
          intrinsicPlasticityLearningRate = description.intrinsicPlasticityLearningRate,
          intrinsicPlasticityDecayHalfLife = description.intrinsicPlasticityDecayHalfLife
        )
        (context.actorOf(neuronProps, description.neuronId), description.locationDescription)

      case BistableIntegratorParams(limitCycleThreshold, restingStateThreshold, fireRate) =>
        val neuronProps = BistableIntegrator.props(
          timeFactor = timeFactor,
          id = description.neuronId,
          firingThreshold = limitCycleThreshold,
          restingThreshold = restingStateThreshold,
          tonicFireRate = fireRate,
          refractoryPeriod = description.refractoryPeriod,
          minMembranePotential = description.minMembranePotential,
          spikePotential = description.spikePotential,
          decayHalfLife = description.membranePotentialDecayHalfLife,
          riseHalfLife = description.membranePotentialRiseHalfLife,
          weightDecayFunction = convert(description.weightDecayDescription),
          weightLimitFunction = convert(description.weightLimitDescription),
          synapseTiming = convert(description.synapseTimingDescription),
          conductanceSpeed = description.conductanceSpeed,
          inhibitory = description.inhibitor,
          membranePotentialNoiseMagnitude = description.membranePotentialNoise,
          weightNoiseMagnitude = description.weightNoiseMagnitude,
          intrinsicPlasticityBase = description.intrinsicPlasticityBase,
          intrinsicPlasticityLearningRate = description.intrinsicPlasticityLearningRate,
          intrinsicPlasticityDecayHalfLife = description.intrinsicPlasticityDecayHalfLife
        )
        (context.actorOf(neuronProps, description.neuronId), description.locationDescription)
    }
  }

  override def receive: Receive = {
    case CreateTestMessage(message) => println(s"received create-test message: $message")

    case SimulationTimeFactor(factor) => timeFactor = factor

    case CreateNeurons(neurons) => sender() ! createNeurons(neurons)

    case CreateNeuron(description) => sender() ! convert(description)

    case _ => println(s"received invalid request")
  }
}

object NeuronCreator {
  /**
    * Create-neuron message
    * @param description The neuron description
    */
  case class CreateNeuron(description: NeuronDescription)
  case class CreateNeuronResponse(actorRef: ActorRef, location: LocationDescription)

  case class CreateNeurons(neurons: List[NeuronDescription])
  case class CreateNeuronsResponse(neurons: Map[ActorRef, LocationDescription])

  case class SimulationTimeFactor(timeFactor: Int)

  case class CreateTestMessage(message: String)
}
