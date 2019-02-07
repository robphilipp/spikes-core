package com.digitalcipher.spiked.neurons

import akka.actor.ActorRef
import com.digitalcipher.spiked.neurons.SignalClock.current
import com.digitalcipher.spiked.neurons.learning.stdp._
import com.digitalcipher.spiked.construction.description.{LearningFunctionDescription, StdpAlphaLearningParams, StdpHardLimitLearningParams, StdpSoftLimitLearningParams}
import squants.Time
import squants.electro.{ElectricPotential, MagneticFlux, Millivolts}
import squants.motion.Velocity
import squants.space.Length
import squants.time.Milliseconds

import scala.util.Random

/**
  * Methods for the based calculations. For example calculating the updated membrane potential and decaying
  * it. Or for calculating the refractoriness, etc.
  *
  * These methods should common for neurons in each of the four dynamics categories:
  *   1.  mono-stable integrator
  *   2.  bi-stable integrator
  *   3.  mono-stable resonator
  *   4.  bi-stable resonator
  */
object Neuron {

  //
  // timing trait?
  //
  /**
    * // todo this ultimately needs to be removed and just fully covered in the sub-classes (maybe with a trait)
    * Returns a clock with the clock's start time to the specified value, and the neuron's time adjustment set to 0
    * @param time The number of milliseconds since epoch
    * @param clock The current clock (used to calculate the new clock)
    * @return (SignalClock, Long) a pair holding the new signal clock and the neuron's time-adjustment
    */
  def initializedTime(time: Long, clock: SignalClock): (SignalClock, Long) = (clock.withUpdatedStartTime(time), 0)

  /**
    * Calculates the time adjustment based on the current time and the specified time
    * @param time The number of milliseconds since epoch
    */
  def timeAdjustment(time: Long): Long = time - current()


  //
  //
  //

  /**
    * Calculates the delay time for a signal to travel the distance, down the axon, from one neuron to another
    * @param distance The distance between the neurons
    * @param conductanceSpeed The signal propagation speed
    * @return The delay incurred by the signal traveling the specified distance
    */
  def propagationDelay(distance: Length, conductanceSpeed: Velocity): Time = distance / conductanceSpeed

  /**
    * Calculates the refractoriness based on the last fire time. There are two components to the calculation: the
    * absolute refractoriness; and the relative refractoriness. During the absolute refractory period, the membrane
    * potential is zeroed out, regardless of its value.<p>
    * u(t < τ,,r,,) = 0 mV
    * <p>During the relative refractory period, an amount is subtracted from the membrane potential. The amount
    * subtracted starts at the base-refractoriness level and decays to zero exponentially, with a time-constant
    * given by the refractory period.<p>
    * u(t > τ,,r,,) = -φ,,r,, / (s - f - τ,,r,,)
    * <p>where s is the signal time, f is the last-fire time of the neuron, φ,,r,, is the base-refractoriness, and
    * τ,,r,, is the refractory period.
    *
    * @param signalTime        The signal time
    * @param lastFire          The last fire time
    * @param membranePotential The current membrane potential (after the incoming signal)
    * @return The refractoriness in mV
    */
  def refractorinessAdjusted(signalTime: Time,
                             lastFire: Time,
                             membranePotential: ElectricPotential,
                             refractoryPeriod: Time,
                             baseRefractoriness: MagneticFlux): ElectricPotential = {
    val timeSinceLastFire = signalTime - lastFire
    if (timeSinceLastFire >= refractoryPeriod || lastFire == Milliseconds(0)) {
      membranePotential - baseRefractoriness / (timeSinceLastFire - refractoryPeriod)
    }
    else {
      Millivolts(0)
    }
  }

  /**
    * Calculates the weight-change based on the spike-timing-dependent plasticity function. The is the method called
    * for pre-synaptic spikes the come before the post-synaptic spike, and will cause the weight to increase.
    * (Put yourself into the perspective of the neuron that receives signals from upstream, pre-synaptic, neurons)
    *
    * @param previousFireEventTime The previous fire event (sets the time for t=0)
    * @param processedSignals      An array holding the pre-synaptic signals received since the last firing of this neuron
    * @param fireEventTime         The number of milliseconds from epoch of the recent spike of the post-synaptic neuron.
    *                              The time should be the upper bound of all the pre-synaptic spikes that are being
    *                              processed by this call.
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param weightDecayFunction The function that describes the synapse connection weight decay
    * @param weightLimitFunction The function that sets the lower and upper bounds of the connection weights
    * @param maxWeight The maximum value a weight can have (i.e. the weight's upper bound).
    * @param preSpikeStdpAdjusters  stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                               ∆t = time between last two fire-events
    */
  def preSpikeWeightUpdates(previousFireEventTime: Time,
                            processedSignals: Vector[Signal],
                            fireEventTime: Time,
                            weights: Map[String, Weight],
                            weightDecayFunction: Time => Double,
                            weightLimitFunction: Double => Double,
                            maxWeight: Double,
                            preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double]): Map[String, StdpWeightAdjustment] = {
    val timeWindow: Time = fireEventTime - previousFireEventTime
    processedSignals
      .filter(signal => signal.preSynaptic != SignalReceiver.world)
      .groupBy(signal => signal.preSynaptic)
      .map(entry => {
        val preSynaptic = entry._1
        val originId = preSynaptic.path.name
        val signals = entry._2
        val totalAdjustment = signals
          .map(signal => signal.timestamp - previousFireEventTime)
          .filter(elapsedTime => elapsedTime.toMilliseconds > 0)
          .map(elapsedTime => {
            val adjustment = preSpikeStdpAdjusters(originId)(elapsedTime, timeWindow, weights(originId).value, maxWeight)
            //            println(s"${preSynaptic.path.name} -> ${self.path.name}; timeWindow: $timeWindow; elapsed: $elapsedTime; adjustment: $adjustment")
            adjustment
          })
          .sum

        // update the weights associated with this pre-synaptic neuron
        val updated = calculateUpdatedWeight(
          preSynaptic = preSynaptic,
          noise = 0,
          timeWindow = timeWindow,
          adjustment = totalAdjustment,
          weight = weights(originId),
          weightDecayFunction = weightDecayFunction,
          weightLimitFunction = weightLimitFunction
        )

        // return the entry with the updated weights
        originId -> StdpWeightAdjustment(
          sourceId = preSynaptic.path.name,
          previousWeight = updated._1,
          newWeight = updated._2,
          adjustment = totalAdjustment,
          timeWindow = timeWindow,
          stdpTime = timeWindow,
          signalTime = fireEventTime
        )
      })
  }

  /**
    * Calculates the weight-change based on the spike-timing-dependent plasticity function. This method is called when
    * signals arrive after the spike, but during the refractory period. (Put yourself into the
    * perspective of the neuron that receives signals from upstream, pre-synaptic, neurons)
    *
    * @param fireEventTime The spike time
    * @param signal        The signal
    */
  def postSpikeWeightUpdate(fireEventTime: Time,
                            signal: Signal,
                            weights: Map[String, Weight],
                            weightDecayFunction: Time => Double,
                            weightLimitFunction: Double => Double,
                            minWeight: Double,
                            postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double]): Option[StdpWeightAdjustment] = {
    if (signal.preSynaptic != SignalReceiver.world && signal.timestamp >= fireEventTime) {
      val originId: String = signal.preSynaptic.path.name
      val delta: Time = signal.timestamp - fireEventTime
      val weight = weights(originId)
      val adjustment: Double = postSpikeStdpAdjusters(originId)(-delta, weight.value, minWeight)

      val updated: (Double, Weight) = calculateUpdatedWeight(
        preSynaptic = signal.preSynaptic,
        noise = 0,
        timeWindow = delta,
        adjustment = adjustment,
        weight = weight,
        weightDecayFunction = weightDecayFunction,
        weightLimitFunction = weightLimitFunction
      )

      Option(StdpWeightAdjustment(
        sourceId = originId,
        previousWeight = weight.value,
        newWeight = updated._2,
        adjustment = adjustment,
        timeWindow = delta,
        stdpTime = delta,
        signalTime = signal.timestamp
      ))
    }
    else Option.empty
  }

  /**
    * Updates the weights with noise, based on a Wiener process
    *
    * @param preSynaptic The reference to the neuron that sent the signal
    * @param elapsedTime  The time since the last event; used to decay the weights
    */
  def weightUpdatedForNoise(preSynaptic: ActorRef,
                            elapsedTime: Time,
                            weightNoiseMagnitude: Double,
                            random: Random,
                            weight: Weight,
                            weightDecayFunction: Time => Double,
                            weightLimitFunction: Double => Double): Weight = {
    // calculate the amount of noise to add to the weights as a Wiener process
    val noise = weightNoiseFor(preSynaptic, elapsedTime, weightNoiseMagnitude, random)

    // update the weights
    calculateUpdatedWeight(
      preSynaptic = preSynaptic,
      timeWindow = elapsedTime,
      noise = noise,
      adjustment = 0,
      weight = weight,
      weightDecayFunction = weightDecayFunction,
      weightLimitFunction = weightLimitFunction
    )._2
  }

  /**
    * Calculate the amount of noise to add to the weights as a Wiener process
    * @param preSynaptic The pre-synaptic neuron reference
    * @param elapsedTime The elapsed time
    * @param weightNoiseMagnitude The magnitude of the weight noise in units of √s
    * @param random A random number generator
    * @return The amount the weight has drifted in the elapsed time due to noise of the specified magnitude
    */
  def weightNoiseFor(preSynaptic: ActorRef, elapsedTime: Time, weightNoiseMagnitude: Double, random: Random): Double =
    weightNoiseMagnitude * math.sqrt(elapsedTime.toMilliseconds) * random.nextGaussian()




  /**
    * Calculates the updated unbounded and bounded weights returns the unbound weight along with the weight
    *
    * @param preSynaptic The pre-synaptic neuron
    * @param timeWindow  The time since the last event
    * @param noise       The noise amount
    * @param adjustment  The learning adjustment
    * @param weight                The current pre-synaptic neuron connection with a efficacy (weight)
    * @param weightDecayFunction The function that describes the synapse connection weight decay
    * @param weightLimitFunction The function that sets the lower and upper bounds of the connection weights
    * @return A tuple holding the unbound weight and the weight object with the bounded and equilibrium weight
    */
  def calculateUpdatedWeight(preSynaptic: ActorRef,
                             timeWindow: Time,
                             noise: Double,
                             adjustment: Double,
                             weight: Weight,
                             weightDecayFunction: Time => Double,
                             weightLimitFunction: Double => Double): (Double, Weight) = {
    // decay the weight towards the equilibrium
    val currentWeight = weight.equilibrium + (weight.value + noise - weight.equilibrium) * weightDecayFunction(timeWindow)

    // bound the weight
    val boundedWeight = weightLimitFunction(currentWeight + adjustment)

    // update the weight held in the map
    (currentWeight, Weight(value = boundedWeight, equilibrium = weight.equilibrium))
  }


  /**
    * Calculate whether or not the event should take place based on whether a uniform random number in
    * interval [0,1] is less than the specified probability threshold. For example, a probability threshold
    * of 0.9 means that the event has a 0.9 probability of occurring. And so if the uniform random number is
    * less than the threshold then the event should take place.
    *
    * @param probability The probability of the event happening. Must be a number in the interval [0,1]
    * @param random The random number generator
    * @return `true` if the event should take place; `false` if the event should not take place
    */
  def releasesVesicles(probability: Double, random: Random): Boolean = random.nextDouble() < probability

  /**
    * Calculates an amount of noise to add to the membrane potential
    *
    * @param magnitude The magnitude of the membrane potential noise
    * @param random The random number generator
    * @return an amount of noise to add to the membrane potential
    */
  def noise(magnitude: ElectricPotential, random: Random): ElectricPotential = magnitude * random.nextGaussian()

  /**
    * Calculates the amount of noise ∆u in the membrane potential for a given ∆t as a Wiener process
    * with the specified noise magnitude, σ.
    * <p>
    * ∆u = σ sqrt(∆t/δt) N(0,1)
    * <p>
    * where N(0,1) is a standard distribution, ∆t is the time since the last event, δt is the time step
    * of 1 ms, and σ is the noise magnitude σ.
    *
    * @param deltaTime The time since the last event, ∆t
    * @param magnitude The magnitude of the membrane potential noise, σ
    * @param random The random number generator, N(0,1)
    * @return The noise in the membrane potential
    */
  def noise(deltaTime: Time, magnitude: ElectricPotential, random: Random): ElectricPotential =
    magnitude * math.sqrt(deltaTime.toMilliseconds) * random.nextGaussian()

  /**
    * Decays the intrinsic plasticity and returns the updated value
    * <p>
    *   b(t,,n,,) = b(t,,n,,) exp(-∆t/τ,,p,,)
    * <p>
    * @param deltaTime The time since the last neuron fire event, ∆t
    * @param plasticity The current value of the intrinsic plasticity, b(t,,n,,)
    * @param decayHalfLife The decay half-life of the intrinsic plasticity, τ,,p,,
    * @return The decayed intrinsic plasticity
    */
  def decayedIntrinsicPlasticity(deltaTime: Time, plasticity: ElectricPotential, decayHalfLife: Time): ElectricPotential =
    plasticity * math.exp(-deltaTime / decayHalfLife)

  /**
    * Updates the intrinsic plasticity that is adjusted each time the neuron fires.
    * <p>
    *   b(t,,n+1,,) = b(t,,n,,) + η exp(-(b(t,,n,,) + b,,0,,)
    * <p>
    * @param plasticity The current value of the intrinsic plasticity, b(t,,n,,)
    * @param learningRate The intrinsic plasticity learning rate, η
    * @param basePlasticity The base intrinsic plasticity, b,,0,,
    * @return The intrinsic plasticity
    */
  def updatedIntrinsicPlasticity(plasticity: ElectricPotential,
                                 learningRate: ElectricPotential,
                                 basePlasticity: ElectricPotential): ElectricPotential =
    plasticity + learningRate * math.exp(-(plasticity + basePlasticity) / Millivolts(1))


  //
  // original functions
  //

  /**
    * Calculates the membrane potential<p>
    *
    * P,,a,, = max( P,,min,,, V,,s,, + d(t) )
    *
    * <p>where P,,a,, is the membrane potential, P,,min,, is the minimum allowed membrane potential, V,,s,, is the
    * incoming signal voltage, d(t) is the decay function [[Neuron.decayMembranePotential]]
    *
    * @param signal               The signal holding the value and the timestamp
    * @param lastEvent            The timestamp of the previous event
    * @param membranePotential    The membrane potential
    * @param minMembranePotential The minimum value of the membrane potential
    * @param decayHalfLife        The half-life of the membrane-potential decay
    * @return The updated membrane potential
    */
  def calculateMembranePotential(signal: Signal,
                                 lastEvent: Time,
                                 membranePotential: ElectricPotential,
                                 minMembranePotential: ElectricPotential,
                                 decayHalfLife: Time): ElectricPotential =
    minMembranePotential.max(signal.value + decayMembranePotential(membranePotential, lastEvent, signal.timestamp, decayHalfLife))

  /**
    * The activation potential decays over time. The calculates the exponential decay at the time if the incoming signal<p>
    *
    * d(t) = P,,a,, exp( -(t - t,,p,,)/τ )
    *
    * <p>where P,,a,, is the current membrane potential, t is the current time, t,,p,, is the time of the last event,
    * and τ is the decay half-life.
    *
    * @param activationPotential The value of the activation potential just after the last event
    * @param lastEventTime       The milliseconds from epoch of the last event
    * @param currentEventTime    The milliseconds from epoch of the current event
    * @param decayHalfLife       The half-life of the membrane-potential decay
    * @return The decayed activation potential
    */
  def decayMembranePotential(activationPotential: ElectricPotential,
                             lastEventTime: Time,
                             currentEventTime: Time,
                             decayHalfLife: Time): ElectricPotential =
    activationPotential * math.exp(-(currentEventTime - lastEventTime) / decayHalfLife)

  /**
    * The response of the neuron to an incoming spike has a finite rise time given by the threshold
    * response kernel, which looks similar to the α-function. With our event-based approach, we can really
    * only calculate the time it takes the membrane potential to rise through the threshold, it if does,
    * and then delay the signal to the post-synaptic neurons by that time.
    * <p>
    * u(t) = (s e t / τ,,rise,,) e^-t / τ,,rise,,^
    * <p>
    * where s is the incoming signal, e is exp(1), τ,,rise,, is the half-life of the rise, typically the same
    * as the membrane potential decay half-life. The membrane potential response kernel reaches its maximum
    * value at t = τ,,rise,,, and has a value of s at that maximum.
    * <p>
    * The response kernel is approximately linear during the rise. Therefore, we approximate the rise as linear.
    * so that we can calculate the time it takes for the response to reach the threshold θ. Importantly, we make
    * the assumption that the membrane potential before the incoming signal, u,,0,,, and the membrane potential
    * at the maximum, u,,f,,, meet u,,0,, < θ ≤ u,,f,,. Then under that condition, the rise time is the calculated
    * by solving for the time it takes for the response to reach the threshold θ,
    * <p>
    * u(t) = (u,,f,, / t,,f,,) t
    * <p>
    * u(θ) = (u,,f,, / t,,f,,) t,,θ,, = (u,,f,, / t,,f,,) τ,,rise,,
    * <p>
    * u(t,,0,,) = (u,,f,, / t,,f,,) t,,0,,
    * <p>
    * t,,rise,, = t,,θ,, - t,,0,, = τ,,rise,, - t,,0,,
    * <p>
    * Solving for τ,,rise,, in u(θ), and for t,,0,, in u(t,,0,,)
    * <p>
    * τ,,rise,, = (θ / u,,f,,) t,,f,,
    * <p>
    * t,,0,, = ((θ - u,,f,, + s) / u,,f,,) t,,f,,
    * <p>
    * Notice that this method doesn't specified u,,0,,, but rather it specifies u,,f,,, where u,,f,, = u,,0,, + s.
    * And so,
    * <p>
    * t = τ,,rise,, (θ - u,,f,, + s) / θ
    * <p>
    * Finally, when the signal is 0 or when the condition u,,0,, < θ ≤ u,,f,, is not met, then we simply return
    * a rise time of 0. This makes sense because this method is only valid if u(t) ≥ θ, and so s = 0 implies that
    * we've already reached the threshold. Similarly for u,,0,, ≥ θ.
    *
    * @param initialMembranePotential The initial value of the membrane potentials before it was adjusted for the
    *                                 incoming signal
    * @param membranePotential The current membrane potential (already includes the full incoming signal) u,,f,,
    * @param threshold         The spike threshold value, θ
    * @param riseHalfLife      The rise half-life, τ,,rise,,
    * @return The delay in the spike to account for the rise time of the membrane potential. The maximum delay is
    *         0 if the signal s = 0 or u,,0,,≥ θ.
    */
  def calculateRiseTimeDelay(initialMembranePotential: ElectricPotential,
                             membranePotential: ElectricPotential,
                             threshold: ElectricPotential,
                             riseHalfLife: Time): Time = {
    if (membranePotential <= initialMembranePotential || initialMembranePotential >= threshold)
      Milliseconds(0)
    else
      riseHalfLife * (threshold.min(membranePotential) - initialMembranePotential) / threshold
  }

  /**
    * Converts the learning function description into a learning function
    *
    * @param description The learning function description
    * @return The learning function
    */
  def convert(description: LearningFunctionDescription): SpikeTimingDependentPlasticity = {
    import com.digitalcipher.spiked.construction.description.LearningFunctionDescription.{NO_LEARNING, STDP_ALPHA, STDP_HARD, STDP_SOFT}
    description.learningParams.learningType match {
      case STDP_HARD.name =>
        val stdp = description.learningParams.asInstanceOf[StdpHardLimitLearningParams]
        StdpHardLimit(
          inhibitionAmplitude = stdp.inhibitionAmplitude,
          inhibitionPeriod = stdp.inhibitionPeriod,
          excitationAmplitude = stdp.excitationAmplitude,
          excitationPeriod = stdp.excitationPeriod
        )

      case STDP_SOFT.name =>
        val stdp = description.learningParams.asInstanceOf[StdpSoftLimitLearningParams]
        StdpSoftLimit(
          inhibitionAmplitude = stdp.inhibitionAmplitude,
          inhibitionPeriod = stdp.inhibitionPeriod,
          excitationAmplitude = stdp.excitationAmplitude,
          excitationPeriod = stdp.excitationPeriod
        )

      case STDP_ALPHA.name =>
        val stdp = description.learningParams.asInstanceOf[StdpAlphaLearningParams]
        StdpAlphaLearning(
          baseline = stdp.baseline,
          timeConstant = stdp.timeConstant,
          learningRate = stdp.learningRate
        )

      case NO_LEARNING.name => NoLearning()
    }
  }

  /**
    * The information associated with the weight adjustment for STDP learning
    * @param sourceId The ID of the pre-synaptic neuron (i.e. the signal's source)
    * @param previousWeight The connection weight before the adjustment
    * @param newWeight The connection weight after the adjustment was made
    * @param adjustment The connection weight adjustment amount
    * @param timeWindow The time-window covering the STDP learning event (i.e. since the last
    *                   spike or current spike)
    * @param stdpTime The signal time used to calculate the weight adjustment
    * @param signalTime The time of the signal's arrival
    */
  case class StdpWeightAdjustment(sourceId: String,
                                  previousWeight: Double,
                                  newWeight: Weight,
                                  adjustment: Double,
                                  timeWindow: Time,
                                  stdpTime: Time,
                                  signalTime: Time)

  /**
    * Message to set the initial time based on the message time
    *
    * @param time The time from the server in milliseconds from epoch
    */
  case class InitializeTime(time: Long)

  /**
    * Message to synchronize to times
    *
    * @param time The time from the server in milliseconds from epoch
    */
  case class SynchronizeTime(time: Long)

  /**
    * The message for connecting two neurons
    *
    * @param postSynaptic      The post-synaptic neuron ([[ActorRef]])
    * @param weight            The synapse weight (i.e. the factor multiplying the membrane potential increase due to an incoming signal)
    * @param equilibriumWeight The equilibrium weight to which the weight decays
    * @param distance          The distance that the neurons are apart
    * @param learning          Function description for calculating the pre- and post-spike spike-time-dependent plasticity for the connection
    */
  case class Connect(postSynaptic: ActorRef,
                     weight: Double,
                     equilibriumWeight: Double,
                     distance: Length,
                     learning: LearningFunctionDescription)

  /**
    * The message for adding a copy of a connection between two connected neurons. This message is sent from the post-synaptic
    * neuron to the pre-synaptic neuron.
    *
    * @param postSynaptic The post-synaptic neuron that send the request
    * @param weight       The weight of the current connection between the two
    */
  case class AddConnection(postSynaptic: ActorRef, weight: Double)

  /**
    * The message for adding the pre-synaptic neuron for managing weights in the post-synaptic neuron
    *
    * @param preSynaptic       The pre-synaptic neuron ([[ActorRef]])
    * @param postSynaptic      The ID of the connection as held by the pre-synaptic neuron. The neuron in the connection should be
    *                          the post-synaptic neuron (i.e. the one receiving the request to register the pre-synaptic neuron)
    * @param weight            The synapse weight (i.e. the factor multiplying the membrane potential increase due to an incoming signal)
    * @param equilibriumWeight The equilibrium weight to which the weight decays
    * @param learning          Function description for calculating the pre- and post-spike spike-time-dependent plasticity for the connection
    */
  case class AddPreSynapticRef(preSynaptic: ActorRef,
                               postSynaptic: ActorRef,
                               weight: Double,
                               equilibriumWeight: Double,
                               learning: LearningFunctionDescription)

  /**
    * Represent a synapse between two neurons
    *
    * @param preSynapticNeuron The pre-synaptic neuron ([[ActorRef]])
    * @param weight            The synapse weight (i.e. the factor multiplying the membrane potential increase due to an incoming signal)
    */
  case class Synapse(preSynapticNeuron: ActorRef, weight: Double)

  /**
    * Represents a query for the neuron's initial time and timing adjustment
    */
  case class TimingQuery()

  /**
    * Response to a query for the neuron's initial time and timing adjustment
    * @param clock The clock holding the timing information (including the initial time)
    * @param timingAdjustment The timing adjustment to the clock
    */
  case class TimingResponse(clock: SignalClock, timingAdjustment: Long)

  /**
    * Represents a query to retrieve the neuron's current simulation time
    */
  case class SimulationTimeQuery()

  /**
    * Represents the response to a query to retrieve the neuron's current simulation time, and holds
    * the neuron's current simulation time
    * @param currentTime The neuron's current simulation time
    */
  case class SimulationTimeResponse(currentTime: Time)

  /**
    * Represents a query to retrieve the neuron's current state data
    */
  case class StateDataQuery()

  /**
    * Represents a query to retrieve the neuron's current pre-synaptic weights
    */
  case class WeightsQuery()

  /**
    * Represents a query to retrieve the neuron's post-synaptic connections
    */
  case class ConnectionsQuery()

  /**
    * Represents a query to retrieve the neuron's current state (i.e. depolarized, spiking, etc)
    */
  case class StateQuery()

  /**
    * Represents a connection weight
    * @param value       The actual weight at any given time
    * @param equilibrium The equilibrium weight
    */
  case class Weight(value: Double, equilibrium: Double)

  case object Weight {
    /**
      * A weight intended to represent input from the world rather than from a connection
      *
      * @return An empty weight that has neutral values
      */
    def world = Weight(1.0, 1.0)
  }
}
