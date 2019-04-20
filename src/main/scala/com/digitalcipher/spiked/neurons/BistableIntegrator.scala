package com.digitalcipher.spiked.neurons

import akka.actor.{ActorRef, FSM, Props, Timers}
import com.digitalcipher.spiked.logging._
import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.neurons.BistableIntegrator._
import com.digitalcipher.spiked.neurons.Neuron._
import com.digitalcipher.spiked.neurons.SignalClock.nowWithTimeFactor
import com.digitalcipher.spiked.neurons.SignalReceiver.{IdQuery, TimeFactorQuery}
import com.digitalcipher.spiked.neurons.weights.limit.{Unbounded, WeightLimiterFunction}
import com.digitalcipher.spiked.construction.description.LearningFunctionDescription
import squants.Time
import squants.electro._
import squants.motion.{MetersPerSecond, Velocity}
import squants.space.Length
import squants.time._

import scala.collection.immutable.HashMap

/**
  * Bistable integrator.
  *
  * Use the companion object [[com.digitalcipher.spiked.neurons.BistableIntegrator]] `from(...)` method to construct an instance. The bistable integrator
  * neuron starts in the resting state, which is a stable equilibrium as long as the incoming current is less than the
  * firing threshold. A neuron in the resting state whose current exceeds the firing threshold enters the limit cycle
  * representing a tonic firing rate. It remains in the limit cycle until the current drops below the resting threshold.
  *
  * @constructor Creates a neuron with the specified properties
  * @param neuronId                         The unique ID of this neuron
  * @param timeFactor                       The simulation time-factor (1 second simulation time is F seconds in real-time)
  * @param firingThreshold                  The current at which the neuron transitions from the resting state into the limit cycle.
  * @param restingThreshold                 The current at which the neuron transitions from the limit cycle to the resting state.
  * @param tonicFiredRate                   A function that maps the incoming current to the tonic firing rate of the neuron in the
  *                                         the limit cycle.
  * @param refractoryPeriod                 The refractory period after a fire event
  * @param baseRefractoriness               The base-refractoriness constant (V • s) that determines the amount of the relative
  *                                         refractoriness after the refractory period.
  * @param minMembranePotential             The minimum allowed value for the membrane potential
  * @param spikePotential                   The spike intensity
  * @param decayHalfLife                    The decay half-life of the membrane potential after receiving a signal (response kernel)
  * @param riseHalfLife                     The rise half-life of the membrane potential after receiving a signal (response kernel)
  * @param weightDecayFunction              The function that describes the synapse connection weight decay
  * @param weightLimitFunction              The function that sets the lower and upper bounds of the connection weights
  * @param inhibitory                       `true` when the neuron is inhibitory (i.e. sends negative signals); `false` when neuron is
  *                                         excitatory (default) and sends positive signals
  * @param membranePotentialNoiseMagnitude  The magnitude of the membrane potential noise
  * @param weightNoiseMagnitude             The magnitude of the weight noise
  * @param intrinsicPlasticityLearningRate  The rate of learning for the intrinsic plasticity
  * @param intrinsicPlasticityBase          The base of the intrinsic learning
  * @param intrinsicPlasticityDecayHalfLife The half-life of the intrinsic learning decay
  */
class BistableIntegrator(timeFactor: Int,
                         neuronId: String,
                         firingThreshold: ElectricPotential,
                         restingThreshold: ElectricPotential,
                         tonicFiredRate: Frequency,

                         refractoryPeriod: Time,
                         baseRefractoriness: MagneticFlux = Webers(8e-8),
                         minMembranePotential: ElectricPotential,
                         spikePotential: ElectricPotential,

                         decayHalfLife: Time,
                         riseHalfLife: Time,
                         weightDecayFunction: Time => Double,
                         weightLimitFunction: Double => Double,
                         synapseTiming: SignalReleaseProbability = SignalReleaseProbability.default(),
                         conductanceSpeed: Velocity,
                         inhibitory: Boolean,
                         membranePotentialNoiseMagnitude: ElectricPotential = Millivolts(0.1),
                         weightNoiseMagnitude: Double,
                         intrinsicPlasticityLearningRate: ElectricPotential,
                         intrinsicPlasticityBase: ElectricPotential,
                         intrinsicPlasticityDecayHalfLife: Time) extends FSM[BsiState, StateData] with Timers {
  // calculate the min and max allowed connection weight values based on the specified weight limit functions
  private val maxWeight: Double = weightLimitFunction(Double.MaxValue)
  private val minWeight: Double = weightLimitFunction(Double.MinValue)

  // the inhibition factor that multiples the outgoing signal strength
  private val inhibitionFactor: Int = if (inhibitory) -1 else 1

  import context.dispatcher

  import scala.concurrent.duration._

  // called during construction to set the initial state
  startWith(Depolarized, initialStateData(clock = nowWithTimeFactor(timeFactor)))

  // when a message arrives and the neuron is in the depolarized state
  when(Depolarized) {
    case Event(Signal(sent, origin, value), data: StateData) => processSignal(sent, origin, value, data)
  }

  when(Spiking) {
    case Event(TonicSpike(), data: StateData) => dispatchSpikes(data)
  }

  onTransition {
    case Depolarized -> Spiking => stateData match {
      case _ => timers.startSingleTimer(TonicSpikeTimer, TonicSpike(), (1.0 / tonicFiredRate.toKilohertz).toLong millis)
    }

    case Spiking -> Depolarized => timers.cancel(TonicSpikeTimer)
  }

  // when a message arrives and no other handler has dealt with the message
  whenUnhandled {
    case Event(IdQuery(), _) =>
      sender ! id
      stay

    case Event(TimeFactorQuery(), data) =>
      sender ! data.clock.timeFactor
      stay

    case Event(TimingQuery(), data) =>
      sender ! TimingResponse(data.clock, data.timeAdjustment)
      stay

    case Event(SimulationTimeQuery(), data) =>
      sender ! SimulationTimeResponse(data.clock.now())
      stay

    case Event(StateDataQuery(), data) =>
      sender ! data
      stay

    case Event(StateQuery(), _) =>
      sender ! stateName
      stay

    case Event(WeightsQuery(), data) =>
      sender ! data.weights
      stay

    case Event(ConnectionsQuery(), data) =>
      sender ! data.connections
      stay

    case Event(InitializeTime(time), data: StateData) => initializeTime(time, data)

    case Event(SynchronizeTime(time), data: StateData) => synchronizeTime(time, data)

    case Event(Connect(postSynaptic, weight, equilibriumWeight, distance, learning), data) =>
      connectPostSynaptic(postSynaptic, weight, equilibriumWeight, distance, learning, data)

    case Event(AddPreSynapticRef(preSynaptic, postSynaptic, weight, equilibriumWeight, learning), data) =>
      registerPreSynaptic(preSynaptic, postSynaptic, weight, equilibriumWeight, learning, data)

    case Event(e, s) =>
      log.warning("received unhandled request {} in state {}/{}", e, stateName, s)
      sender ! "Invalid message"
      stay
  }

  // initialize the neuron
  initialize()

  /**
    * Updates the state for the bistable integrator neuron
    *
    * @param processedSignals       Holds a list of the processed signals for calculating learning (i.e. weight adjustments)
    * @param membranePotential      The current membrane potentional of the neuron
    * @param lastEvent              The time of the last event
    * @param lastFire               The time of the last fire (spike)
    * @param lastTransitionToRest   The time of the last transition from tonic spiking to the resting state
    * @param connections            A map associating a connected post-synaptic neuron to the signal propagation time for the
    *                               spike to travel from the pre-synaptic neuron to the post-synaptic neuron
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param preSpikeStdpAdjusters  stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                               ∆t = time between last two fire-events
    * @param postSpikeStdpAdjusters stdp(t) where t = signal time relative to last fire-event
    * @param timeAdjustment         adjustment made to the initial time (added to the initial time) when calculating "now". for example,
    *                               if current is 100,001,000, initial time is 100,000,000, and adjustment is 0, then now would be 1,000.
    *                               if, in the same example, the time adjustment is 100, then now = 100,001,000 - 100,000,000 - 100 = 900.
    *                               a time adjustment of 100 would mean that this neuron's clock has drifted 100 ms into the future, and
    *                               the time adjustment of 100 ms is applied to correct that drift
    * @param intrinsicPlasticity    Neurons that have more spiking activity tend to fire more readily. This is the base value.
    * @param clock                  The clock defining elapsed times for the neuron
    * @return The new updated [[StateData]] instance
    */
  private def updatedStateData(data: StateData)
                              (processedSignals: Vector[Signal] = stateData.processedSignals,
                               membranePotential: ElectricPotential = stateData.membranePotential,
                               lastEvent: Time = stateData.lastEvent,
                               lastFire: Time = stateData.lastFire,
                               lastTransitionToRest: Time = stateData.lastTransitionToRest,
                               // base neuron state
                               connections: Map[ActorRef, Time] = stateData.connections,
                               weights: Map[String, Weight] = stateData.weights,
                               preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double] = stateData.preSpikeStdpAdjusters,
                               postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double] = stateData.postSpikeStdpAdjusters,
                               timeAdjustment: Long = stateData.timeAdjustment,
                               intrinsicPlasticity: ElectricPotential = stateData.intrinsicPlasticity,
                               clock: SignalClock = stateData.clock): StateData = {
    stateData.copy(
      processedSignals = processedSignals,
      membranePotential = membranePotential,
      lastEvent = lastEvent,
      lastFire = lastFire,
      lastTransitionToRest = lastTransitionToRest,
      // base neuron state
      connections = connections,
      weights = weights,
      preSpikeStdpAdjusters = preSpikeStdpAdjusters,
      postSpikeStdpAdjusters = postSpikeStdpAdjusters,
      timeAdjustment = timeAdjustment,
      intrinsicPlasticity = intrinsicPlasticity,
      clock = clock
    )
  }

  /**
    * Sets the initial time to the value specified, and calculates that time adjustment
    *
    * @param time The number of milliseconds since epoch
    */
  private def initializeTime(time: Long, stateData: StateData): FSM.State[BsiState, StateData] = {
    val timing: (SignalClock, Long) = initializedTime(time, stateData.clock)
    stay using updatedStateData(stateData)(clock = timing._1, timeAdjustment = timing._2)
  }

  // todo make this a running average or some algorithm that smoothly adjusts the times
  /**
    * Calculates the time adjustment based on the current time and the specified time
    *
    * @param time The number of milliseconds since epoch
    */
  private def synchronizeTime(time: Long, stateData: StateData): FSM.State[BsiState, StateData] =
    stay using updatedStateData(stateData)(timeAdjustment = timeAdjustment(time))

  /**
    * @return The unique ID of the neuron
    */
  def id: String = neuronId

  /**
    * Connects this neuron to the specified post-synaptic neuron. (Put yourself in the perspective of the neuron that
    * sends signals to downstream, post-synaptic, neurons)
    *
    * @param postSynaptic      The post-synaptic neuron to which to connect
    * @param weight            The initial connection weight
    * @param equilibriumWeight The equilibrium weight to which the weight decays
    * @param distance          The distance between the pre-synaptic and post-synaptic neuron
    * @param learning          The description of the learning function
    */
  private def connectPostSynaptic(postSynaptic: ActorRef,
                                  weight: Double,
                                  equilibriumWeight: Double,
                                  distance: Length,
                                  learning: LearningFunctionDescription,
                                  stateData: StateData): FSM.State[BsiState, StateData] = {

    // added the connection to the post-synaptic neuron
    val signalDelayTime = Neuron.propagationDelay(distance, conductanceSpeed)
    val newStateData = updatedStateData(stateData)(connections = stateData.connections + (postSynaptic -> signalDelayTime))

    // log the connection to the post-synaptic neuron
    EventLogger.log(context.system.name, () => ConnectedPostSynaptic(id, postSynaptic.path.name, signalDelayTime))

    // let the post-synaptic neuron know that the pre-synaptic neuron as connected to it
    postSynaptic ! AddPreSynapticRef(self, postSynaptic, weight, equilibriumWeight, learning)

    stay using newStateData
  }

  /**
    * Registers the pre-synaptic neuron so that it can apply the synapse weight to signals from. (Put yourself into the
    * perspective of the neuron that receives signals from upstream, pre-synaptic, neurons)
    *
    * @param preSynaptic       The pre-synaptic neuron that formed a connection to this one
    * @param postSynaptic      The ID of the connection (as held by the pre-synaptic neuron; the actor ref should be this neuron!;
    *                          but we care about the cardinality value)
    * @param initialWeight     The initial membrane potential response to incoming signal from the specified pre-synaptic neuron
    * @param equilibriumWeight The equilibrium weight to which the connection weight will decay over time
    * @param learning          The description of the learning function
    */
  private def registerPreSynaptic(preSynaptic: ActorRef,
                                  postSynaptic: ActorRef,
                                  initialWeight: Double,
                                  equilibriumWeight: Double,
                                  learning: LearningFunctionDescription,
                                  stateData: StateData): FSM.State[BsiState, StateData] = {
    val preSynapticNeuronId = preSynaptic.path.name
    val learningFunctions = convert(learning)

    val newStateData = updatedStateData(stateData)(
      weights = stateData.weights + (preSynaptic.path.name -> Weight(value = initialWeight, equilibrium = equilibriumWeight)),
      preSpikeStdpAdjusters = stateData.preSpikeStdpAdjusters + (preSynapticNeuronId -> learningFunctions.preSpikeStdpFunction),
      postSpikeStdpAdjusters = stateData.postSpikeStdpAdjusters + (preSynapticNeuronId -> learningFunctions.postSpikeStdpFunction)
    )

    // log registration
    EventLogger.log(context.system.name, () => PreSynapticRegistration(id, preSynapticNeuronId, initialWeight))

    stay using newStateData
  }

  /**
    * Processes the received signal and records any time changes for the neuron's events
    *
    * @param sent   The timestamp recording when the received signal was sent (uses the sending neuron's clock, which
    *               may not be synchronized with this neuron's clock)
    * @param origin The neuron from which the signal originated
    * @param value  The signal strength (voltage)
    * @param stateData The current neuron state data
    * @return The updated FSM state
    */
  private def processSignal(sent: Time, origin: ActorRef, value: ElectricPotential, stateData: StateData): FSM.State[BsiState, StateData] = {
    val timestamp = stateData.clock.now()

    // log receipt of signal
    EventLogger.log(context.system.name, () => SignalReceived(
      neuronId = id,
      sourceId = if (origin == SignalReceiver.world) "[world]" else origin.path.name,
      timestamp = timestamp,
      lastEventTime = stateData.lastEvent,
      lastFireTime = stateData.lastFire,
      signalIntensity = value
    ))

    // grab the membrane potential response, or if the signal came from the world-at-large (i.e. to a sensory neuron)
    // then set the weight to 1.0, then rewrite the signal with the received-timestamp and the new signal intensity
    val weight = if (origin == SignalReceiver.world) Weight.world else stateData.weights(origin.path.name)
    val signal = Signal(timestamp, origin, weight.value * value)

    // add to the list of processed signals
    val signals = stateData.processedSignals :+ signal

    // update the average incoming current
    updateMembranePotential(
      stateData = stateData,
      signal = signal,
      lastEvent = stateData.lastEvent,
      lastFire = stateData.lastFire,
      lastTransitionToRest = stateData.lastTransitionToRest,
      membranePotential = stateData.membranePotential,
      clock = stateData.clock,
      processedSignals = signals,
      weights = stateData.weights,
      preSpikeStdpAdjusters = stateData.preSpikeStdpAdjusters,
      postSpikeStdpAdjusters = stateData.postSpikeStdpAdjusters
    )
  }

  /**
    * Updates the membrane potential if the neuron is beyond the refractory period. If the membrane potential exceeds
    * the threshold, then fires a spike to all its post-synaptic neurons.
    *
    * @param stateData The neuron state data
    * @param signal                 The [[Signal]] that holds the timestamp of when it was sent and the intensity (value)
    * @param lastEvent              The timestamp of the last time a signal was received
    * @param lastFire               The timestamp of the last time this neuron fired
    * @param lastTransitionToRest   The time of the last transition from tonic spiking to the resting state
    * @param membranePotential      The current membrane potential of the neuron
    * @param clock                  The clock defining elapsed times for the neuron
    * @param processedSignals       Holds a list of the processed signals for calculating learning (i.e. weight adjustments)
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param preSpikeStdpAdjusters  stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                               ∆t = time between last two fire-events
    * @param postSpikeStdpAdjusters stdp(t) where t = signal time relative to last fire-event
    * @return The last time this neuron fired
    */
  private def updateMembranePotential(stateData: StateData,
                                      signal: Signal,
                                      lastEvent: Time,
                                      lastFire: Time,
                                      lastTransitionToRest: Time,
                                      membranePotential: ElectricPotential,
                                      clock: SignalClock,
                                      processedSignals: Vector[Signal],
                                      weights: Map[String, Weight],
                                      preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double],
                                      postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double]): FSM.State[BsiState, StateData] = {

    // calculate the updated membrane potential given the incoming signal
    var updatedMembranePotential = calculateMembranePotential(signal, lastEvent, membranePotential, minMembranePotential, decayHalfLife)
    //    stateData = updatedStateData(membranePotential = updatedMembranePotential)

    // adjust the membrane potential for refractoriness and cap it at the firing threshold, and cap it at the
    // firing threshold so that it doesn't grow too large (kind of a refractory period for tonic spiking)
    updatedMembranePotential = refractorinessAdjusted(
      signal.timestamp, lastTransitionToRest, updatedMembranePotential, refractoryPeriod, baseRefractoriness
    ).min(firingThreshold)

    //    stateData = updatedStateData(membranePotential = updatedMembranePotential)

    // log the membrane potential update
    EventLogger.log(context.system.name, () => MembranePotentialUpdate(
      neuronId = id,
      timestamp = signal.timestamp,
      lastEventTime = lastEvent,
      lastFireTime = lastFire,
      membranePotential = updatedMembranePotential
    ))

    // a current at or above the firing threshold when the neuron isn't already firing moves the neuron into the
    // limit-cycle (i.e. tonic firing)
    var lastTransitionTime = lastFire
    if (updatedMembranePotential >= firingThreshold) {
      // mark the transition from resting state to the limit-cycle (tonic firing)
      lastTransitionTime = clock.now()

      // log the phase transition to the limit cycle
      EventLogger.log(context.system.name, () => PhaseTransition(
        neuronId = id,
        timestamp = lastTransitionTime,
        transitionType = "limit_cycle",
        membranePotential = updatedMembranePotential,
        firingRate = tonicFiredRate
      ))

      // update the synapse weights (membrane potential response multipliers) for the pre-synaptic signals
      val updatedWeights = preSpikeUpdatedWeights(
        previousFireEventTime = lastFire,
        processedSignals = processedSignals,
        lastTransitionTime = lastTransitionTime,
        weights = weights,
        preSpikeStdpAdjusters = preSpikeStdpAdjusters
      )

      // return the updated state
      goto(Spiking) using stateData.copy(
        processedSignals = Vector.empty,
        membranePotential = updatedMembranePotential,
        weights = updatedWeights,
        lastEvent = lastTransitionTime,
        lastFire = lastTransitionTime
      )
    }
    else if (signal.timestamp > lastTransitionTime + refractoryPeriod) {
      stay using stateData.copy(
        membranePotential = updatedMembranePotential,
        weights = postSpikeUpdateWeight(lastTransitionTime, signal, weights, postSpikeStdpAdjusters),
        lastEvent = signal.timestamp
      )
    }
    else {
      // update the last event time and the membrane potential, but stay in the depolarized state
      stay using stateData.copy(
        lastEvent = signal.timestamp,
        membranePotential = updatedMembranePotential
      )
    }
  }

  /**
    * Calculates, updates, and logs the weights based on the spike-timing-dependent plasticity (stdp) function.
    * The method is called for pre-synaptic spikes the come before the post-synaptic neuron's (this neuron) spike,
    * and will cause the weight to increase.
    * (Put yourself into the perspective of the neuron that receives signals from upstream, pre-synaptic, neurons)
    *
    * @param previousFireEventTime The previous fire event (sets the time for t=0)
    * @param processedSignals      An array holding the pre-synaptic signals received since the last firing of this neuron
    * @param lastTransitionTime    The number of milliseconds from epoch of the most recent transition time from tonic
    *                              spiking.
    *                              The time should be the upper bound of all the pre-synaptic spikes that are being
    *                              processed by this call.
    * @param weights               A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param preSpikeStdpAdjusters stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                              ∆t = time between last two fire-events
    * @return The updated map of weights for each pre-synaptic neuron
    */
  private def preSpikeUpdatedWeights(previousFireEventTime: Time,
                                     processedSignals: Vector[Signal],
                                     lastTransitionTime: Time,
                                     weights: Map[String, Weight],
                                     preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double]): Map[String, Weight] = {
    // calculate new weights based on the signals that arrive before this neuron spiked
    val weightAdjustments = Neuron.preSpikeWeightUpdates(
      previousFireEventTime = previousFireEventTime,
      processedSignals = processedSignals,
      fireEventTime = lastTransitionTime,
      weights = weights,
      weightDecayFunction = weightDecayFunction,
      weightLimitFunction = weightLimitFunction,
      maxWeight = maxWeight,
      preSpikeStdpAdjusters = preSpikeStdpAdjusters
    )

    // log each of the weight updates
    weightAdjustments.foreach(entry => EventLogger.log(
      context.system.name,
      () => StdpWeightUpdated(
        neuronId = id,
        sourceId = entry._1,
        previousWeight = entry._2.previousWeight,
        newWeight = entry._2.newWeight.value,
        adjustment = entry._2.adjustment,
        timeWindow = entry._2.timeWindow,
        stdpTime = entry._2.stdpTime,
        signalTime = entry._2.signalTime
      )
    ))

    // merge the current weights map with the updated weights map
    weights ++ weightAdjustments.mapValues(entry => entry.newWeight)
  }

  /**
    * Calculates the weight-change based on the spike-timing-dependent plasticity function. This method is called when
    * signals arrive after the spike, but during the refractory period. (Put yourself into the
    * perspective of the neuron that receives signals from upstream, pre-synaptic, neurons)
    *
    * @param lastTransitionTime     The number of milliseconds from epoch of the most recent transition time from tonic
    *                               spiking.
    * @param signal                 The signal from the pre-synaptic neruon
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param postSpikeStdpAdjusters stdp(t) where t = signal time relative to last fire-event
    * @return The updated map of weights for each pre-synaptic neuron
    */
  private def postSpikeUpdateWeight(lastTransitionTime: Time,
                                    signal: Signal,
                                    weights: Map[String, Weight],
                                    postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double]): Map[String, Weight] = {
    val weightAdjustment: Option[StdpWeightAdjustment] = Neuron.postSpikeWeightUpdate(
      fireEventTime = lastTransitionTime,
      signal = signal,
      weights = weights,
      weightDecayFunction = weightDecayFunction,
      weightLimitFunction = weightLimitFunction,
      minWeight = minWeight,
      postSpikeStdpAdjusters = postSpikeStdpAdjusters
    )

    // log the weight update and update it if an update is present
    weightAdjustment.foreach(adjustedWeight => EventLogger.log(
      context.system.name,
      () => StdpWeightUpdated(
        neuronId = id,
        sourceId = signal.preSynaptic.path.name,
        previousWeight = adjustedWeight.previousWeight,
        newWeight = adjustedWeight.newWeight.value,
        adjustment = adjustedWeight.adjustment,
        timeWindow = adjustedWeight.timeWindow,
        stdpTime = adjustedWeight.stdpTime,
        signalTime = adjustedWeight.signalTime
      ))
    )

    // return the updated weights or the original weights
    weightAdjustment.map(adjustedWeight => weights + (signal.preSynaptic.path.name -> adjustedWeight.newWeight)).getOrElse(weights)
  }

  /**
    * Sends the spikes to the post-synaptic neurons
    * @param stateData The neuron's state data
    * @return The state and data for the finite-state machine
    */
  private def dispatchSpikes(stateData: StateData): FSM.State[BsiState, StateData] = {
    // create the outgoing signal and send it to all the post-synaptic neurons
    val spike = Signal(stateData.clock.now(), self, inhibitionFactor * spikePotential)
    stateData.connections.foreach(connection => {
      val delay = Duration(connection._2.toMilliseconds.toInt * timeFactor, MILLISECONDS)
      context.system.scheduler.scheduleOnce(delay, connection._1, spike)
    })

    // log the spike
    EventLogger.log(context.system.name, () => Spiked(
      neuronId = id,
      timestamp = spike.timestamp,
      signalIntensity = spikePotential,
      lastFireTime = stateData.lastFire
    ))

    // decay the membrane potential
    val updatedMembranePotential = decayMembranePotential(stateData.membranePotential, stateData.lastEvent, spike.timestamp, decayHalfLife)

    // if the membrane potential has dropped below the resting threshold, then we cancel the tonic firing
    if (updatedMembranePotential < restingThreshold) {
      // log the phase transition to the limit cycle
      EventLogger.log(context.system.name, () => PhaseTransition(
        neuronId = id,
        timestamp = spike.timestamp,
        transitionType = "resting",
        membranePotential = updatedMembranePotential,
        firingRate = Kilohertz(0)
      ))

      // update the state
      goto(Depolarized) using stateData.copy(
        membranePotential = updatedMembranePotential,
        lastTransitionToRest = spike.timestamp
      )
    }
    else {
      // set the timer for the next tonic spike
      timers.startSingleTimer(TonicSpikeTimer, TonicSpike(), (1.0 / tonicFiredRate.toKilohertz).toLong millis)

      stay using stateData.copy(membranePotential = updatedMembranePotential)
    }
  }
}

/**
  * Companion object for [[com.digitalcipher.spiked.neurons.BistableIntegrator]] class used for constructing validated instances
  */
object BistableIntegrator {

  /**
    * Constructs a [[com.digitalcipher.spiked.neurons.BistableIntegrator]] wrapped in an [[akka.actor.ActorRef]] instance
    *
    * @param id                   The unique ID of the neuron
    * @param timeFactor           The simulation time-factor (1 second simulation time is F seconds in real-time)
    * @param firingThreshold      The incoming current threshold, above which the neuron will transition to limit cycle
    *                             (i.e. tonic spiking)
    * @param restingThreshold     The incoming current threshold, below which the neuron will transition to its resting
    *                             statue (i.e. stop tonic spiking)
    * @param tonicFireRate        The spike rate for tonic firing
    * @param refractoryPeriod     The duration after a spike when the neuron will not spike
    * @param baseRefractoriness               The base-refractoriness constant (V • s) that determines the amount of the relative
    *                                         refractoriness after the refractory period.
    * @param minMembranePotential The minimum value allowed for the membrane potential
    * @param spikePotential       The spike intensity
    * @param decayHalfLife        The decay half-life of the membrane potential after receiving a signal
    * @param weightDecayFunction  The decay function describing the synapse (connection) weight decay after usage
    * @param weightLimitFunction  The bounding function for the synapse (connection) weights
    * @param inhibitory           `true` when the neuron is inhibitory (i.e. sends negative signals); `false` when neuron is
    *                             excitatory (default) and sends positive signals
    * @param membranePotentialNoiseMagnitude  The magnitude of the membrane potential noise
    * @param weightNoiseMagnitude             The magnitude of the weight noise
    * @param intrinsicPlasticityLearningRate  The rate of learning for the intrinsic plasticity
    * @param intrinsicPlasticityBase          The base of the intrinsic learning
    * @param intrinsicPlasticityDecayHalfLife The half-life of the intrinsic learning decay
    * @return An [[akka.actor.ActorRef]] wrapping a [[com.digitalcipher.spiked.neurons.BistableIntegrator]]
    */
  def props(id: String,
            timeFactor: Int,

            firingThreshold: ElectricPotential = Millivolts(15),
            restingThreshold: ElectricPotential = Millivolts(10),
            tonicFireRate: Frequency = Hertz(10),

            refractoryPeriod: Time = Milliseconds(20),
            baseRefractoriness: MagneticFlux = Webers(8e-8),

            minMembranePotential: ElectricPotential = Millivolts(0),
            spikePotential: ElectricPotential = Millivolts(1),

            decayHalfLife: Time = Milliseconds(250),
            riseHalfLife: Time = Milliseconds(2),
            weightDecayFunction: Time => Double = weights.decay.Exponential(Seconds(1.5)).decayFunction,
            weightLimitFunction: WeightLimiterFunction = Unbounded(),

            synapseTiming: SignalReleaseProbability = SignalReleaseProbability.default(),
            conductanceSpeed: Velocity = MetersPerSecond(10),
            inhibitory: Boolean = false,
            membranePotentialNoiseMagnitude: ElectricPotential = Millivolts(0.1),
            weightNoiseMagnitude: Double = 0,
            intrinsicPlasticityLearningRate: ElectricPotential = Microvolts(0),
            intrinsicPlasticityBase: ElectricPotential = Millivolts(0),
            intrinsicPlasticityDecayHalfLife: Time = Minutes(10)
           ): Props = {

    // validate and throw exception if invalid
    require(firingThreshold > Millivolts(0), "The spike threshold must be specified and must be a positive number")
    require(refractoryPeriod > Microseconds(1), "The refractory period must be larger than 1 µs")
    require(minMembranePotential >= Millivolts(0))

    // create new actor
    Props(
      new BistableIntegrator(
        timeFactor = timeFactor,
        neuronId = id,
        firingThreshold = firingThreshold,
        restingThreshold = restingThreshold,
        tonicFiredRate = tonicFireRate,
        refractoryPeriod = refractoryPeriod,
        baseRefractoriness = baseRefractoriness,
        minMembranePotential = minMembranePotential,
        spikePotential = spikePotential,
        decayHalfLife = decayHalfLife,
        riseHalfLife = riseHalfLife,
        weightDecayFunction = weightDecayFunction,
        weightLimitFunction = weightLimitFunction.limiterFunction,
        synapseTiming = synapseTiming,
        conductanceSpeed = conductanceSpeed,
        inhibitory = inhibitory,
        membranePotentialNoiseMagnitude = membranePotentialNoiseMagnitude,
        weightNoiseMagnitude = weightNoiseMagnitude,
        intrinsicPlasticityLearningRate = intrinsicPlasticityLearningRate,
        intrinsicPlasticityBase = intrinsicPlasticityBase,
        intrinsicPlasticityDecayHalfLife = intrinsicPlasticityDecayHalfLife
      ))
  }

  /**
    * The constant firing rate function
    *
    * @param rate The firing rate when the neuron is in the limit-cycle (tonic firing rate)
    * @return The constant [[squants.time.Frequency]] specified
    */
  def defaultFiringRate(rate: Frequency): ElectricPotential => Frequency = (_: ElectricPotential) => rate

  /**
    * Sigmoid firing frequency that depends on the membrane potential.<p>
    *
    * ν(V) = r,,min,, + (r,,max,, - r,,min,,) / ( 1 + exp( -(V - V,,off,,) / V,,f,, ) )
    *
    * <p>where<br>
    * ν(V) is the membrane-potential-dependent tonic frequency<br>
    * r,,min,, is the lowest tonic firing rate<br>
    * r,,max,, is the maximum tonic firing rate<br>
    * V is the membrane potential<br>
    * V,,off,, is the offset of the sigmoid's center (half max)<br>
    * V,,f,, is the flatness measure (1/slope)
    *
    * @param minRate  The lowest tonic firing rate
    * @param maxRate  The maximum tonic firing rate
    * @param invSlope The measure of flatness (i.e. 1/slope)
    * @param offset   The offset of the center frequency
    * @return The tonic firing rate as a [[squants.time.Frequency]]
    */
  def sigmoidFiringRate(minRate: Frequency,
                        maxRate: Frequency,
                        invSlope: ElectricPotential,
                        offset: ElectricPotential): ElectricPotential => Frequency =
    (potential: ElectricPotential) => minRate + (maxRate - minRate) / (1 + math.exp(-(potential - offset) / invSlope))

  /**
    * Holds the bistable integrator's state-data
    *
    * @param processedSignals       Holds a list of the processed signals for calculating learning (i.e. weight adjustments)
    * @param membranePotential      The current membrane potential of the neuron
    * @param lastEvent              The time of the last event
    * @param lastFire               The time of the last fire (spike)
    * @param lastTransitionToRest   The time of the last transition from tonic spiking to the resting state
    * @param connections            A map associating a connected post-synaptic neuron to the signal propagation time for the
    *                               spike to travel from the pre-synaptic neuron to the post-synaptic neuron
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param preSpikeStdpAdjusters  stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                               ∆t = time between last two fire-events
    * @param postSpikeStdpAdjusters stdp(t) where t = signal time relative to last fire-event
    * @param timeAdjustment         adjustment made to the initial time (added to the initial time) when calculating "now". for example,
    *                               if current is 100,001,000, initial time is 100,000,000, and adjustment is 0, then now would be 1,000.
    *                               if, in the same example, the time adjustment is 100, then now = 100,001,000 - 100,000,000 - 100 = 900.
    *                               a time adjustment of 100 would mean that this neuron's clock has drifted 100 ms into the future, and
    *                               the time adjustment of 100 ms is applied to correct that drift
    * @param intrinsicPlasticity    Neurons that have more spiking activity tend to fire more readily. This is the base value.
    * @param clock                  The clock defining elapsed times for the neuron
    */
  case class StateData(processedSignals: Vector[Signal],
                       membranePotential: ElectricPotential,
                       lastEvent: Time,
                       lastFire: Time,
                       lastTransitionToRest: Time,
                       // base neuron data
                       connections: Map[ActorRef, Time],
                       weights: Map[String, Weight],
                       preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double],
                       postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double],
                       timeAdjustment: Long,
                       intrinsicPlasticity: ElectricPotential,
                       clock: SignalClock
                      )

  def initialStateData(clock: SignalClock) = StateData(
    processedSignals = Vector.empty,
    membranePotential = Millivolts(0),
    lastEvent = Milliseconds(0),
    lastFire = Milliseconds(0),
    lastTransitionToRest = Milliseconds(0),
    // base neuron data
    connections = new HashMap[ActorRef, Time],
    weights = new HashMap[String, Weight],
    preSpikeStdpAdjusters = new HashMap[String, (Time, Time, Double, Double) => Double],
    postSpikeStdpAdjusters = new HashMap[String, (Time, Double, Double) => Double],
    timeAdjustment = 0,
    intrinsicPlasticity = Millivolts(0),
    clock = clock
  )

  sealed trait BsiState
  case object Depolarized extends BsiState
  case object Spiking extends BsiState
  case object Refractory extends BsiState

  case class TonicSignal()
  case object TonicSpikeTimer
  case class TonicSpike()

}
