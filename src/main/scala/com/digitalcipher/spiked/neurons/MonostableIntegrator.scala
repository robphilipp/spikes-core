package com.digitalcipher.spiked.neurons

import akka.actor.{ActorRef, FSM, Props}
import com.digitalcipher.spiked.logging._
import com.digitalcipher.spiked.logging.messages._
import com.digitalcipher.spiked.neurons.MonostableIntegrator.{Depolarized, MsiState, StateData, initialStateData}
import com.digitalcipher.spiked.neurons.Neuron._
import com.digitalcipher.spiked.neurons.SignalClock.{current, nowWithTimeFactor}
import com.digitalcipher.spiked.neurons.SignalReceiver.{IdQuery, TimeFactorQuery}
import com.digitalcipher.spiked.neurons.weights.limit.{Unbounded, WeightLimiterFunction}
import com.digitalcipher.spiked.construction.description.LearningFunctionDescription
import squants.Time
import squants.electro._
import squants.motion.{MetersPerSecond, Velocity}
import squants.space.Length
import squants.time.{Microseconds, Milliseconds, Minutes, Seconds}

import scala.collection.immutable.HashMap
import scala.concurrent.duration._
import scala.util.Random

/**
  * Monostable integrator.
  *
  * Use the companion object [[com.digitalcipher.spiked.neurons.MonostableIntegrator]] `from(...)` method to construct an instance
  *
  * @constructor Creates a neuron with the specified properties
  * @param timeFactor           The simulation time-factor (1 second simulation time is F seconds in real-time)
  * @param neuronId             The unique ID of this neuron
  * @param threshold            The membrane potential threshold above which the neuron fires a spike
  * @param refractoryPeriod     The refractory period after a fire event
  * @param baseRefractoriness   The base-refractoriness constant (V • s) that determines the amount of the relative
  *                             refractoriness after the refractory period.
  * @param minMembranePotential The minimum allowed value for the membrane potential
  * @param spikePotential       The spike intensity
  * @param decayHalfLife        The decay half-life of the membrane potential
  * @param weightDecayFunction  The function that describes the synapse connection weight decay
  * @param weightLimitFunction  The function that sets the lower and upper bounds of the connection weights
  * @param conductanceSpeed     The speed at which a signal propagates down the axon
  * @param inhibitory           `true` when the neuron is inhibitory (i.e. sends negative signals); `false` when neuron is
  *                             excitatory (default) and sends positive signals
  * @param seed The seed for the random number generator
  * @param membranePotentialNoiseMagnitude The magnitude of the membrane potential noise
  * @param weightNoiseMagnitude The magnitude of the weight noise
  * @param intrinsicPlasticityLearningRate The rate of learning for the intrinsic plasticity
  * @param intrinsicPlasticityBase The base of the intrinsic learning
  * @param intrinsicPlasticityDecayHalfLife The half-life of the intrinsic learning decay
  */
class MonostableIntegrator(timeFactor: Int,
                           neuronId: String,
                           threshold: ElectricPotential,
                           refractoryPeriod: Time = Milliseconds(20),
                           baseRefractoriness: MagneticFlux = Webers(8e-8),
                           minMembranePotential: ElectricPotential,
                           spikePotential: ElectricPotential,
                           decayHalfLife: Time,
                           riseHalfLife: Time,
                           weightDecayFunction: Time => Double,
                           weightLimitFunction: Double => Double,
                           synapseTiming: SignalReleaseProbability,
                           conductanceSpeed: Velocity,
                           inhibitory: Boolean,
                           seed: Long = 0,
                           membranePotentialNoiseMagnitude: ElectricPotential,
                           weightNoiseMagnitude: Double,
                           intrinsicPlasticityLearningRate: ElectricPotential,
                           intrinsicPlasticityBase: ElectricPotential,
                           intrinsicPlasticityDecayHalfLife: Time) extends FSM[MsiState, StateData] {

  // calculate the min and max allowed connection weight values based on the specified weight limit functions
  private val maxWeight: Double = weightLimitFunction(Double.MaxValue)
  private val minWeight: Double = weightLimitFunction(Double.MinValue)

  // the inhibition factor that multiples the outgoing signal strength
  private val inhibitionFactor: Int = if (inhibitory) -1 else 1

  private val random: Random = new Random(if (seed == 0) current() else seed)

  // called during construction to set the initial state
  startWith(Depolarized, initialStateData(clock = nowWithTimeFactor(timeFactor), synapseTiming = synapseTiming))

  // when a message arrives and the neuron is in the depolarized state
  when(Depolarized) {
    case Event(Signal(_, origin, value), data: StateData) => processSignal(origin, value, data)
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
    * Updates the state for the monostable integrator neuron
    * @param processedSignals Holds a list of the processed signals for calculating learning (i.e. weight adjustments)
    * @param membranePotential The current membrane potentional of the neuron
    * @param lastEvent The time of the last event
    * @param lastFire The time of the last fire (spike)
    * @param releases The map holding the last release for each post-synaptic neuron
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
                              (processedSignals: Vector[Signal] = data.processedSignals,
                               membranePotential: ElectricPotential = data.membranePotential,
                               lastEvent: Time = data.lastEvent,
                               lastFire: Time = data.lastFire,
                               releases: Map[String, Time] = data.releases,
                               synapseTiming: SignalReleaseProbability = data.synapseTiming,
                               // base neuron state
                               connections: Map[ActorRef, Time] = data.connections,
                               weights: Map[String, Weight] = data.weights,
                               preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double] = data.preSpikeStdpAdjusters,
                               postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double] = data.postSpikeStdpAdjusters,
                               timeAdjustment: Long = data.timeAdjustment,
                               intrinsicPlasticity: ElectricPotential = data.intrinsicPlasticity,
                               clock: SignalClock = data.clock): StateData = {
    stateData.copy(
      processedSignals = processedSignals,
      membranePotential = membranePotential,
      lastEvent = lastEvent,
      lastFire = lastFire,
      releases = releases,
      synapseTiming = synapseTiming,
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

  import context.dispatcher

  /**
    * Sets the initial time to the value specified, and calculates that time adjustment
    *
    * @param time The number of milliseconds since epoch
    */
  private def initializeTime(time: Long, stateData: StateData): FSM.State[MsiState, StateData] = {
    val timing: (SignalClock, Long) = initializedTime(time, stateData.clock)
    stay using updatedStateData(stateData)(clock = timing._1, timeAdjustment = timing._2)
  }

  // todo make this a running average or some algorithm that smoothly adjusts the times
  /**
    * Calculates the time adjustment based on the current time and the specified time
    *
    * @param time The number of milliseconds since epoch
    */
  private def synchronizeTime(time: Long, stateData: StateData): FSM.State[MsiState, StateData] =
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
                                  stateData: StateData): FSM.State[MsiState, StateData] = {

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
                                  stateData: StateData): FSM.State[MsiState, StateData] = {
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
    * @param origin The pre-synaptic neuron
    * @param value The signal strength (voltage)
    * @param stateData The current neuron state data
    * @return The updated FSM state
    */
  private def processSignal(origin: ActorRef, value: ElectricPotential, stateData: StateData): FSM.State[MsiState, StateData] = {
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
    val (weight, weights) = if (origin == SignalReceiver.world) {
      (Weight.world, stateData.weights)
    } else {
      // update the weights that have evolved due to noise as a Wiener process
      val weight = Neuron.weightUpdatedForNoise(
        preSynaptic = origin,
        elapsedTime = timestamp - stateData.lastEvent,
        weightNoiseMagnitude = weightNoiseMagnitude,
        random = random,
        weight = stateData.weights(origin.path.name),
        weightDecayFunction = weightDecayFunction,
        weightLimitFunction = weightLimitFunction
      )

      // return the updated weight and the map of weights to which this was added/overwritten
      (weight, stateData.weights + (origin.path.name -> weight))
    }

    // create the signal for updating the membrane potential
    val signal = Signal(timestamp, origin, weight.value * value)

    // update the membrane potential and return the FSM state
    updateMembranePotential(
      stateData = stateData,
      signal = signal,
      clock = stateData.clock,
      lastEvent = stateData.lastEvent,
      lastFire = stateData.lastFire,
      processedSignals = stateData.processedSignals,
      membranePotential = stateData.membranePotential,
      intrinsicPlasticity = stateData.intrinsicPlasticity,
      releases = stateData.releases,
      synapseTiming = stateData.synapseTiming,
      connections = stateData.connections,
      weights = weights,
      preSpikeStdpAdjusters = stateData.preSpikeStdpAdjusters,
      postSpikeStdpAdjusters = stateData.postSpikeStdpAdjusters
    )
  }

  /**
    * Updates the membrane potential if the neuron is beyond the refractory period. If the membrane potential exceeds
    * the threshold, then fires a spike to all its post-synaptic neurons.
    * <p>
    * Note that the logged learning events may be out of order because of the way stdp is processed for pre-spike
    * events. Pre-spike stdp (learning) events are not processed and logged until the neuron fires, and post-spike
    * learning events are processed as they occur.
    *
    * @param stateData The neuron state data
    * @param signal    The [[Signal]] that holds the timestamp of when it was received and the intensity (value)
    * @param lastEvent The timestamp of the last time a signal was received
    * @param lastFire  The timestamp of the last time this neuron fired
    * @param processedSignals Holds a list of the processed signals for calculating learning (i.e. weight adjustments)
    * @param membranePotential The current membrane potential of the neuron
    * @param intrinsicPlasticity    Neurons that have more spiking activity tend to fire more readily. This is the base value.
    * @param releases The map holding the last release for each post-synaptic neuron
    * @param connections            A map associating a connected post-synaptic neuron to the signal propagation time for the
    *                               spike to travel from the pre-synaptic neuron to the post-synaptic neuron
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param preSpikeStdpAdjusters  stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                               ∆t = time between last two fire-events
    * @param postSpikeStdpAdjusters stdp(t) where t = signal time relative to last fire-event
    * @return The updated neuron state data
    */
  private def updateMembranePotential(stateData: StateData,
                                      signal: Signal,
                                      clock: SignalClock,
                                      lastEvent: Time,
                                      lastFire: Time,
                                      processedSignals: Vector[Signal],
                                      membranePotential: ElectricPotential,
                                      intrinsicPlasticity: ElectricPotential,
                                      releases: Map[String, Time],
                                      synapseTiming: SignalReleaseProbability,

                                      // base neuron data
                                      connections: Map[ActorRef, Time],
                                      weights: Map[String, Weight],
                                      preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double],
                                      postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double]): FSM.State[MsiState, StateData] = {
    // add to the list of signals needed for the excitatory learning (pre-spike STDP)
    val signals = processedSignals :+ signal

    //    println(s"update membrane potential; id: $id; t: ${signal.timestamp}; u: $membranePotential; ip: $intrinsicPlasticity")

    // add the noise contribution (Wiener process) and then decay the membrane potential to evolve
    // the membrane potential since the last event
    val decayedMembranePotential = decayMembranePotential(
      activationPotential = membranePotential + Neuron.noise(signal.timestamp - lastEvent, membranePotentialNoiseMagnitude, random),
      lastEventTime = lastEvent,
      currentEventTime = signal.timestamp,
      decayHalfLife = decayHalfLife
    )

    // add the response to the pre-synaptic signal to the membrane potential. the response is the magnitude of the
    // pre-synaptic signal times the connection weight between this and the pre-synaptic neuron
    var potential = minMembranePotential.max(signal.value + decayedMembranePotential)

    // adjust the membrane potential for refractoriness
    potential = Neuron
      .refractorinessAdjusted(
        signalTime = signal.timestamp,
        lastFire = lastFire,
        membranePotential = potential,
        refractoryPeriod = refractoryPeriod,
        baseRefractoriness = baseRefractoriness
      )
      .max(minMembranePotential)

    //    println(s"-->; id: $id; t: ${signal.timestamp}; u: $membranePotential; ip: $intrinsicPlasticity")

    // decay the intrinsic plasticity since the last event
    var plasticity: ElectricPotential = Neuron.decayedIntrinsicPlasticity(
      deltaTime = signal.timestamp - lastEvent,
      plasticity = intrinsicPlasticity,
      decayHalfLife = intrinsicPlasticityDecayHalfLife
    )

    //    println(s"---->; id: $id; t: ${signal.timestamp}; u: $membranePotential; ip: $plasticity")

    // log the membrane potential update
    EventLogger.log(context.system.name, () => MembranePotentialUpdate(
      neuronId = id,
      timestamp = signal.timestamp,
      lastEventTime = lastEvent,
      lastFireTime = lastFire,
      membranePotential = potential
    ))

    // todo here we need to do a transition to refractory, and pull the spiking code into the onTransition() method
    // if the membrane potential is exceeded then this neuron may send a spike
    if (potential + plasticity >= threshold) {
      //      println(s"**fire**; id: $id; t: ${signal.timestamp}; u: $membranePotential; ip: $plasticity")

      // add the action potential to the synapse timing so that it can update the facilitation
      var releaseProb = synapseTiming.addActionPotentialEvent(signal.timestamp, lastEvent)

      // mark the time of this firing so that we can calculate the time delta for the next signal
      val lastNeuronFireEvent = clock.now()

      // add the fire event to the synapse timing so that it can recalculate the depletion
      releaseProb = releaseProb.addFireEvent(lastNeuronFireEvent, lastFire)

      // the response of the neuron to an incoming spike has a finite rise time given by a
      // response kernel that looks similar to the α-function. with event-based approach, we can really
      // only calculate how long it takes for the membrane potential to rise through the threshold,
      // if it does. this rise time results in a delay in which the signal will be sent to its post-synaptic
      // neurons.
      val riseDelay = calculateRiseTimeDelay(decayedMembranePotential, potential + plasticity, threshold, riseHalfLife)

      //      println(s"**-->; id: $id; t: ${signal.timestamp}; u: $membranePotential; ip: $intrinsicPlasticity; rd: $riseDelay")

      // reset the membrane potential to it's resting state
      potential = minMembranePotential

      // log the spike
      EventLogger.log(context.system.name, () => Spiked(
        neuronId = id,
        timestamp = lastNeuronFireEvent + riseDelay,
        signalIntensity = inhibitionFactor * spikePotential,
        lastFireTime = lastFire
      ))

      // update the intrinsic plasticity
      plasticity = Neuron.updatedIntrinsicPlasticity(
        plasticity = intrinsicPlasticity,
        learningRate = intrinsicPlasticityLearningRate,
        basePlasticity = intrinsicPlasticityBase
      )

      // log the intrinsic plasticity for the neuron
      EventLogger.log(context.system.name, () => IntrinsicPlasticityUpdated(
        neuronId = id,
        timestamp = lastNeuronFireEvent,
        intrinsicPlasticity = plasticity
      ))

      // at this point the neuron has created an action potential that travels down the axon
      // to the synapses. for each synapse, we calculate the release probability and then,
      // based on that, role the die to see if the synapse releases a vesicle containing
      // neurotransmitters.
      val updatedReleases = sendSpikesStochastically(
        connections = connections,
        releases = releases,
        signalTiming = releaseProb,
        signal = signal,
        clock = clock,
        riseDelay = riseDelay,
        lastEvent = lastEvent,
        lastFire = lastFire,
        lastNeuronFireEvent = lastNeuronFireEvent
      )

      // update the synapse weights (membrane potential response multipliers) for the pre-synaptic signals
      val updatedWeights = preSpikeUpdatedWeights(
        previousFireEventTime = lastFire,
        processedSignals = signals,
        fireEventTime = lastNeuronFireEvent,
        weights = weights,
        preSpikeStdpAdjusters = preSpikeStdpAdjusters
      )

      stay using stateData.copy(
        membranePotential = potential,
        intrinsicPlasticity = plasticity,
        processedSignals = Vector.empty,
        weights = updatedWeights,
        releases = releases ++ updatedReleases,
        synapseTiming = releaseProb,
        lastEvent = signal.timestamp,
        lastFire = lastNeuronFireEvent
      )
    }
    else {
      // update the weight for post-spike signals (decreases the weight)
      val updatedWeights = postSpikeUpdateWeight(
        fireEventTime = lastFire,
        signal = signal,
        weights = weights,
        postSpikeStdpAdjusters = postSpikeStdpAdjusters
      )

      stay using stateData.copy(
        membranePotential = potential,
        intrinsicPlasticity = plasticity,
        processedSignals = signals,
        weights = updatedWeights,
        lastEvent = signal.timestamp,
        lastFire = lastFire
      )
    }
  }

  /**
    * When the neuron's membrane potential exceeds its threshold, the neuron spikes. The action potential
    * travels down the axon to its synapses, which release vesicles into the synaptic cleft with some
    * probability given by the release probability. In this case, for each post-synaptic synapse,
    * we calculate whether the neuron transmitters were released into the synaptic cleft. Algorithmically,
    * for each synapse, we calculate the release probability and then, based on that, role the die to see if
    * the synapse releases a vesicle containing neurotransmitters.
    * @param connections The connections to this neuron's post-synaptic neurons, along with the delay times
    * @param releases The previous release time for each of the post-synaptic neurons
    * @param signalTiming The release probability based on neuron timing
    * @param signal The signal to send to each neuron
    * @param clock The network clock
    * @param riseDelay The rise time for the spike
    * @param lastEvent The time of the last incoming signal
    * @param lastFire The last time the neuron fired
    * @param lastNeuronFireEvent The time before the last neuron fire
    * @return
    */
  private def sendSpikesStochastically(connections: Map[ActorRef, Time],
                                       releases: Map[String, Time],
                                       signalTiming: SignalReleaseProbability,
                                       signal: Signal,
                                       clock: SignalClock,
                                       riseDelay: Time,
                                       lastEvent: Time,
                                       lastFire: Time,
                                       lastNeuronFireEvent: Time): Iterable[(String, Time)] = {
    connections.map(connection => {
      val postSynapticNeuron = connection._1

      // calculate the release probability based on the last release (or last fire time if the
      // synapse has not yet fired)
      val lastRelease = releases.getOrElse(postSynapticNeuron.path.name, lastFire)
      val probabilityOfRelease = synapseTiming.releaseProbability(signal.timestamp, lastEvent, lastRelease)

      // calculate whether the synapse fires (stochastic) based on its release probability
      if (Neuron.releasesVesicles(probabilityOfRelease, random)) {
        // set the scheduler to send the message after the calculated delay (in real time as opposed to simulation
        // time). the delay represents the signal propagation delay and the incoming spike response time (i.e.
        // the riseDelay)
        val propagationTime = connection._2
        val delay = Duration(
          math.round((propagationTime + riseDelay).toMilliseconds) * clock.timeFactor,
          MILLISECONDS
        )

        // if the delay (which is real time rather than simulation time) is less than a millisecond, then
        // don't waste the scheduler, and just send along the message without any additional delay
        if (delay.toMicros >= 200) {
          // create the outgoing signal and send it to the post-synaptic neuron (timestamped with the
          // last fire event, plus the propagation delay to the synapse, plus the delay to account
          // of the rise-time of the membrane potential)
          context.system.scheduler.scheduleOnce(delay) {
            postSynapticNeuron ! Signal(clock.now(), self, inhibitionFactor * spikePotential)
          }
        } else {
          // create the outgoing signal and send it to the post-synaptic neuron (timestamped with the
          // last fire event, plus the propagation delay to the synapse, plus the delay to account
          // of the rise-time of the membrane potential)
          postSynapticNeuron ! Signal(lastNeuronFireEvent, self, inhibitionFactor * spikePotential)
        }

        Option(postSynapticNeuron.path.name -> (lastNeuronFireEvent + riseDelay))
      }
      else {
        Option.empty
      }
    }).filter(result => result.nonEmpty).map(result => result.get)
  }

  /**
    * Calculates, updates, and logs the weights based on the spike-timing-dependent plasticity (stdp) function.
    * The method is called for pre-synaptic spikes the come before the post-synaptic neuron's (this neuron) spike,
    * and will cause the weight to increase.
    * (Put yourself into the perspective of the neuron that receives signals from upstream, pre-synaptic, neurons)
    *
    * @param previousFireEventTime The previous fire event (sets the time for t=0)
    * @param processedSignals      An array holding the pre-synaptic signals received since the last firing of this neuron
    * @param fireEventTime         The number of milliseconds from epoch of the recent spike of the post-synaptic neuron.
    *                              The time should be the upper bound of all the pre-synaptic spikes that are being
    *                              processed by this call.
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param preSpikeStdpAdjusters  stdp(t, ∆t) where t = signal time relative to fire-event prior to the last one;
    *                               ∆t = time between last two fire-events
    * @return The updated map of weights for each pre-synaptic neuron
    */
  private def preSpikeUpdatedWeights(previousFireEventTime: Time,
                                     processedSignals: Vector[Signal],
                                     fireEventTime: Time,
                                     weights: Map[String, Weight],
                                     preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double]): Map[String, Weight] = {
    // calculate new weights based on the signals that arrive before this neuron spiked
    val weightAdjustments = Neuron.preSpikeWeightUpdates(
      previousFireEventTime = previousFireEventTime,
      processedSignals = processedSignals,
      fireEventTime = fireEventTime,
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
    * @param fireEventTime The spike time
    * @param signal        The signal
    * @param weights                A map associating the pre-synaptic neuron connection with a efficacy (weight)
    * @param postSpikeStdpAdjusters stdp(t) where t = signal time relative to last fire-event
    * @return The updated map of weights for each pre-synaptic neuron
    */
  private def postSpikeUpdateWeight(fireEventTime: Time,
                                    signal: Signal,
                                    weights: Map[String, Weight],
                                    postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double]): Map[String, Weight] = {
    val weightAdjustment: Option[StdpWeightAdjustment] = Neuron.postSpikeWeightUpdate(
      fireEventTime = fireEventTime,
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
}

/**
  * Companion object for [[com.digitalcipher.spiked.neurons.MonostableIntegrator]] class used for constructing validated instances
  */
object MonostableIntegrator {

  /**
    * Constructs a [[com.digitalcipher.spiked.neurons.MonostableIntegrator]] wrapped in an [[akka.actor.ActorRef]] instance
    *
    * @param id                   The unique ID of the neuron
    * @param timeFactor           The simulation time-factor (i.e. for a time factor of N, 1 second simulation time
    *                             takes N seconds in real-time to run)
    * @param threshold            The membrane potential threshold, above which the neuron will spike
    * @param refractoryPeriod     The duration after a spike when the neuron will not spike
    * @param minMembranePotential The minimum value allowed for the membrane potential
    * @param spikePotential       The spike intensity
    * @param decayHalfLife        The decay half-life of the membrane potential after receiving a signal (response kernel)
    * @param riseHalfLife         The rise half-life of the membrane potential after receiving a signal (response kernel)
    * @param weightDecayFunction  The decay function describing the synapse (connection) weight decay after usage
    * @param weightLimitFunction  The bounding function for the synapse (connection) weights
    * @param conductanceSpeed     The axon conductance speed (which determines the signal delay)
    * @param inhibitory           `true` when the neuron is inhibitory (i.e. sends negative signals); `false` when neuron is
    *                             excitatory (default) and sends positive signals
    * @return An [[akka.actor.ActorRef]] wrapping a [[com.digitalcipher.spiked.neurons.MonostableIntegrator]]
    */
  def props(id: String,
            timeFactor: Int,

            threshold: ElectricPotential = Millivolts(10),

            refractoryPeriod: Time = Milliseconds(20),
            baseRefractoriness: MagneticFlux = Webers(8e-8),

            minMembranePotential: ElectricPotential = Millivolts(0),
            spikePotential: ElectricPotential = Millivolts(0),
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
    require(threshold > Millivolts(0), "The spike threshold must be specified and must be a positive number")
    require(refractoryPeriod > Microseconds(1), "The refractory period must be larger than 1 µs")
    require(minMembranePotential >= Millivolts(0))

    // create new actor
    Props(
      new MonostableIntegrator(
        timeFactor = timeFactor,
        neuronId = id,
        threshold = threshold,
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

  case class NeuronState(membranePotential: ElectricPotential, lastFire: Time, lastEvent: Time)

  /**
    * Holds the monostable integrator's state-data
    *
    * @param processedSignals       Holds a list of the processed signals for calculating learning (i.e. weight adjustments)
    * @param membranePotential      The current membrane potentional of the neuron
    * @param lastEvent              The time of the last event
    * @param lastFire               The time of the last fire (spike)
    * @param releases               The map holding the last release for each post-synaptic neuron
    * @param synapseTiming          The release probability and signal timing to calculate the release probability
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
                       releases: Map[String, Time],
                       synapseTiming: SignalReleaseProbability,
                       // base neuron data
                       connections: Map[ActorRef, Time],
                       weights: Map[String, Weight],
                       preSpikeStdpAdjusters: Map[String, (Time, Time, Double, Double) => Double],
                       postSpikeStdpAdjusters: Map[String, (Time, Double, Double) => Double],
                       timeAdjustment: Long,
                       intrinsicPlasticity: ElectricPotential,
                       clock: SignalClock
                      )

  /**
    * Creates the initial state-data for the neuron
    * @param clock    The neuron's clock
    * @param synapseTiming The release probability and signal timing to calculate the release probability
    * @return
    */
  def initialStateData(clock: SignalClock, synapseTiming: SignalReleaseProbability): StateData = StateData(
    processedSignals = Vector.empty,
    membranePotential = Millivolts(0),
    lastEvent = Milliseconds(0),
    lastFire = Milliseconds(0),
    releases = new HashMap[String, Time],
    synapseTiming = synapseTiming,

    connections = new HashMap[ActorRef, Time],
    weights = new HashMap[String, Weight],
    preSpikeStdpAdjusters = new HashMap[String, (Time, Time, Double, Double) => Double],
    postSpikeStdpAdjusters = new HashMap[String, (Time, Double, Double) => Double],
    timeAdjustment = 0,
    intrinsicPlasticity = Millivolts(0),
    clock = clock
  )

  /**
    * Neuron state
    */
  sealed trait MsiState
  case object Depolarized extends MsiState
//  case object Refractory extends MsiState
}
