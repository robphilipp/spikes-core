package com.digitalcipher.spiked.neurons

import squants.Time
import squants.time.Milliseconds

/**
  * <p>Immutable</p>
  *
  * Represents the time-dependence of the synapse by modelling the depletion of vesicles and the facilitation
  * increased calcium concentrations that result from action potentials arriving at the pre-synaptic neuron's synapse.
  * The release-probability, given an action potential at t,,i,, is modelled as
  * <p>
  * p,,S,,(t,,i,,) = 1 - e^-C(t,,i,,) V(t,,i,,)^
  * </p>
  * where C(t) is the facilitation and V(t) is the depletion. Physiologically, the facilitation could be driven
  * by the increased calcium brought by arriving action potentials, and the depletion could result from the
  * depletion of the pool of vesicles caused by a release (spike) into the synaptic cleft.
  * <p>
  * The facilitation at some time t on or after that last action potential is given by<p>
  * C(t) = C,,0,, + α ∑ exp(-(t - t,,i,,) / τ,,c,,)
  * </p>
  * where C,,0,, is the base, α is the magnitude, and τ,,c,, is the time-constant. The sum runs over all
  * the action-potential times t,,i,, for which t,,i,, ≤ t.
  * <p>
  * The depletion is given by<p>
  * V(t) = max(0, V,,0,, - β ∑ exp(-(t - t,,i,,) / τ,,v,,))
  * </p>
  * where V,,0,, is the base, β is the magnitude, and τ,,v,, is the time-constant. The sum runs over all
  * the action-potential times t,,i,, for which t,,i,, ≤ t, and for which the action potential caused a
  * release.
  * <p>
  * Calls to the `releaseProbability(...)` method reflect the current release probability, with the facilitation
  * and depletion decayed. When an action potential occurs, call the `addActionPotential(...)` method. When a
  * release event occurs, then call the `addDepletion(...)` method.
  *
  * @param facilitatorBase          The base facilitation value to which the facilitation decays, C(0)
  * @param facilitatorMagnitude     The magnitude of the facilitation response to an action potential, α
  * @param facilitationTimeConstant The facilitation response decay time-constant, τ,,c,,
  * @param depletionBase            The base depletion value to which the depletion response decays, V(0)
  * @param depletionMagnitude       The magnitude of the depletion in response to a release (i.e spike), β (often set to 1)
  * @param depletionTimeConstant    The depletion response decay time-constant, τ,,v,,
  * @param facilitationSum The current facilitation sum (defaults to 0)
  * @param depletionSum The current depletion sum (defaults to 0)
  */
class SignalReleaseProbability(val facilitatorBase: Double,
                               val facilitatorMagnitude: Double,
                               val facilitationTimeConstant: Time,
                               val depletionBase: Double,
                               val depletionMagnitude: Double,
                               val depletionTimeConstant: Time)(val facilitationSum: Double = 0, val depletionSum: Double = 0) {

  /**
    * Calculates the probability that a synapse which received a pre-synaptic action potential will release the
    * neurotransmitter vesicles into the synaptic cleft. Action potentials are generated by pre-synaptic
    * neuron whose membrane potential u,,m,, exceeds its threshold u,,τ,,,. The synapse of such a neuron releases
    * neurotransmitters if a chosen random number r ∈ [0,1] satisfies r < p,,S,,.
    *
    * The release-probability, given an action potential at t,,i,, is modelled as
    * <p>
    * p,,S,,(t,,i,,) = 1 - e^-C(t,,i,,) V(t,,i,,)^
    * </p>
    * where C(t) is the facilitation and V(t) is the depletion.
    *
    * @param time                    The current time of the signal
    * @param lastActionPotentialTime The last time the membrane potential exceeded the threshold, i.e. u,,m,, ≥ u,,τ,,
    * @param lastReleaseTime         The time of the last release (i.e. last spike time)
    * @return The probability of a release given that an action potential occurred
    */
  def releaseProbability(time: Time, lastActionPotentialTime: Time, lastReleaseTime: Time): Double = {
    1 - math.exp(-facilitation(time, lastActionPotentialTime) * depletion(time, lastReleaseTime))
  }

  /**
    * The current facilitation value
    *
    * @param time     The current time
    * @param lastTime The time of the last action potential
    * @return The current (decayed) facilitation value
    */
  def facilitation(time: Time, lastTime: Time): Double =
    facilitatorBase + decayedFacilitationSum(time - lastTime)

  /**
    * The current depletion value
    *
    * @param time     The current time
    * @param lastTime The time of the last release (i.e. spike time)
    * @return The current (decayed) depletion value
    */
  def depletion(time: Time, lastTime: Time): Double =
    math.max(0, depletionBase - decayedDepletionSum(time - lastTime))

  /**
    *
    * @param time The time since the last the action potential
    * @return The facilitation value decayed for the elapsed time
    */
  private def decayedFacilitationSum(time: Time): Double =
    facilitationSum * math.exp(-time / facilitationTimeConstant)

  /**
    * Calculates the value of the depletion sum at the given time
    *
    * @param time The time since that last vesicle release (i.e. time from last spike)
    * @return The depletion value decayed for the elapsed time
    */
  private def decayedDepletionSum(time: Time): Double =
    depletionSum * math.exp(-time / depletionTimeConstant)

  /**
    * Updates the facilitation by first decaying the existing facilitation sum and then add the facilitation
    * magnitude. Then returns the current facilitation (including the base). The facilitation is given by<p>
    * C(t) = C,,0,, + α ∑ exp(-(t - t,,i,,) / τ,,c,,)
    * </p>
    * where C,,0,, is the base, α is the magnitude, and τ,,c,, is the time-constant. The sum runs over all
    * the action-potential times t,,i,, for which t,,i,, ≤ t.
    *
    * @param time     The time of the current action potential
    * @param lastTime The time of the previous action potential
    * @return A copy of the updated release-probability
    */
  def addActionPotentialEvent(time: Time, lastTime: Time): SignalReleaseProbability = {
    val sum = decayedFacilitationSum(time - lastTime) + facilitatorMagnitude
    update(facilitationSum = sum + facilitatorBase)
  }

  /**
    * Updates the depletion by first decaying the existing depletion sum and then adding the depletion
    * magnitude. Then returns the current depletion (including the base). The depletion is given by<p>
    * V(t) = max(0, V,,0,, - β ∑ exp(-(t - t,,i,,) / τ,,v,,))
    * </p>
    * where V,,0,, is the base, β is the magnitude, and τ,,v,, is the time-constant. The sum runs over all
    * the action-potential times t,,i,, for which t,,i,, ≤ t, and for which the action potential caused a
    * release.
    *
    * @param time     The time of the current action potential
    * @param lastFire The time of the previous vesicle release
    * @return A copy of the updated release-probability
    */
  def addFireEvent(time: Time, lastFire: Time): SignalReleaseProbability = {
    val sum = decayedDepletionSum(time - lastFire) + depletionMagnitude
    update(depletionSum = math.max(0, depletionBase - sum))
  }

  /**
    * Makes a copy of the current release-probability with the updated facilitation and/or depletion sum specified
    * @param facilitationSum The optional facilitation sum
    * @param depletionSum The optional depletion sum
    * @return A copy of the updated release-probability
    */
  private def update(facilitationSum: Double = facilitationSum, depletionSum: Double = depletionSum): SignalReleaseProbability =
    SignalReleaseProbability.copyWith(this)(
      facilitationSum = facilitationSum,
      depletionSum = depletionSum
    )


  def canEqual(other: Any): Boolean = other.isInstanceOf[SignalReleaseProbability]

  override def equals(other: Any): Boolean = other match {
    case that: SignalReleaseProbability =>
      (that canEqual this) &&
        facilitatorBase == that.facilitatorBase &&
        facilitatorMagnitude == that.facilitatorMagnitude &&
        facilitationTimeConstant == that.facilitationTimeConstant &&
        depletionBase == that.depletionBase &&
        depletionMagnitude == that.depletionMagnitude &&
        depletionTimeConstant == that.depletionTimeConstant &&
        facilitationSum == that.facilitationSum &&
        depletionSum == that.depletionSum
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(facilitatorBase, facilitatorMagnitude, facilitationTimeConstant, depletionBase, depletionMagnitude, depletionTimeConstant, facilitationSum, depletionSum)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**
  * Companion object for creating default synapse timing functions
  */
object SignalReleaseProbability {

  /**
    * Constructs a [[SignalReleaseProbability]] instance with default settings
    *
    * @return A default [[SignalReleaseProbability]] instance
    */
  def default(): SignalReleaseProbability = new SignalReleaseProbability(
    facilitatorBase = 5,
    facilitatorMagnitude = 0.1,
    facilitationTimeConstant = Milliseconds(100),
    depletionBase = 10,
    depletionMagnitude = 1,
    depletionTimeConstant =  Milliseconds(100)
  )()

  /**
    * Constructs a [[SignalReleaseProbability]] class based on the specified parameters
    *
    * @param facilitatorBase          The base facilitation value to which the facilitation decays, C(0)
    * @param facilitatorMagnitude     The magnitude of the facilitation response to an action potential, α
    * @param facilitationTimeConstant The facilitation response decay time-constant, τ,,c,,
    * @param depletionBase            The base depletion value to which the depletion response decays, V(0)
    * @param depletionMagnitude       The magnitude of the depletion in response to a release (i.e spike), β (often set to 1)
    * @param depletionTimeConstant    The depletion response decay time-constant, τ,,v,,
    * @return A [[SignalReleaseProbability]] instance
    */
  def from(facilitatorBase: Double,
           facilitatorMagnitude: Double,
           facilitationTimeConstant: Time,
           depletionBase: Double,
           depletionMagnitude: Double,
           depletionTimeConstant: Time) =
    new SignalReleaseProbability(
      facilitatorBase = facilitatorBase,
      facilitatorMagnitude = facilitatorMagnitude,
      facilitationTimeConstant = facilitationTimeConstant,
      depletionBase = depletionBase,
      depletionMagnitude = depletionMagnitude,
      depletionTimeConstant = depletionTimeConstant
    )()

  /**
    * Makes a copy of the release probability, updating the facilitation and/or depletion sum
    * @param signalReleaseProbability The release probability
    * @param facilitationSum The current facilitation sum (defaults to the current value)
    * @param depletionSum The current depletion sum (defaults to the current value)
    * @return A copy of the release probability with the optionally updated facilitation and/or depletion sum
    */
  def copyWith(signalReleaseProbability: SignalReleaseProbability)(facilitationSum: Double, depletionSum: Double) =
    new SignalReleaseProbability(
      facilitatorBase = signalReleaseProbability.facilitatorBase,
      facilitatorMagnitude = signalReleaseProbability.facilitatorMagnitude,
      facilitationTimeConstant = signalReleaseProbability.facilitationTimeConstant,
      depletionBase = signalReleaseProbability.depletionBase,
      depletionMagnitude = signalReleaseProbability.depletionMagnitude,
      depletionTimeConstant = signalReleaseProbability.depletionTimeConstant
    )(facilitationSum, depletionSum)
}