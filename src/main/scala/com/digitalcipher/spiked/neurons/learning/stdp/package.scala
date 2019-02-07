package com.digitalcipher.spiked.neurons.learning

import squants.Time
import squants.time._

/**
  * Learning functions
  * Created by rob on 1/15/17.
  */
package object stdp {

  /**
    * Base class for the STDP learning functions
    */
  abstract class SpikeTimingDependentPlasticity {
    /**
      * Calculates the weight change for pre-synaptic spikes that arrive before the post-synaptic neuron fires.
      * Given the following definitions,
      * <ul>
      *   <li>T,,f,n,, as the post-synaptic neuron spike time</li>
      *   <li>T,,f,n-1,, as the previous post-synaptic spike time</li>
      *   <li>t,,pre,, as the pre-synaptic spike time</li>
      *   <li>ω is the synaptic weight (efficacy)</li>
      *   <li>ω,,max,, is the upper bound on the connection weights</li>
      * </ul>
      * the 4 function arguments are
      * <ol>
      *   <li>∆t,,b,, = t,,pre,, - T,,f,n-1,,, where ∆t,,b,, ≥ 0</li>
      *   <li>∆T = T,,f,n,, - T,,f,n-1,,, where ∆ > 0</li>
      *   <li>ω</li>
      *   <li>ω,,max,,</li>
      * </ol>
      * and returns the weight adjustment, ∆ω.
      * @return a function f(∆t,,b,,, ∆T, ω, ω,,max,,) -> ∆ω.
      */
    def preSpikeStdpFunction: (Time, Time, Double, Double) => Double

    /**
      * Calculates the weight change for pre-synaptic spikes that arrive after the post-synaptic neuron has fired.
      * Given the following definitions,
      * <ul>
      *   <li>T,,f,n,, as the post-synaptic neuron spike time</li>
      *   <li>t,,pre,, as the pre-synaptic spike time</li>
      *   <li>ω is the synaptic weight (efficacy)</li>
      *   <li>ω,,min,, is the lower bound on the connection weights</li>
      * </ul>
      * the 4 function arguments are
      * <ol>
      *   <li>∆t,,a,, = T,,f,n,, - t,,pre,,, where ∆t,,a,, ≤ 0</li>
      *   <li>ω</li>
      *   <li>ω,,min,,</li>
      * </ol>
      * and returns the weight adjustment, ∆ω.
      * @return a function f(∆t,,a,,, ω, ω,,min,,) -> ∆ω.
      */
    def postSpikeStdpFunction: (Time, Double, Double) => Double

    /**
      * @return The Heaviside function Θ(x) = 1 for x > 0; Θ(x) = 0 for x <= 0
      */
    protected def heaviside: Double => Double = value => if (value > 0) 1 else 0
  }

  /**
    * Generator of the spike-timing-dependent plasticity function that has a hard weight limit. The function is a
    * function of the post-synaptic spike time, the pre-synaptic spike time, and the connection weight. The function
    * has the form
    * <p>
    * f(∆t,,b,,, ∆t,,a,,, ∆T, ω, w,,min,,, ω,,max,,) = -f,,a,,(∆t,,a,,, ω, ω,,max,,) + f,,b,,(∆t,,b,,, ∆T, ω, ω,,max,,)
    * </p>
    * <p>
    * where the inhibition for pre-synaptic spikes arriving after the post-synaptic neuron has fired is given by
    * </p>
    *   f,,a,,(∆t,,a,,, ω, ω,,max,,) = Θ(ω - ω,,min,,) A,,inhib,, exp(-(t,,pre,, - T,,f,n,,) / τ,,inhib,,)
    * <p>
    * and where the excitation for pre-synaptic spikes arriving before the post-synaptic neuron has fired is given by
    * </p>
    *   f,,b,,(∆t,,b,,, ∆T, ω, ω,,max,,) = Θ(ω,,max,, - ω) A,,exc,, exp((∆t,,b,, - ∆) / τ,,exc,,)
    * </p>
    * <p>and
    * <ul>
    *   <li>Θ(x) is the Heaviside function</li>
    *   <li>ω is the connection weight between the pre- and post-synaptic neurons</li>
    *   <li>ω,,min,, is the lower bound on connection weights</li>
    *   <li>ω,,max,, is the upper bound on the connection weights</li>
    *   <li>T,,f,n,, as the post-synaptic neuron spike time</li>
    *   <li>T,,f,n-1,, as the previous post-synaptic spike time</li>
    *   <li>t,,pre,, as the pre-synaptic spike time</li>
    *   <li>ω is the synaptic weight (efficacy)</li>
    *   <li>∆t,,a,, = T,,f,n,, - t,,pre,,, where ∆t,,a,, ≤ 0</li>
    *   <li>∆t,,b,, = t,,pre,, - T,,f,n-1,,, where ∆t,,b,, ≥ 0</li>
    *   <li>∆T = T,,f,n,, - T,,f,n-1,,, where ∆ > 0</li>
    *   <li>A,,inhib,, is the amplitude of the inhibition</li>
    *   <li>τ,,inhib,, is the decay half-life of the inhibition signal</li>
    *   <li>A,,exc,, is the amplitude of the excitation</li>
    *   <li>τ,,exc,, is the decay half-life of the excitation</li>
    * </ul>
    * </p>
    *
    * @param inhibitionAmplitude The amplitude of the inhibition (the negative of value of the STDP function for t,,pre,, - T,,f,n,, = 0,,+,,)
    * @param inhibitionPeriod    The decay half-life of the inhibition adjustment
    * @param excitationAmplitude The amplitude of the excitation (the value of the STDP function for t,,pre,, - T,,f,n,, = 0,,-,,)
    * @param excitationPeriod    The decay half-life of the excitation adjustment
    * @return The adjustment to the pre-synaptic weight (-A,,inhib,, ≤ ∆ω ≤ A,,exc,,)
    */
  case class StdpHardLimit(inhibitionAmplitude: Double,
                           inhibitionPeriod: Time,
                           excitationAmplitude: Double,
                           excitationPeriod: Time) extends SpikeTimingDependentPlasticity {

    /**
      * Calculates the weight increase due to learning. This method should be called when the pre-synaptic
      * neuron fires just before the post-synaptic neuron (causal). The function accepts the signal time
      * relative to the previous fire time, and the time-window between firing times. Doing the math
      * gives the expected value. The specified
      * <p>
      *   ∆t,,b,, = t,,pre,, - T,,f,n-1,,
      * <p>
      * where t,,pre,, is the signal time (i.e. when the spike from the pre-synaptic neuron was received) and T,,f,n-1,,
      * is the previous post-synaptic spike time. And
      * <p>
      *   ∆T = T,,f,n,, - T,,f,n-1,,
      * <p>
      * where T,,f,n,, is the current post-synaptic spike time. Then the weight adjustment
      * <p>
      *   ∆ω = Θ(ω,,max,, - ω) A,,exc,, exp((∆t,,b,, - ∆) / τ,,exc,,)
      * <p>
      *   ∆ω = Θ(ω,,max,, - ω) A,,exc,, exp(((t,,pre,, - T,,f,n-1,,) - (T,,f,n,, - T,,f,n-1,,)) / τ,,exc,,)
      * <p>
      *   ∆ω = Θ(ω,,max,, - ω) A,,exc,, exp((t,,pre,, - T,,f,n,,) / τ,,exc,,)
      * <p>
      *   ∆ω = Θ(ω,,max,, - ω) A,,exc,, exp(-(T,,f,n,, - t,,pre,,) / τ,,exc,,)
      * <p>
      * which is the expected functional form, with T,,f,n,, - t,,pre,, ≥ 0, and where Θ(x) is the Heaviside function.
      * @return a function f(∆t,,b,,, ∆T, ω, ω,,max,,) -> ∆ω.
      */
    override def preSpikeStdpFunction: (Time, Time, Double, Double) => Double = (time: Time, timeWindow: Time, weight: Double, weightMax: Double) =>
      heaviside(weightMax - weight) * excitationAmplitude * math.exp((time - timeWindow) / excitationPeriod)

    /**
      * Calculates the weight decrease (inhibition) for pre-synaptic spikes arriving at the post-synaptic
      * neuron after the post-synaptic neuron has fired. The parameter ∆t is the time between the signal
      * and the last spike of the post-synaptic neuron.
      * <p>
      *   ∆t,,a,, = T,,f,n,, - t,,pre,,
      * <p>
      * where t,,pre,, ≥ T,,f,n,,, and t,,pre,, is the time at which the pre-synpatic spike arrived at the post-synaptic
      * neuron, and T,,f,n,, is the time of the post-synaptic neuron's most recent spike. Then the weight adjustment
      * <p>
      *   ∆ω = Θ(ω - ω,,min,,) A,,inhib,, exp(∆t / τ,,inhib,,) = Θ(ω - ω,,min,,) A,,inhib,, exp(-(t,,pre,, - T,,f,n,,) / τ,,inhib,,)
      * <p>
      * where Θ(x) is the Heaviside function.
      * @return a function f(∆t,,a,,, ω, ω,,max,,) -> ∆ω.
      */
    override def postSpikeStdpFunction: (Time, Double, Double) => Double = (delta: Time, weight: Double, minWeight: Double) =>
      -heaviside(weight - minWeight) * inhibitionAmplitude * math.exp(delta / inhibitionPeriod)

    /**
      * @return A string representing the STDP function
      */
    override def toString: String = s"type=stdp-hard-limit; " +
      s"inhibition amplitude=$inhibitionAmplitude; inhibition period=$inhibitionPeriod; " +
      s"excitation amplitude=$excitationAmplitude; excitation period=$excitationPeriod"
  }

  /**
    * Generator of the spike-timing-dependent plasticity function that has a soft (exponential) weight limit. The function is a
    * function of the post-synaptic spike time, the pre-synaptic spike time, and the connection weight. The function
    * has the form
    * <p>
    * f(∆t,,b,,, ∆t,,a,,, ∆T, ω, w,,min,,, ω,,max,,) = -f,,a,,(∆t,,a,,, ω, ω,,max,,) + f,,b,,(∆t,,b,,, ∆T, ω, ω,,max,,)
    * </p>
    * <p>
    * where the inhibition for pre-synaptic spikes arriving after the post-synaptic neuron has fired is given by
    * </p>
    *   f,,a,,(∆t,,a,,, ω, ω,,max,,) = min(0, ω - ω,,min,,) A,,inhib,, exp(-(t,,pre,, - T,,f,n,,) / τ,,inhib,,)
    * <p>
    * and where the excitation for pre-synaptic spikes arriving before the post-synaptic neuron has fired is given by
    * </p>
    *   f,,b,,(∆t,,b,,, ∆T, ω, ω,,max,,) = max(0, ω,,max,, - ω) A,,exc,, exp((∆t,,b,, - ∆) / τ,,exc,,)
    * </p>
    * <p>and
    * <ul>
    *   <li>ω is the connection weight between the pre- and post-synaptic neurons</li>
    *   <li>ω,,min,, is the lower bound on connection weights</li>
    *   <li>ω,,max,, is the upper bound on the connection weights</li>
    *   <li>T,,f,n,, as the post-synaptic neuron spike time</li>
    *   <li>T,,f,n-1,, as the previous post-synaptic spike time</li>
    *   <li>t,,pre,, as the pre-synaptic spike time</li>
    *   <li>ω is the synaptic weight (efficacy)</li>
    *   <li>∆t,,a,, = T,,f,n,, - t,,pre,,, where ∆t,,a,, ≤ 0</li>
    *   <li>∆t,,b,, = t,,pre,, - T,,f,n-1,,, where ∆t,,b,, ≥ 0</li>
    *   <li>∆T = T,,f,n,, - T,,f,n-1,,, where ∆ > 0</li>
    *   <li>A,,inhib,, is the amplitude of the inhibition</li>
    *   <li>τ,,inhib,, is the decay half-life of the inhibition signal</li>
    *   <li>A,,exc,, is the amplitude of the excitation</li>
    *   <li>τ,,exc,, is the decay half-life of the excitation</li>
    * </ul>
    * </p>
    *
    * @param inhibitionAmplitude The amplitude of the inhibition (the negative of value of the STDP function for t,,pre,, - T,,f,n,, = 0,,+,,)
    * @param inhibitionPeriod    The decay half-life of the inhibition adjustment
    * @param excitationAmplitude The amplitude of the excitation (the value of the STDP function for t,,pre,, - T,,f,n,, = 0,,-,,)
    * @param excitationPeriod    The decay half-life of the excitation adjustment
    * @return The adjustment to the pre-synaptic weight (-A,,inhib,, ≤ ∆ω ≤ A,,exc,,)
    */
  case class StdpSoftLimit(inhibitionAmplitude: Double,
                           inhibitionPeriod: Time,
                           excitationAmplitude: Double,
                           excitationPeriod: Time) extends SpikeTimingDependentPlasticity {

    /**
      * Calculates the weight increase due to learning. This method should be called when the pre-synaptic
      * neuron fires just before the post-synaptic neuron (causal). The function accepts the signal time
      * relative to the previous fire time, and the time-window between firing times. Doing the math
      * gives the expected value. The specified
      * <p>
      *   ∆t,,b,, = t,,pre,, - T,,f,n-1,,
      * <p>
      * where t,,pre,, is the signal time (i.e. when the spike from the pre-synaptic neuron was received) and T,,f,n-1,,
      * is the previous post-synaptic spike time. And
      * <p>
      *   ∆T = T,,f,n,, - T,,f,n-1,,
      * <p>
      * where T,,f,n,, is the current post-synaptic spike time. Then the weight adjustment
      * <p>
      *   ∆ω = max(0, ω,,max,, - ω) A,,exc,, exp((∆t,,b,, - ∆T) / τ,,exc,,)
      * <p>
      *   ∆ω = max(0, ω,,max,, - ω) A,,exc,, exp(((t,,pre,, - T,,f,n-1,,) - (T,,f,n,, - T,,f,n-1,,)) / τ,,exc,,)
      * <p>
      *   ∆ω = max(0, ω,,max,, - ω) A,,exc,, exp((t,,pre,, - T,,f,n,,) / τ,,exc,,)
      * <p>
      *   ∆ω = max(0, ω,,max,, - ω) A,,exc,, exp(-(T,,f,n,, - t,,pre,,) / τ,,exc,,)
      * <p>
      * which is the expected functional form, with T,,f,n,, - t,,pre,, ≥ 0.
      * @return a function f(∆t,,b,,, ∆T, ω, ω,,max,,) -> ∆ω.
      */
    override def preSpikeStdpFunction: (Time, Time, Double, Double) => Double = (time: Time, timeWindow: Time, weight: Double, weightMax: Double) =>
      math.max(0, weightMax - weight) * excitationAmplitude * math.exp((time - timeWindow) / excitationPeriod)

    /**
      * Calculates the weight decrease (inhibition) for pre-synaptic spikes arriving at the post-synaptic
      * neuron after the post-synaptic neuron has fired. The parameter ∆t is the time between the signal
      * and the last spike of the post-synaptic neuron.
      * <p>
      *   ∆t,,a,, = T,,f,n,, - t,,pre,,
      * <p>
      * where t,,pre,, ≥ T,,f,n,,, and t,,pre,, is the time at which the pre-synpatic spike arrived at the post-synaptic
      * neuron, and T,,f,n,, is the time of the post-synaptic neuron's most recent spike. Then the weight adjustment
      * <p>
      *   ∆ω = min(0, ω - ω,,min,,) A,,inhib,, exp(∆t / τ,,inhib,,)
      * <p>
      *   ∆ω = min(0, ω - ω,,min,,) A,,inhib,, exp(-(t,,pre,, - T,,f,n,,) / τ,,inhib,,)
      * <p>
      * Note that t,,pre,, - T,,f,n,, ≥ 0
      * @return a function f(∆t,,a,,, ω, ω,,max,,) -> ∆ω.
      */
    override def postSpikeStdpFunction: (Time, Double, Double) => Double = (delta: Time, weight: Double, minWeight: Double) =>
      math.min(0, minWeight - weight) * inhibitionAmplitude * math.exp(delta / inhibitionPeriod)

    /**
      * @return A string representing the STDP function
      */
    override def toString: String = s"type=stdp-soft-limit; " +
      s"inhibition amplitude=$inhibitionAmplitude; inhibition period=$inhibitionPeriod; " +
      s"excitation amplitude=$excitationAmplitude; excitation period=$excitationPeriod"
  }

  /**
    * STDP learning function that is shaped like an α-function (f(t) = (-t/τ) exp(t/τ)) on a baseline b,
    * f(t) = max(b, (-t/τ) exp(t/τ) + b)). When the pre-synaptic spike arrives at the post-synaptic neuron before the
    * post-synaptic neuron fires, then the weight change is greater than or equal to the baseline times the learning rate
    * (depending on the timing). And when the pre-synaptic spike arrives at the post-synaptic neuron after the
    * post-synaptic neuron has fired, then the weight change is the baseline times the learning rate value.
    * <p>
    *   ω,,t+1,, = ω,,t,, + ∆ω
    * <p>
    * where
    * <p>
    *   ω,,t,, is the current connection weight
    * <p>
    *   ω,,t+1,, is the updated connection weight
    * <p>
    *   ∆ω is the calculated weight change
    * <p>
    *   η is the learning rate that is included in the weight change calculation
    * @param baseline The baseline value that satisfies the condition b ≤ 0
    * @param timeConstant The time-constant τ
    * @param learningRate The learning rate η
    */
  case class StdpAlphaLearning(baseline: Double, timeConstant: Time, learningRate: Double) extends SpikeTimingDependentPlasticity {

    // calculates the approximate zero of α(t) to within the specified tolerance
    val zeroOffset: Time = -StdpAlphaLearning.zeroTimeOffset(
      time = -StdpAlphaLearning.zeroTimeOffsetGuess(timeConstant, baseline),
      previousTime = null,
      tolerance = Nanoseconds(1),
      alpha = StdpAlphaLearning.alphaFunction(timeConstant, Milliseconds(0), baseline),
      alphaDot = StdpAlphaLearning.alphaFunctionDot(timeConstant, Milliseconds(0), baseline)
    )

    // α(t) -- the alpha learning function, offset so that α(t=0) is approximately 0
    val alphaFunction: Time => Double = StdpAlphaLearning.alphaFunction(timeConstant, zeroOffset, baseline)

    /**
      * Calculates the weight increase due to learning. This method should be called when the pre-synaptic
      * neuron fires just before the post-synaptic neuron (causal). The function accepts the signal time
      * relative to the previous fire time, and the time-window between firing times. Doing the math
      * gives the expected value. The specified `time` ∆t,,b,, is given by
      * <p>
      *   ∆t,,b,, = t,,pre,, - T,,f,n-1,,
      * <p>
      * where t,,pre,, is the signal time (i.e. when the spike from the pre-synaptic neuron was received) and T,,f,n-1,,
      * is the previous post-synaptic spike time. And the specified `timeWindow` ∆T is given by
      * <p>
      *   ∆T = T,,f,n,, - T,,f,n-1,,
      * <p>
      *   ∆ω = η Θ(ω,,max,, - ω) max(b, -exp(1) (1-b) ((∆t,,b,, - ∆T)/τ) exp((∆t,,b,, - ∆T)/τ) + b)
      * <p>
      *   ∆ω = η Θ(ω,,max,, - ω) max(b, exp(1) (1-b) (-(T,,f,n,, - t,,pre,,)/τ) exp(-(T,,f,n,, - t,,pre,,)/τ) + b)
      * where T,,f,n,, is the current post-synaptic spike time. Then the weight adjustment
      * which is the expected functional form, with T,,f,n,, - t,,pre,, ≥ 0, and where Θ(x) is the Heaviside function.
      * The baseline, b, times the learning rate, η, is the minimum weight change returned. The time-constant, τ,
      * determines the rise and decay of the learning rate multiplier, based on the timing of the signals. Both
      * the baseline and the time-constant define the shape of the α-function that multiplies the learning rate.
      * @return a function f(∆t,,b,,, ∆T, ω, ω,,max,,) -> ∆ω.
      */
    override def preSpikeStdpFunction: (Time, Time, Double, Double) => Double = (time: Time, timeWindow: Time, weight: Double, weightMax: Double) =>
      math.max(0, learningRate * heaviside(weightMax - weight) * alphaFunction(time - timeWindow))

    /**
      * When the pre-synaptic spike arrives at the post-synaptic neuron after the post-synaptic neuron has already fired,
      * then the weight changes by the baseline amount, regardless of the timing.
      * @return a function f(∆t,,a,,, ω, ω,,min,,) -> ∆ω.
      */
    override def postSpikeStdpFunction: (Time, Double, Double) => Double = (time: Time, weight: Double, minWeight: Double) =>
      math.min(0, learningRate * heaviside(weight - minWeight) * alphaFunction(-time) * math.exp(time / timeConstant))

    /**
      * @return A string representation of the learning function
      */
    override def toString: String = s"type=alpha; time constant=$timeConstant; baseline=$baseline; zero-offset=$zeroOffset"
  }

  /**
    * Companion to the alpha learning class
    */
  case object StdpAlphaLearning {
    /**
      * Configures and returns the α-function α(t) with the specified parameters captured.
      * @param timeConstant The time-constant for the α-function (τ)
      * @param zeroOffset The amount (δ) by which to offset the time so that the α-function is 0 at t = 0
      * @param baseline The baseline value (b)
      * @return The function α(t) with the captured parameters for the baseline, the time constant, and the zero-offset.
      *         The time t is the time between the post-synaptic spike and the pre-synaptic spike (T,,f,n,, - t,,pre,,)
      */
    def alphaFunction(timeConstant: Time, zeroOffset: Time, baseline: Double): Time => Double = (time: Time) => {
      val timeFactor = (zeroOffset - time) / timeConstant
      math.max(baseline, math.exp(1) * (1 - baseline) * timeFactor * math.exp(-timeFactor) + baseline)
    }

    /**
      * Configures and returns the time-derivative of α(t), ∂α(t)/∂t, with the specified parameters captured.
      * @param timeConstant The time-constant for the α-function (τ)
      * @param zeroOffset The amount (δ) by which to offset the time so that the α-function is 0 at t = 0
      * @param baseline The baseline value (b)
      * @return The time-derivative of the function α(t), ∂α(t)/∂t, with captured parameters for the baseline, the
      *         time constant, and the zero-offset. The time t is the time between the post-synaptic spike and the
      *         pre-synaptic spike (T,,f,n,, - t,,pre,,)
      */
    def alphaFunctionDot(timeConstant: Time, zeroOffset: Time, baseline: Double): Time => Frequency = (time: Time) => {
      // note that the timeFactor is -timeFactor from the alphaFunction
      val timeFactor = (time - zeroOffset) / timeConstant
      - math.exp(1) * (1 - baseline) * math.exp(timeFactor) * (1 + timeFactor) / timeConstant
    }

    /**
      * Calculates the approximate offset in time (i.e. the amount the α-function must shift so that it has a value
      * of 0 when the pre-synaptic spike arrives at the post-synaptic neuron just as it fires. The calculation is
      * based on an approximation and then finding the root of the cubic equation.
      * <p>
      *   f(t) = -e (1-b) (t-δ)/τ exp((t-δ)/τ) + b
      * <p>
      * where
      * <p>
      *   t = -(T,,f,n,, - t,,pre,,)
      * <p>
      *   δ is the (approximate) offset amount returned by this function
      * <p>
      *   τ is the time-constant
      * <p>
      *   b is the baseline
      * @param timeConstant The time-constant of the α-function
      * @param baseline The baseline of the stdp function (i.e. the inhibition value)
      * @return The time offset that ensures that the α-function has a value of zero when t = 0
      */
    def zeroTimeOffsetGuess(timeConstant: Time, baseline: Double): Time = {
      val gamma: Double = -baseline * math.exp(-1) / (1 - baseline)
      val R = (27 * gamma - 7) / 54
      val Q = 2.0 / 9
      val lambda = math.sqrt(Q * Q * Q + R * R)
      val S = math.cbrt(R + lambda)
      val T = math.cbrt(R - lambda)
      timeConstant * (S + T + 1.0/3)
    }

    /**
      * Uses the Newton-Raphson method to find the time t,,z,, at which the α-function α(t,,z,,) = 0. This time can be used to offset
      * the α-function so that weight updates are positive when 0 < T,,f,n,, - t,,pre,, < τ, but 0 at T,,f,n,, - t,,pre,,.
      * <p>
      * Tail-recursive approach.
      * @param time The current guess at when the time t,,n,, for which the α(t,,n,,) is 0
      * @param previousTime The previous guess at when the time t,,n-1,, for which the α(t,,n-1,,) is 0 (used to check whether the
      *                     approximation is within tolerance)
      * @param tolerance The tolerance ε determining when we are close enough to the solution |α(t,,n,,) - α(t,,n-1,,)| ≤ ε
      * @param alpha The α-function α(t) used to evaluate the guesses
      * @param alphaDot The time-derivative of the α-function ∂α(t)/∂t used to evaluate the guesses
      * @return The time t,,z,, satisfying 0 ≤ α(t,,z,,) < δ where δ is some desired tolerance, and satisfying |t,,z,, - t,,0,,| ≤ ε
      *         where t,,0,, is the true zero of α(t), i.e. α(t,,0,,) = 0.
      */
    def zeroTimeOffset(time: Time, previousTime: Time, tolerance: Time, alpha: Time => Double, alphaDot: Time => Frequency): Time = {
      // if the time hasn't changed more than the tolerance amount then the approximation is complete, to within tolerance
      if (previousTime != null && (time - previousTime).abs <= tolerance && alpha(time) >= 0) return time

      // calculate Newton's approximation and make the next iteration
      val t = time - alpha(time) * Seconds(1 / alphaDot(time).toHertz)
      zeroTimeOffset(t, time, tolerance, alpha, alphaDot)
    }
  }

  /**
    * Flat learning function that leaves the weights unchanged
    */
  case class NoLearning() extends SpikeTimingDependentPlasticity {

    /**
      * Always returns 0 for the weight adjustment, ∆ω
      * @return a function f(∆t,,b,,, ∆T, ω, ω,,max,,) -> 0.
      */
    override def preSpikeStdpFunction: (Time, Time, Double, Double) => Double = (_: Time, _: Time, _: Double, _: Double) => 0.0

    /**
      * Always returns 0 for the weight adjustment, ∆ω
      * @return a function f(∆t,,a,,, ω, ω,,max,,) -> 0.
      */
    override def postSpikeStdpFunction: (Time, Double, Double) => Double = (_: Time, _: Double, _: Double) => 0.0

    /**
      * @return A string representing the no-learning function
      */
    override def toString: String = s"type=no learning"
  }
}
