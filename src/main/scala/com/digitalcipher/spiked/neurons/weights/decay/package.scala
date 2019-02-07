package com.digitalcipher.spiked.neurons.weights

import squants.Time
import squants.time.Milliseconds

/**
  * Created by rob on 1/15/17.
  */
package object decay {

  /**
    * base class for the connection-weight decay functions
    */
  abstract class WeightDecayFunction {
    def decayFunction: Time => Double
  }

  /**
    * Exponential connection-weight-decay-function<p>
    * d(t) = exp( -t / T,,half,, )
    * <p>where<p>
    * <ul>
    * <li>t is the time since the last incoming signal for the source of this connection</li>
    * <li>T,,half,, is the decay half-life</li>
    * </ul>
    * @param halfLife The exponential-decay half-life
    * @return A function that returns a decay factor that can be multiplied to the connection weight to decay it
    */
  case class Exponential(halfLife: Time) extends WeightDecayFunction {
    /**
      * Default connection-weight-decay-function generator<p>
      * d(t) = exp( -t / T,,half,, )
      * <p>where<p>
      * <ul>
      * <li>t is the time since the last incoming signal for the source of this connection</li>
      * <li>T,,half,, is the decay half-life</li>
      * </ul>
      *
      * @return A function that returns a decay factor that can be multiplied to the connection weight to decay it
      */
    override def decayFunction: Time => Double = (time: Time) => math.exp(-time / halfLife)

    override def toString: String = s"type=exponential, halfLife=$halfLife"
  }

  /**
    * A no-decay connection-weight decay function. In other words, the connection weights do not decay over time.
    */
  case class NoDecay() extends WeightDecayFunction {
    /**
      * @return A function that always returns 1 so that the connection weights do NOT decay
      */
    override def decayFunction: Time => Double = (_: Time) => 1.0

    override def toString: String = s"type=zero"
  }

  /**
    * Weight stickiness adjustment function
    */
  abstract class WeightStickiness {
    def adjustment: (Double, Time) => Double
  }

  /**
    * Meant to represent a longer-term structural change that can be reverted. The longer a weight is in the boundary,
    * the more likely it is to stay there.
    * <p>Stickiness slows down the weight decay when the weights are at the boundaries<p>
    *   (i.e. w(t) ≤ W,,min,, or w(t) ≥ W,,max,,).
    * <p>The amount by which the decay is slowed depends on the time the weight has spent at the boundaries, specified
    * by the boundary time. For example, suppose the weight has been at the boundary for 20 s and the weight decay
    * factor is 0.5. Then the adjust decay factor may be, for example, 0.73, giving a slower overall decay.
    * @param halfLife The stickiness half-life
    * @return An adjusted weight decay factor
    */
  case class SigmoidWeightStickiness(halfLife: Time) extends WeightStickiness {

    /**
      * Returns a function that accepts a decay-factor (i.e. the weight decay) and the amount of time that the weight
      * has spent in the boundary regions (i.e. w ≥ W,,max,, or w ≤ W,,min,,), and returns the adjusted weight decay
      * factor.
      * @return function that accepts the decay factor to adjust and the amount of time spent in the boundary
      */
    override def adjustment: (Double, Time) => Double = (decayFactor: Double, deltaTime: Time) => {
      val stickiness = 2.0 * ( 1.0 / ( 1.0 + math.exp( -deltaTime.max(Milliseconds(0)) / halfLife ) ) - 0.5 )
      decayFactor + (1.0 - decayFactor) * stickiness
    }

    override def toString: String = s"type=sigmoid, halfLife=$halfLife"
  }

  case class NoStickiness() extends WeightStickiness {
    override def adjustment: (Double, Time) => Double = (decayFactor: Double, _) => decayFactor

    override def toString: String = "type=none"
  }
}
