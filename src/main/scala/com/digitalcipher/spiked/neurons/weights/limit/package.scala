package com.digitalcipher.spiked.neurons.weights

/**
  * Created by rob on 1/15/17.
  */
package object limit {

  /**
    * Abstract class for weight-limiter functions used to bound the values the connection-weights can have.
    */
  abstract class WeightLimiterFunction {
    /**
      * Returns a function that bounds a weight. For example, in it's simplest form, a weight-limiter function could
      * look like<p>
      *  f(w) = min( W,,max,,, max( W,,min,,, w ) )
      * <p>where W,,max,, is the weight's upper bound, and W,,min,, is the weight's lower bound
      * @return A function that takes a weight and returns a bounded weight
      */
    def limiterFunction: Double => Double
  }

  /**
    * Default connection-weight-limiter-function generator limits the connection weight such that B,,lower,, ≤ w ≤ B,,upper,,<p>
    * f(w) = max( B,,lower,,, min( B,,upper,,, w )
    * <p>where
    * <ul>
    * <li>w is the synapse weight (multiples the incoming signal to represent the membrane response)</li>
    * <li>B,,lower,, The lower bound of the connect weight</li>
    * <li>B,,upper,, The upper bound of the connect weight</li>
    * </ul>
    * @constructor
    * @param lowerBound The smallest value allowed for the connection weight
    * @param upperBound The largest value allowed for the connection weight
    */
  case class Bounded(lowerBound: Double, upperBound: Double) extends WeightLimiterFunction {

    /**
      * Default connection-weight-limiter-function generator limits the connection weight such that B,,lower,, ≤ w ≤ B,,upper,,<p>
      * f(w) = max( B,,lower,,, min( B,,upper,,, w )
      * <p>where
      * <ul>
      * <li>w is the synapse weight (multiples the incoming signal to represent the membrane response)</li>
      * <li>B,,lower,, The lower bound of the connect weight</li>
      * <li>B,,upper,, The upper bound of the connect weight</li>
      * </ul>
      * @return A function that returns a connection weight constrained by the upper and lower bounds
      */
    override def limiterFunction: Double => Double =
      (weight: Double) => math.max(lowerBound, math.min(upperBound, weight))

    override def toString: String = s"type=bounded, bounds=($lowerBound, $upperBound)"
  }

  /**
    * @return An identity function so that the connection weight can be unbounded
    */
  case class Unbounded() extends WeightLimiterFunction {
    override def limiterFunction: Double => Double = (weight: Double) => weight

    override def toString: String = "type=unbounded"
  }
}
