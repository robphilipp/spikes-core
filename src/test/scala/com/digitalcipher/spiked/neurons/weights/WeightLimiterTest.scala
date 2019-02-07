package com.digitalcipher.spiked.neurons.weights

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.neurons.weights.limit.{Bounded, Unbounded}

/**
  * Created by rob on 5/13/17.
  */
class WeightLimiterTest extends BaseSpec {

  "Bounded limiter function for [0, 5]" should "limit weights to that interval" in {
    val limiter = Bounded(lowerBound = 0, upperBound = 5).limiterFunction
    assert(limiter(11) == 5)
    assert(limiter(5) == 5)
    assert(limiter(2) == 2)
    assert(limiter(-1) == 0)
    assert(limiter(-100) == 0)
  }

  "Unbounded limiter function" should "not limit weights" in {
    val limiter = Unbounded().limiterFunction
    assert(limiter(100) == 100)
    assert(limiter(0) == 0)
    assert(limiter(-100) == -100)
  }
}

object WeightLimiterTest {
  def approxEqual(a: Double, b: Double)(implicit tolerance: Double = 1e-9): Boolean = math.abs(a - b) <= tolerance
}
