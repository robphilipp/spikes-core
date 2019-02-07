package com.digitalcipher.spiked.neurons

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.neurons.learning.stdp.{NoLearning, StdpHardLimit}
import com.digitalcipher.spiked.construction.description.{LearningFunctionDescription, NoLearningParams, StdpHardLimitLearningParams}
import squants.Time
import squants.electro.{ElectricPotential, Millivolts}
import squants.time.Milliseconds

/**
  * Tests for the basic calculations in the [[Neuron]] object. For tests for the [[Neuron]] class
  * see [[MonostableIntegratorTest]] and [[BistableIntegratorTest]] classes.
  */
class NeuronTest extends BaseSpec {

  behavior of "signal added to exponentially decaying membrane potential"

  def membranePotential(currentTime: Int, prevTime: Int, currPotential: Int, halfLife: Int): ElectricPotential =
    Neuron.calculateMembranePotential(
      signal = Signal(Milliseconds(currentTime), null, Millivolts(1)),
      lastEvent = Milliseconds(prevTime),
      membranePotential = Millivolts(currPotential),
      minMembranePotential = Millivolts(0),
      decayHalfLife = Milliseconds(halfLife)
    )

  it should "have the signal value when the membrane potential is 0 and the signal time is anything" in {
    assert(membranePotential(0, 0, 0, 10).toMillivolts == 1)
    assert(membranePotential(1, 0, 0, 10).toMillivolts == 1)
    assert(membranePotential(10, 0, 0, 10).toMillivolts == 1)
    assert(membranePotential(100, 0, 0, 10).toMillivolts == 1)
    assert(membranePotential(1000, 0, 0, 10).toMillivolts == 1)
  }

  it should "have the signal value plus the decayed membrane potential when the membrane potential is not 0" in {
    assert(membranePotential(0, 0, 1, 10).toMillivolts == 1 + 1)
    assert(membranePotential(10, 0, 1, 10).toMillivolts == 1 + Math.exp(-1))
    assert(membranePotential(100, 0, 1, 10).toMillivolts == 1 + Math.exp(-100/10))
  }

  behavior of "the membrane potential rise-time in response to an incoming signal"

  def riseTime(initialPotential: Int, currentPotential: Int, threshold: Int, halfLife: Int): Time =
    Neuron.calculateRiseTimeDelay(
      initialMembranePotential = Millivolts(initialPotential),
      membranePotential= Millivolts(currentPotential),
      threshold = Millivolts(threshold),
      riseHalfLife = Milliseconds(halfLife)
    )

  it should "have a zero response time if the signal is 0 or inhibiting" in {
    assert(riseTime(initialPotential = 3, currentPotential = 3, threshold = 5, halfLife = 10).toMilliseconds == 0)
  }

  // recall that the membrane potential handed to the rise-time function is the membrane potential
  // AFTER the signal has already been added (and the membrane potential decayed). so in order for
  // the membrane potential to cross the threshold (and thus have a rise time) the membrane potential
  // minus the signal (1 mV) must be < to the threshold
  it should "have a zero response time if the membrane potential did not cross the threshold" in {
    assert(riseTime(initialPotential = 5, currentPotential = 6, threshold = 5, halfLife = 10).toMilliseconds == 0)
  }

  // recall the that the response kernel is an α-function for which the rise-time is approximately linear
  // for small times. we use a linear approximation
  it should "have a linear rise-time when the half-life is 10 ms" in {
    assert(riseTime(initialPotential = 4, currentPotential = 5, threshold = 5, halfLife = 10).toMilliseconds == 2)
    assert(riseTime(initialPotential = 3, currentPotential = 5, threshold = 5, halfLife = 10).toMilliseconds == 4)
    assert(riseTime(initialPotential = 0, currentPotential = 5, threshold = 5, halfLife = 10).toMilliseconds == 10)
  }

  behavior of "the conversion of the learning description into a function"

  it should "convert an stdp description into an stdp function" in {
    val params = StdpHardLimitLearningParams(
      inhibitionAmplitude = 1,
      inhibitionPeriod = Milliseconds(10),
      excitationAmplitude = 2,
      excitationPeriod = Milliseconds(20)
    )
    val description = LearningFunctionDescription(params)
    val learningFunction = Neuron.convert(description)
    assert(learningFunction.isInstanceOf[StdpHardLimit])
    assert(learningFunction.equals(StdpHardLimit(1, Milliseconds(10), 2, Milliseconds(20))))
  }

  it should "convert an stdp description into an flat learning function" in {
    val description = LearningFunctionDescription(NoLearningParams())
    val learningFunction = Neuron.convert(description)
    assert(learningFunction.isInstanceOf[NoLearning])
  }
}
