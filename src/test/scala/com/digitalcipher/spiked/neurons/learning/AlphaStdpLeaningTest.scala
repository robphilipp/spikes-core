package com.digitalcipher.spiked.neurons.learning

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.neurons.learning.stdp.StdpAlphaLearning
import squants.time.Milliseconds
import org.scalatest.prop.TableDrivenPropertyChecks._

class AlphaStdpLeaningTest extends BaseSpec {

  behavior of "learning depends on the relative spike timing between pre and post-synaptic neurons"

  def heaviside: Double => Double = value => if (value > 0) 1 else 0

  it should "with a 0 baseline delta should be zero" in {
    val tcs = Table("tc", 2, 3, 4, 5, 6, 7, 10, 20, 100)
    forAll (tcs) {tc => StdpAlphaLearning.zeroTimeOffsetGuess(Milliseconds(tc), 0) should equal (Milliseconds(0))}
  }

  it should "with a 0 baseline at t = 0 the learning function should have a value of 0" in {
    val stdpLearning = StdpAlphaLearning(baseline = 0, timeConstant = Milliseconds(5), learningRate = 1)

    val preSynapticSpike = Milliseconds(120)
    val currentPostSynapticSpike = Milliseconds(120)
    val previousPostSynapticSpike = Milliseconds(20)

    stdpLearning.preSpikeStdpFunction
      .apply(preSynapticSpike - previousPostSynapticSpike, currentPostSynapticSpike - previousPostSynapticSpike, 0.2, 100.0
    ) should equal (0.0)
  }

  it should "with a 0 baseline the time for max should be -timeConstant and the max value should be 1" in {
    val timeConstant = Milliseconds(5)
    val stdpLearning = StdpAlphaLearning(baseline = 0, timeConstant = timeConstant, learningRate = 1)

    val preSynapticSpike = Milliseconds(120) - timeConstant  // 115 ms
    val currentPostSynapticSpike = Milliseconds(120)
    val previousPostSynapticSpike = Milliseconds(20)

    stdpLearning.preSpikeStdpFunction
      .apply(preSynapticSpike - previousPostSynapticSpike, currentPostSynapticSpike - previousPostSynapticSpike, 0.2, 100.0
    ) should equal (1.0)
  }

  it should "with a non-zero baseline at t = 0 the learning function should be approximately 0 and positive" in {
    val preSynapticSpike = Milliseconds(120)
    val currentPostSynapticSpike = Milliseconds(120)
    val previousPostSynapticSpike = Milliseconds(20)

    val baselines = Table("baseline", -1.0, -0.9, -0.8, -0.5, -0.1, 0)

    forAll (baselines) {baseline =>
      StdpAlphaLearning(baseline = baseline, timeConstant = Milliseconds(5), learningRate = 1).preSpikeStdpFunction.apply(
        preSynapticSpike - previousPostSynapticSpike, currentPostSynapticSpike - previousPostSynapticSpike, 0.2, 100.0
    ) should equal (0.0 +- 0.001)}

    forAll (baselines) {baseline =>
      StdpAlphaLearning(baseline = baseline, timeConstant = Milliseconds(5), learningRate = 1).preSpikeStdpFunction.apply(
        preSynapticSpike - previousPostSynapticSpike, currentPostSynapticSpike - previousPostSynapticSpike, 0.2, 100.0
    ) should be >= 0.0}
  }

  it should "with a non-zero baseline at t = 0 the learning function should have a max value of 1 at delta - time-constant" in {
    val timeConstant = Milliseconds(5)
    val preSynapticSpike = Milliseconds(120) - timeConstant
    val currentPostSynapticSpike = Milliseconds(120)
    val previousPostSynapticSpike = Milliseconds(20)

    val baselines = Table("baseline", -1.0, -0.9, -0.8, -0.5, -0.1, 0)

    forAll (baselines) {baseline => {
      val learning = new StdpAlphaLearning(baseline = baseline, timeConstant = timeConstant, learningRate = 1)
      learning.preSpikeStdpFunction.apply(
        preSynapticSpike - previousPostSynapticSpike + learning.zeroOffset, currentPostSynapticSpike - previousPostSynapticSpike, 0.2, 100.0
      )
    } should equal (1.0 +- 0.0001)}
  }

  it should "with a non-zero baseline and 0 weight, weight adjustments must only be positive" in {
    val learning = StdpAlphaLearning(baseline = -1, timeConstant = Milliseconds(30), learningRate = 0.02)

    val preSynapticSpikes = Table("preSynapticSpikes", 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130)

    val postSynapticSpike = Milliseconds(120)
    val previousPostSynapticSpike = Milliseconds(20)
    forAll (preSynapticSpikes) {spike => {
      val spikeTime = Milliseconds(spike)
      learning.preSpikeStdpFunction.apply(
        spikeTime - previousPostSynapticSpike, postSynapticSpike - previousPostSynapticSpike, 0.0, 1.0
      )
    } should be >= 0.0}
  }

  it should "with a non-zero baseline and the weight equals the max weight, weight adjustments must only be negative" in {
    val learning = new StdpAlphaLearning(baseline = -1, timeConstant = Milliseconds(30), learningRate = 0.02)

    val preSynapticSpikes = Table("preSynapticSpikes", 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130)

    val postSynapticSpike = Milliseconds(120)
    val previousPostSynapticSpike = Milliseconds(20)
    forAll (preSynapticSpikes) {spike => {
      val spikeTime = Milliseconds(spike)
      learning.preSpikeStdpFunction.apply(
        spikeTime - previousPostSynapticSpike, postSynapticSpike - previousPostSynapticSpike, 1.0, 1.0
      )
    } should be <= 0.0}
  }
}
