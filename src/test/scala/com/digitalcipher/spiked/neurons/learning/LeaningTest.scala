package com.digitalcipher.spiked.neurons.learning

import com.digitalcipher.spiked.BaseSpec
import com.digitalcipher.spiked.neurons.learning.stdp.{NoLearning, StdpHardLimit}
import squants.Time
import squants.time.Milliseconds

class LeaningTest extends BaseSpec {

  behavior of "learning depends on the relative spike timing between pre and post-synaptic neurons"

  val inhibitionAmplitude = 1
  val inhibitionPeriod: Time = Milliseconds(30)
  val excitationAmplitude = 2
  val excitationPeriod: Time = Milliseconds(20)

  val stdpLearning = StdpHardLimit(inhibitionAmplitude, inhibitionPeriod, excitationAmplitude, excitationPeriod)

  def heaviside: Double => Double = value => if (value > 0) 1 else 0
  def excitation(previousSpikeTime: Time, signalTime: Time, spikeTime: Time, weight: Double, maxWeight: Double): Double =
    heaviside(maxWeight - weight) * excitationAmplitude * math.exp(-(spikeTime - signalTime) / excitationPeriod)
  def inhibition(signalTime: Time, spikeTime: Time, weight: Double, minWeight: Double): Double =
    -heaviside(weight - minWeight) * inhibitionAmplitude * math.exp(-(signalTime - spikeTime) / inhibitionPeriod)

  it should "increase the weights when the pre-synaptic neuron fires before the post-synaptic neuron" in {
    val previousSpike = Milliseconds(10)
    val spike = Milliseconds(120)
    (BigDecimal(previousSpike.toMilliseconds) to spike.toMilliseconds by 2)
      .map(time => Milliseconds(time))
      .foreach(signal => assert(
        stdpLearning.preSpikeStdpFunction.apply(signal - previousSpike, spike - previousSpike, 0.5, 1) == excitation(previousSpike, signal, spike, 0.5, 1)
      ))
  }

  it should "decrease the weights when the pre-synaptic neuron fires after the post-synaptic neuron" in {
    val spike = Milliseconds(20)
    (BigDecimal(spike.toMilliseconds) to 120 by 2)
      .map(time => Milliseconds(time))
      .foreach(signal => {
        val actual = stdpLearning.postSpikeStdpFunction.apply(spike - signal, 0.5, 0)
        val expected = inhibition(signal, spike, 0.5, 0)
        println(s"delta: ${signal - spike}; actual: $actual; expected: $expected")
        assert(actual == expected)
      })
  }

  behavior of "flat learning function"

  val noLearning = NoLearning()

  it should "not change the weight under any condition" in {
    val previousSpike = Milliseconds(10)
    val spike = Milliseconds(120)
    (BigDecimal(previousSpike.toMilliseconds) to spike.toMilliseconds by 2)
      .map(time => Milliseconds(time))
      .foreach(signal => {
        assert(noLearning.preSpikeStdpFunction.apply(signal - previousSpike, spike - previousSpike, 0.5, 1) == 0)
        assert(noLearning.postSpikeStdpFunction.apply(signal - spike, 0.5, 0.0) == 0)
      })
  }
}
