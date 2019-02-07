package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.construction.description.LearningFunctionDescription.STDP_HARD
import squants.Time

/**
  * STDP learning function that has hard weight limits that allow the weights to be adjusted linearly
  *
  * @param inhibitionAmplitude The amplitude of the inhibition (the negative of value of the STDP function for t,,pre,, - T,,f,n,, = 0,,+,,)
  * @param inhibitionPeriod    The decay half-life of the inhibition adjustment
  * @param excitationAmplitude The amplitude of the excitation (the value of the STDP function for t,,pre,, - T,,f,n,, = 0,,-,,)
  * @param excitationPeriod    The decay half-life of the excitation adjustment
  */
case class StdpHardLimitLearningFunction(inhibitionAmplitude: Double,
                                         inhibitionPeriod: Time,
                                         excitationAmplitude: Double,
                                         excitationPeriod: Time) {
  private val partition = "learning"
  override def toString: String = s"$partition; learning_type: ${STDP_HARD.name}; " +
    s"inhibitory_amplitude: $inhibitionAmplitude; inhibitory_period: $inhibitionPeriod; " +
    s"excitation_amplitude: $excitationAmplitude; excitation_period: $excitationPeriod"
}
