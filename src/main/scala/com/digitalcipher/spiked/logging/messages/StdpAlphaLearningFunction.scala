package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.construction.description.LearningFunctionDescription.STDP_ALPHA
import squants.Time

/**
  * STDP alpha learning function
  *
  * @param baseline The baseline value that satisfies the condition b ≤ 0
  * @param timeConstant The time-constant τ
  * @param learningRate The learning rate η
  */
case class StdpAlphaLearningFunction(baseline: Double, timeConstant: Time, learningRate: Double) {
  private val partition = "learning"
  override def toString: String = s"$partition; learning_type: ${STDP_ALPHA.name}; " +
    s"baseline: $baseline; learning_rate: $learningRate; time_constant: $timeConstant"
}
