package com.digitalcipher.spiked.logging.messages

import com.digitalcipher.spiked.logging.MessageNames
import squants.Time

/**
  * Created by rob on 3/19/17.
  */
case class StdpWeightUpdated(neuronId: String,
                             sourceId: String,
                             previousWeight: Double,
                             newWeight: Double,
                             adjustment: Double,
                             timeWindow: Time,
                             signalTime: Time,
                             stdpTime: Time) {
  import MessageNames.WEIGHT_UPDATE
  override def toString: String = s"${WEIGHT_UPDATE.name}; " +
    s"id: $neuronId; " +
    s"source: $sourceId; " +
    s"previous_weight: $previousWeight; " +
    s"new_weight: $newWeight; " +
    s"adjustment: $adjustment; " +
    s"time_window: $timeWindow; " +
    s"stdp_time: $stdpTime; " +
    s"signal_time: $signalTime"
}
