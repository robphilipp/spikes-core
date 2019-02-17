package com.digitalcipher.spiked.logging.json

import com.digitalcipher.spiked.logging.messages.{ConnectedPostSynaptic, IntrinsicPlasticityUpdated}
import org.scalatest._
import squants.electro.Millivolts
import squants.time.{Milliseconds, Seconds}

class SpikeEventsJsonProtocolTest extends WordSpecLike with Matchers with OptionValues with Inside with Inspectors {

  import com.digitalcipher.spiked.logging.json.SpikeEventsJsonProtocol._
  import spray.json._

  "connection to post-synaptic neuron" must {
    val connection = ConnectedPostSynaptic("pre", "post", Milliseconds(314))
    val json = "{\"pre_synaptic\":\"pre\",\"post_synaptic\":\"post\",\"signal_delay\":314}"

    "convert to json" in {
      connection.toJson.compactPrint == json
    }

    "convert from json" in {
      json.parseJson.convertTo[ConnectedPostSynaptic] == connection
    }
  }

  "intrinsic plasticity updates" must {
    val update = IntrinsicPlasticityUpdated("id", Seconds(31415.926), Millivolts(3))
    val json = "{\"neuron_id\":\"id\",\"timestamp\":314159,\"plasticity\":3}"

    "convert to json" in {
      update.toJson.compactPrint == json
    }

    "convert from json" in {
      json.parseJson.convertTo[IntrinsicPlasticityUpdated] == update
    }
  }
}
