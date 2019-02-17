package com.digitalcipher.spiked.logging.json

import com.digitalcipher.spiked.logging.messages.IntrinsicPlasticityUpdated
import org.scalatest._
import squants.electro.Millivolts
import squants.time.Seconds

class IntrinsicPlasticityUpdatedJsonTest extends WordSpecLike with Matchers with OptionValues with Inside with Inspectors {

  val update = IntrinsicPlasticityUpdated("id", Seconds(31415.926), Millivolts(3))
  val json = "{\"neuron_id\":\"id\",\"timestamp\":314159,\"plasticity\":3}"
  val jsonI = "{\"intrinsic_plasticity\":{\"neuron_id\":\"id\",\"timestamp\":314159,\"plasticity\":3}}"

  import com.digitalcipher.spiked.logging.json.SpikeEventsJsonProtocol._
  import spray.json._

  "intrinsic plasticity updates" must {
    "convert to json" in {
      update.toJson.compactPrint == json
    }

    "convert from json" in {
      val jsonAst = json.parseJson
      jsonAst.convertTo[IntrinsicPlasticityUpdated] == update
    }
  }
}
