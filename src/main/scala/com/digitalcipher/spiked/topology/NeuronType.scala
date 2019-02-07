package com.digitalcipher.spiked.topology

import scala.util.matching.Regex

/**
  * The enumeration of neuron types and the regular expression used to the determine whether
  * a neuron is an input neuron, an output neuron, or a hidden neuron. Neuron IDs starting with
  * `input-N`, where `N` is a digit, and followed by digits, letters, dots, or dashes, are classified
  * as input neurons. Neuron IDs staring with `output-N` and followed by digits, letters, dots, or
  * dashes, are classified as output neurons. All other neuron IDs are classified as hidden neurons.
  */
object NeuronType extends Enumeration {
  type NeuronType = Value
  val Input, Hidden, Output = Value

  val inputRegex: Regex = """input\-[0-9]+(\-[0-9a-zA-Z\.]*)*""".r
  val outputRegex: Regex = """output\-[0-9]+(\-[0-9a-zA-Z\.]*)*""".r

  /**
    * Constructs a [[NeuronType]] instance from the specified string.
    * @param neuronType One of `input`, `hidden`, or `output`.
    * @return The neuron type based on the specified string
    */
  def from(neuronType: String): NeuronType = {
    neuronType.toLowerCase match {
      case "input" => NeuronType.Input
      case "hidden" => NeuronType.Hidden
      case "output" => NeuronType.Output
      case _ => NeuronType.Hidden
    }
  }

  /**
    * Returns the [[NeuronType]] associated with the neuron's ID. If the neuron's ID matches the [[inputRegex]], then
    * the neuron has a type of [[Input]]. If the neuron's ID matches the [[outputRegex]], then the neuron's type is
    * [[Output]]. Otherwise, the neuron's type is [[Hidden]].
    * @param neuronId The ID of the neuron.
    * @return the [[NeuronType]] associated with the neuron's ID
    */
  def of(neuronId: String): NeuronType = {
    neuronId match {
      case inputRegex(_*) => NeuronType.Input
      case outputRegex(_*) => NeuronType.Output
      case _ => NeuronType.Hidden
    }
  }
}
