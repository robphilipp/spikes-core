package com.digitalcipher.spiked.serialization

import akka.actor.{ActorRef, Props}
import akka.event.Logging.StandardOutLogger
import com.digitalcipher.spiked.construction.NetworkBuilder
import com.esotericsoftware.kryo.Kryo
import com.digitalcipher.spiked.neurons.Neuron._
import com.digitalcipher.spiked.neurons.Signal
import com.digitalcipher.spiked.neurons.SignalReceiver.IdQuery
import com.digitalcipher.spiked.topology.Network._
import com.digitalcipher.spiked.construction.NetworkBuilder.NetworkFromDescription
import com.digitalcipher.spiked.construction.NeuronCreator._
import com.digitalcipher.spiked.construction.description.{BistableIntegratorParams, MonostableIntegratorParams, NeuronDescription, NeuronSpecificParams}
import com.digitalcipher.spiked.topology.coords.spatial.Coordinates.Cartesian
import com.digitalcipher.spiked.construction.description._
import com.romix.scala.serialization.kryo.ScalaImmutableAbstractMapSerializer
import squants.electro.ElectricPotential
import squants.motion.Velocity
import squants.space.Length
import squants.time.{Frequency, Time}

import scala.util.matching.Regex

class KryoInitializer {
  def customize(kryo: Kryo): Unit = {
    // standard serialization
    kryo.register(classOf[NetworkDescription], 50)

    // group description
    kryo.register(classOf[GroupDescription], 51)
    kryo.register(classOf[GroupParams], 5101)
    kryo.register(classOf[RemoteGroupParams], 5102)
    kryo.register(classOf[LocalGroupParams], 5103)

    kryo.register(classOf[LocationDescription], new LocationSerializer, 52)

    // connection description
    kryo.register(classOf[ConnectionDescription], 53)
    kryo.register(classOf[NeuronConnection], 5301)
    kryo.register(classOf[ConnectNeurons], 5302)
    kryo.register(classOf[ConnectNeuronsResponse], 5303)

    kryo.register(classOf[LearningFunctionDescription], 54)
    kryo.register(classOf[StdpHardLimitLearningParams], 5401)
    kryo.register(classOf[StdpSoftLimitLearningParams], 5402)
    kryo.register(classOf[NoLearningParams], 5402)

    // neurons
    kryo.register(classOf[NeuronDescription], 56)
    kryo.register(classOf[NeuronSpecificParams], 57)
    kryo.register(classOf[MonostableIntegratorParams], 58)
    kryo.register(classOf[BistableIntegratorParams], 59)

    // connection-weight decay
    kryo.register(classOf[WeightDecayDescription], 60)
    kryo.register(classOf[WeightDecaySpecificParams], 61)
    kryo.register(classOf[ExponentialDecayParams], 62)
    kryo.register(classOf[NoDecayParams], 63)

    // connection-weight limiter
    kryo.register(classOf[WeightLimitDescription], 64)
    kryo.register(classOf[WeightLimiterSpecificParams], 65)
    kryo.register(classOf[BoundedParams], 66)
    kryo.register(classOf[UnboundedParams], 67)

    // creating the neuron
    kryo.register(classOf[CreateNeuron], 68)
    kryo.register(classOf[CreateNeuronResponse], 69)
    kryo.register(classOf[CreateNeurons], 6901)
    kryo.register(classOf[CreateNeuronsResponse], 6902)

    // adding neurons to the network
    kryo.register(classOf[AddNeurons], 6903)
    kryo.register(classOf[AddNeuronsResponse], 6904)
    kryo.register(classOf[AddNeuron], 6905)
    kryo.register(classOf[AddInputNeuron], 6906)
    kryo.register(classOf[AddHiddenNeuron], 6907)
    kryo.register(classOf[AddOutputNeuron], 6908)

    // retrieving neurons from the network
    kryo.register(classOf[RetrieveNeurons], 6909)
    kryo.register(classOf[RetrieveInputNeurons], 6910)
    kryo.register(classOf[RetrieveHiddenNeurons], 6911)
    kryo.register(classOf[RetrieveOutputNeurons], 6912)
    kryo.register(classOf[RetrieveNeuronDetails], 6913)


    // serializers for squants (dimensioned values)
    kryo.register(classOf[Velocity], new VelocitySerializer, 70)
    kryo.register(classOf[Time], new TimeSerializer, 71)
    kryo.register(classOf[Length], new LengthSerializer, 72)
    kryo.register(classOf[ElectricPotential], new ElectricPotentialSerializer, 73)
    kryo.register(classOf[Frequency], new FrequencySerializer, 74)

    // scala types
    kryo.register(classOf[(Any, Any)], 200)
    kryo.register(classOf[(Any, Any, Any)], 201)
    kryo.register(classOf[Map[Any, Any]], new ScalaImmutableAbstractMapSerializer, 202)
    kryo.register(classOf[List[Any]], 203)
    kryo.register(classOf[scala.collection.immutable.$colon$colon[Any]], 204)
    kryo.register(classOf[Regex], 205)

    // java types
    kryo.register(classOf[java.lang.Class[Any]], 206)

    // akka classes
    kryo.register(classOf[ActorRef], 80)
    /*----------------------
     | repointable actor-ref is not accessible here, so it is (and must be) registered in the
     | configuration files with the same ID
     |   kryo.register(classOf[akka.actor.RepointableActorRef], 81)
     |   kryo.register(classOf[RemoteActorRef], 82)
     |   kryo.register(classOf[akka.event.EventStreamUnsubscriber$Register], 83)
     |   kryo.register(classOf[akka.event.EventStreamUnsubscriber$UnregisterIfNoMoreSubscribedChannels], 84)
    */
    kryo.register(classOf[Props], 85)

    kryo.register(classOf[StandardOutLogger], 90)

    // neuron events
    kryo.register(classOf[Signal], 100)
    kryo.register(classOf[Connect], 101)
    kryo.register(classOf[AddConnection], 102)
    kryo.register(classOf[Synapse], 103)
    kryo.register(classOf[Weight], 104)
    kryo.register(classOf[AddPreSynapticRef], 105)
    kryo.register(classOf[LearningFunctionDescription], 106)
    kryo.register(classOf[StdpHardLimitLearningParams], 107)
    kryo.register(classOf[StdpSoftLimitLearningParams], 108)
    kryo.register(classOf[IdQuery], 110)

    kryo.register(classOf[InitializeTime], 130)
    kryo.register(classOf[SimulationStart], 131)
    kryo.register(classOf[SimulationStartResponse], 132)

    // messages
    kryo.register(classOf[NetworkBuilder], 120)
    kryo.register(classOf[NetworkFromDescription], 121)
    //    // for testing
    //    kryo.register(classOf[TestMessage], 122)
    //    kryo.register(classOf[TestMessageWithArg], 123)
    //    kryo.register(classOf[SerializationTestMessage], 124)
    //    kryo.register(classOf[CreateTestMessage], 1240)
    //    // --------

    kryo.register(classOf[Cartesian], 140)

    kryo.register(classOf[SimulationTimeFactor], 150)

    kryo.setReferences(true)
  }
}
