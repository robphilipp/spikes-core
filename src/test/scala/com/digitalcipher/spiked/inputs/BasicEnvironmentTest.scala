package com.digitalcipher.spiked.inputs

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.digitalcipher.spiked.neurons.{Signal, SignalClock}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import squants.electro.Millivolts
import squants.time.Milliseconds

object BasicEnvironmentTest {
  val actorSystemName = "basic-environment-test"
}

class BasicEnvironmentTest extends TestKit(ActorSystem(BasicEnvironmentTest.actorSystemName)) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "a fake neuron" must {
    "respond when a signal is sent" in {
      val neuron: ActorRef = system.actorOf(FakeNeuron.props())

      neuron ! Signal(Milliseconds(1000), ActorRef.noSender, Millivolts(10))
      expectMsg(s"10.0; ${ActorRef.noSender}; 1000.0")
    }
  }

  "a basic environment" must {
    "send period signals for a specified duration to one of 10 neurons" in {
      val probes: Seq[TestProbe] = for(i <- 1 to 10) yield TestProbe(s"neuron-$i")
      import scala.concurrent.duration._

      val timeFactor = 1
      val duration: FiniteDuration = 1.second
      BasicEnvironment.from(
        system = system,
        neurons = probes.map(probe => probe.ref),
        clock = SignalClock.nowWithTimeFactor(timeFactor),
        initialDelay = 0 seconds,
        period = 100 millis,
        duration = duration,
        signals = (neurons, _) => Map(neurons.head -> Millivolts(1))
      )

      // the first neuron should receive all the signals (as specified in the construction
      // of the basic environment
      val signals = probes.head.receiveN(10, duration * timeFactor).asInstanceOf[Seq[Signal]]
      for((signal, i) <- signals.view.zipWithIndex) {
        assert(signal.timestamp.within(Milliseconds(i * 100 - 30) to Milliseconds(i * 100 + 30)))
        assert(signal.value == Millivolts(1))
        assert(signal.preSynaptic == ActorRef.noSender)
      }

      // the remaining neurons should not get any signals
      for(probe <- probes.tail) {
        println(probe.testActor.path.name)
        probe.expectNoMessage(100 millis)
      }
    }

    "send one signal to each neuron" in {
      val probes: Seq[TestProbe] = for(i <- 1 to 10) yield TestProbe(s"neuron-$i")
      import scala.concurrent.duration._

      val timeFactor = 1
      val duration: FiniteDuration = 1.second
      BasicEnvironment.from(
        system = system,
        neurons = probes.map(probe => probe.ref),
        clock = SignalClock.nowWithTimeFactor(timeFactor),
        initialDelay = 0 seconds,
        period = 100 millis,
        duration = duration,
        signals = (neurons, _) => Map(neurons.head -> Millivolts(1))
      )

      // the first neuron should receive all the signals (as specified in the construction
      // of the basic environment
      val signals = probes.head.receiveN(10, duration * timeFactor).asInstanceOf[Seq[Signal]]
      for((signal, i) <- signals.view.zipWithIndex) {
        assert(signal.timestamp.within(Milliseconds(i * 100 - 30) to Milliseconds(i * 100 + 30)))
        assert(signal.value == Millivolts(1))
        assert(signal.preSynaptic == ActorRef.noSender)
      }

      // the remaining neurons should not get any signals
      for(probe <- probes.tail) {
        println(probe.testActor.path.name)
        probe.expectNoMessage(100 millis)
      }
    }
  }
}

