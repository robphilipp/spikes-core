package com.digitalcipher.spiked.neurons

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import com.digitalcipher.spiked.neurons.MonostableIntegrator.{StateData, initialStateData}
import com.digitalcipher.spiked.neurons.Neuron._
import com.digitalcipher.spiked.neurons.SignalReceiver.{IdQuery, TimeFactorQuery}
import com.digitalcipher.spiked.construction.description.{LearningFunctionDescription, NoLearningParams}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import squants.electro.{Millivolts, Webers}
import squants.motion.MetersPerSecond
import squants.space.Millimeters
import squants.time.{Milliseconds, Seconds}

object MonostableIntegratorTest {
  val actorSystemName = "monostable-neuron-tests"
}

/**
  * Test cases for the monostable-integrator neuron running as an actor
  */
class MonostableIntegratorTest extends TestKit(ActorSystem(MonostableIntegratorTest.actorSystemName)) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "an echo actor" must {
    "send back messages unchanged" in {
      val echo = system.actorOf(TestActors.echoActorProps)
      echo ! "hello world"
      expectMsg("hello world")
    }
  }

  "a monostable-integrator neuron" must {
    // create a neuron with all the the default properties
    val neuron = system.actorOf(MonostableIntegrator.props(id = "test-neuron", timeFactor = 314))

    "send back its id when requested" in {
      neuron ! IdQuery()
      expectMsg("test-neuron")
    }

    "send back its time-factor when requested" in {
      neuron ! TimeFactorQuery()
      expectMsg(314)
    }

    "send back the neuron's state when requested" in {
      neuron ! TimingQuery()
      val timing = expectMsgType[TimingResponse]

      neuron ! StateDataQuery()
      expectMsg(initialStateData(
        clock = timing.clock,
        synapseTiming = SignalReleaseProbability.default()
      ))
    }

    "send back the neuron's post-synaptic connections when requested" in {
      neuron ! WeightsQuery()
      expectMsg(Map())
    }

    "send back the neuron's pre-synaptic weights when requested" in {
      neuron ! WeightsQuery()
      expectMsg(Map())
    }

    "send back an error message when it receives an invalid message" in {
      neuron ! "invalid message"
      expectMsg("Invalid message")
    }
  }

  "a monostable-integrator neuron network with 2 input neurons connected to a single output neuron" must {
    // create the input and output neurons
    val input1 = system.actorOf(MonostableIntegrator.props(id = "input-1", timeFactor = 1, conductanceSpeed = MetersPerSecond(1)))
    val input2 = system.actorOf(MonostableIntegrator.props(id = "input-2", timeFactor = 1, conductanceSpeed = MetersPerSecond(3)))
    val output1 = system.actorOf(MonostableIntegrator.props(id = "output-1", timeFactor = 1))

    // connect the input neurons to the output neuron
    input1 ! Connect(postSynaptic = output1, weight = 1.1, equilibriumWeight = 1.11, distance = Millimeters(10), learning = LearningFunctionDescription(NoLearningParams()))
    input2 ! Connect(postSynaptic = output1, weight = 2.2, equilibriumWeight = 2.22, distance = Millimeters(20), learning = LearningFunctionDescription(NoLearningParams()))

    "have post-synaptic connections from each input neuron to the output neuron" in {
      input1 ! ConnectionsQuery()
      expectMsg(Map(output1 -> Millimeters(10) / MetersPerSecond(1)))
      input2 ! ConnectionsQuery()
      expectMsg(Map(output1 -> Millimeters(20) / MetersPerSecond(3)))
      output1 ! ConnectionsQuery()
      expectMsg(Map())
    }

    "have an output neuron with pre-synaptic weights to each input neuron" in {
      output1 ! WeightsQuery()
      expectMsg(Map(input1.path.name -> Weight(1.1, 1.11), input2.path.name -> Weight(2.2, 2.22)))
    }
  }

  "in response to incoming signals a monostable-integrator neuron" must {

    "update its state when it receives a signal" in {
      // create a neuron with zero noise, really short refractory period, and no refractoriness
      val neuron = system.actorOf(MonostableIntegrator.props(
        id = "test-neuron",
        timeFactor = 1,
        threshold = Millivolts(5),
        spikePotential = Millivolts(1),
        membranePotentialNoiseMagnitude = Millivolts(0),
        decayHalfLife = Milliseconds(100),
        refractoryPeriod = Milliseconds(1),
        baseRefractoriness = Webers(0)    // turn off the refractoriness
      ))

      // initialize the neuron's time and ensure that the times are within tolerance
      val clock = SignalClock.nowWithTimeFactor(1)

      // ask the neuron to initialize its clock to the current clock's start time
      neuron ! InitializeTime(clock.startTime)

      // request the neuron's timing information, which should be the same as the original
      // clock's start time and no timing adjustment should have been made
      neuron ! TimingQuery()
      val timing = expectMsgType[TimingResponse]
      assert(timing.clock.startTime == clock.startTime)
      assert(timing.timingAdjustment == 0)

      // query the initial state which should zero across the board
      neuron ! StateDataQuery()
      val initialState = expectMsgType[StateData]
      assert(initialState.membranePotential.toMillivolts == 0)
      assert(initialState.lastFire.toMilliseconds == 0)
      assert(initialState.lastEvent.toMilliseconds == 0)

      // send it a signal after about 100 ms (this won't be exact) but capture that approximate delay
      Thread.sleep(100)
      neuron ! SimulationTimeQuery()
      val simulationTime = expectMsgType[SimulationTimeResponse]
      neuron ! Signal(timestamp = simulationTime.currentTime, preSynaptic = SignalReceiver.world, value = Millivolts(2))

      // query the state
      neuron ! StateDataQuery()
      val state = expectMsgType[StateData]
      assert(state.membranePotential.toMillivolts == 2)
      assert(state.lastFire.toMilliseconds == 0)
    }

    "update its state with refractoriness when it receives a signal" in {
      val refractoryPeriod = Milliseconds(20)
      val baseRefractoriness = Webers(1e-7)

      // create a neuron with zero noise
      val neuron = system.actorOf(MonostableIntegrator.props(
        id = "test-neuron",
        timeFactor = 1,
        threshold = Millivolts(5),
        spikePotential = Millivolts(1),
        membranePotentialNoiseMagnitude = Millivolts(0),
        decayHalfLife = Seconds(10000),
        refractoryPeriod = refractoryPeriod,
        baseRefractoriness = baseRefractoriness
      ))

      // initialize the neuron's time and ensure that the times are within tolerance
      val clock = SignalClock.nowWithTimeFactor(1)
      neuron ! InitializeTime(clock.startTime)
      neuron ! TimingQuery()
      val timing = expectMsgType[TimingResponse]
      assert(timing.clock.startTime == clock.startTime)
      assert(timing.timingAdjustment == 0)

      // query the initial state which should zero across the board
      neuron ! StateDataQuery()
      val initialState = expectMsgType[StateData]
      assert(initialState.membranePotential.toMillivolts == 0)
      assert(initialState.lastFire.toMilliseconds == 0)
      assert(initialState.lastEvent.toMilliseconds == 0)

      // send it a signal after about 100 ms (this won't be exact) but capture that approximate delay
      Thread.sleep(100)
      neuron ! SimulationTimeQuery()
      val simulationTime = expectMsgType[SimulationTimeResponse]
      val signal = Signal(timestamp = simulationTime.currentTime, preSynaptic = SignalReceiver.world, value = Millivolts(2))
      neuron ! signal

      // query the state and determine the membrane potential with refractoriness
      neuron ! StateDataQuery()
      val state = expectMsgType[StateData]

      state.lastEvent.toMilliseconds should equal (signal.timestamp.toMilliseconds +- 3)
      assert(state.lastEvent >= signal.timestamp)

      assert(state.membranePotential == Millivolts(2) - baseRefractoriness / (state.lastEvent - state.lastFire - refractoryPeriod))
      assert(state.lastFire.toMilliseconds == 0)
    }

    "update its state based on a decayed membrane potential when it receives 2 signal separated in time" in {
      // create a neuron with zero noise, really short refractory period, and no refractoriness
      val decayHalfLife = Milliseconds(100)
      val neuron = system.actorOf(MonostableIntegrator.props(
        id = "test-neuron",
        timeFactor = 1,
        threshold = Millivolts(5),
        spikePotential = Millivolts(1),
        membranePotentialNoiseMagnitude = Millivolts(0),
        decayHalfLife = decayHalfLife,
        refractoryPeriod = Milliseconds(1),
        baseRefractoriness = Webers(0)    // turn off the refractoriness
      ))

      // initialize the neuron's time and ensure that the times are within tolerance
      val clock = SignalClock.nowWithTimeFactor(1)
      neuron ! InitializeTime(clock.startTime)

      // send it a signal after about 100 ms (this won't be exact) but capture that approximate delay
      Thread.sleep(100)
      var simulationTime = clock.now()
      neuron ! Signal(timestamp = simulationTime, preSynaptic = SignalReceiver.world, value = Millivolts(2))

      // query the state
      neuron ! StateDataQuery()
      val state = expectMsgType[StateData]
      assert(state.membranePotential.toMillivolts == 2)
      assert(state.lastFire.toMilliseconds == 0)

      // and after approximately 100 ms, send a 1 mV signal
      Thread.sleep(100)
      simulationTime = clock.now()
      neuron ! Signal(timestamp = simulationTime, preSynaptic = SignalReceiver.world, value = Millivolts(1))

      // query the state after the second signal
      neuron ! StateDataQuery()
      val updatedState = expectMsgType[StateData]
      // calculate the amount that the previous membrane potential has decayed since the last signal
      val decayed = 2 * math.exp(-(updatedState.lastEvent - state.lastEvent)/decayHalfLife)
      assert(updatedState.membranePotential.toMillivolts == decayed + 1)
      assert(updatedState.lastFire.toMilliseconds == 0)
    }

    "update its state, but membrane potential remains at 0, when it receives a signal during refractory period" in {
      // create a neuron with zero noise, really short refractory period, and no refractoriness
      val neuron = system.actorOf(MonostableIntegrator.props(
        id = "test-neuron",
        timeFactor = 1,
        threshold = Millivolts(5),
        spikePotential = Millivolts(1),
        membranePotentialNoiseMagnitude = Millivolts(0),
        decayHalfLife = Milliseconds(100),
        refractoryPeriod = Milliseconds(100)
      ))

      // initialize the neuron's time and ensure that the times are within tolerance
      val clock = SignalClock.nowWithTimeFactor(1)
      neuron ! InitializeTime(clock.startTime)
      neuron ! TimingQuery()
      val timing = expectMsgType[TimingResponse]
      assert(timing.clock.startTime == clock.startTime)
      assert(timing.timingAdjustment == 0)

      // query the initial state which should zero across the board
      neuron ! StateDataQuery()
      val initialState = expectMsgType[StateData]
      assert(initialState.membranePotential.toMillivolts == 0)
      assert(initialState.lastFire.toMilliseconds == 0)
      assert(initialState.lastEvent.toMilliseconds == 0)

      // send a large signal to get the neuron to fire
      val initialSignal = clock.now()
      neuron ! Signal(timestamp = initialSignal, preSynaptic = SignalReceiver.world, value = Millivolts(6))
      Thread.sleep(5)

      // send it a signal while the neuron is still in the refractory period
      val simulationTime = clock.now()
      neuron ! Signal(timestamp = simulationTime, preSynaptic = SignalReceiver.world, value = Millivolts(2))

      // the membrane potential should still be 0 mV because the neuron received the signal in
      // the refractory period
      neuron ! StateDataQuery()
      val state = expectMsgType[StateData]
      assert(state.membranePotential.toMillivolts == 0)
      assert(state.lastFire >= initialSignal)
    }
  }
}