# Spikes

An event-based quasi-real-time spiking neural network. Spikes attempts to model neural network dynamics, that are reasonably
grounded biological neural networks, based on the notion that learning, memory, and thought emerge from dynamical processes
determined by the structure and behavior of a collection of neurons that are continuously active.

To accomplish this, **Spikes** adds time and geometry to the topology of neural networks. In **Spikes**, the geometry of the network
determines signal propagation times, and time effects the decay of the neurons' membrane potential, the decay of their connection weights,
and the adjustment of connection weights based on signal arrival time relative to pre-synaptic spikes (learning, spike 
time-dependent plasticity). 

The arrival of a signal from a pre-synaptic neuron, or the environment, is an *event*. Events have an
associated time. The time of the event causes the neuron to recalculate its state relative the last event, and then
update itself based on the event.