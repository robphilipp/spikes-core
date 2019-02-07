package com.digitalcipher.spiked.serialization

import java.io.NotSerializableException

import com.esotericsoftware.kryo.io.{Input, Output}
import com.esotericsoftware.kryo.{Kryo, Serializer}
import squants.electro.ElectricPotential
import squants.motion.Velocity
import squants.space.Length
import squants.time.{Frequency, Time}

/**
  * Serializer for Squants Velocity
  */
class VelocitySerializer extends Serializer[Velocity] {
  override def read(kryo: Kryo, input: Input, clazz: Class[Velocity]): Velocity = {
    val velocity = Velocity( input.readString() )
    if( velocity.isFailure ) throw new NotSerializableException(clazz.getName) else velocity.get
  }

  override def write(kryo: Kryo, output: Output, velocity: Velocity): Unit = {
    output.writeString(velocity.toString)
  }
}

class TimeSerializer extends Serializer[Time] {
  override def read(kryo: Kryo, input: Input, clazz: Class[Time]): Time = {
    val time = Time( input.readString() )
    if( time.isFailure ) throw new NotSerializableException(clazz.getName) else time.get
  }

  override def write(kryo: Kryo, output: Output, time: Time): Unit = {
    output.writeString(time.toString)
  }
}

class LengthSerializer extends Serializer[Length] {
  override def read(kryo: Kryo, input: Input, clazz: Class[Length]): Length = {
    val length = Length( input.readString() )
    if( length.isFailure ) throw new NotSerializableException(clazz.getName) else length.get
  }

  override def write(kryo: Kryo, output: Output, length: Length): Unit = {
    output.writeString(length.toString)
  }
}

class ElectricPotentialSerializer extends Serializer[ElectricPotential] {
  override def read(kryo: Kryo, input: Input, clazz: Class[ElectricPotential]): ElectricPotential = {
    val electricPotential = ElectricPotential( input.readString() )
    if( electricPotential.isFailure ) throw new NotSerializableException(clazz.getName) else electricPotential.get
  }

  override def write(kryo: Kryo, output: Output, electricPotential: ElectricPotential): Unit = {
    output.writeString(electricPotential.toString)
  }
}

class FrequencySerializer extends Serializer[Frequency] {
  override def read(kryo: Kryo, input: Input, clazz: Class[Frequency]): Frequency = {
    val frequency = Frequency( input.readString() )
    if( frequency.isFailure ) throw new NotSerializableException(clazz.getName) else frequency.get
  }

  override def write(kryo: Kryo, output: Output, frequency: Frequency): Unit = {
    output.writeString(frequency.toString)
  }
}
