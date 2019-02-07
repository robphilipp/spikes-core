package com.digitalcipher.spiked.serialization

import com.esotericsoftware.kryo.{Kryo, Serializer}
import com.esotericsoftware.kryo.io.{Input, Output}
import com.digitalcipher.spiked.construction.description.LocationDescription

class LocationSerializer extends Serializer[LocationDescription] {
  override def read(kryo: Kryo, input: Input, clazz: Class[LocationDescription]): LocationDescription = {
    val coordinates = input.readString().split( "," )
    val coordinate = (coordinates(0), coordinates(1), coordinates(2))
    val coordinateType = coordinates(3)
    new LocationDescription(coordinate, coordinateType)
  }

  override def write(kryo: Kryo, output: Output, location: LocationDescription): Unit = {
    output.writeString(
      s"${location.point._1.toString},${location.point._2.toString},${location.point._3.toString},${location.coordinateType}"
    )
  }
}
