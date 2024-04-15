package ex3

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable.ListBuffer
import java.util.Random
import scala.collection.mutable.ArrayBuffer

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def separate: Unit = println("<><><><><><><><><>")

@main def checkPerformance: Unit =
  import PerformanceUtils.*
  
  val upperBound = 10_000_000
  val lowerBound = 1
  val position = 100_000
  val rangeValues = (lowerBound to upperBound)

  /* Linear sequences: List, ListBuffer */
  var immList = rangeValues.toList
  var mutList = ListBuffer.range(lowerBound, upperBound)
  val randomUpdateIndex = Random().nextInt(position, upperBound)
  println("[Linear Sequences]")
  assert(measure("[Immutable List] Get element")(immList(position)) > measure("[Mutable List] Get element")(mutList(position)))
  separate
  println("Random position index: " + randomUpdateIndex)
  assert(measure("[Immutable List] Update element")(immList.updated(randomUpdateIndex, 2)) > measure("[Mutable List] Update Element")(mutList.update(randomUpdateIndex, 1))) 
  separate
  assert(measure("[Immutable List] Remove element")(immList.drop(randomUpdateIndex)) < measure("[Mutable List] Remove element")(mutList.drop(randomUpdateIndex))) 
  immList = null
  mutList = null
  /* Indexed sequences: Vector, Array, ArrayBuffer */
  separate
  println("[Indexed Sequences]")
  val vector = Vector.range(lowerBound, upperBound)
  val array = Array.range(lowerBound, upperBound)
  val arrayBuffer = ArrayBuffer.range(lowerBound, upperBound)
  assert(measure("[Vector] Get element")(vector(position)) >= measure("[Array] Get element")(array(randomUpdateIndex)))
  assert(measure("[Vector] Get element")(vector(position)) < measure("[ArrayBuffer] Get element")(arrayBuffer(randomUpdateIndex)))
  separate

  /* Sets */

  /* Maps */

  /* Comparison */
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))
