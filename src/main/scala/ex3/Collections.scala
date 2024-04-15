package ex3

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable.ListBuffer
import java.util.Random

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
  val immList = rangeValues.toList
  val mutList = ListBuffer.range(lowerBound, upperBound)
  val randomUpdateIndex = Random().nextInt(position, upperBound)
  assert(measure("[Immutable List] Get element")(immList(position)) > measure("[Mutable List] Get element")(mutList(position)))
  separate
  assert(measure("[Immutable List] Update element")(immList.updated(randomUpdateIndex, 2)) > measure("[Mutable List] Update Element")(mutList.update(randomUpdateIndex, 1))) 
  separate



  /* Indexed sequences: Vector, Array, ArrayBuffer */


  /* Sets */

  /* Maps */

  /* Comparison */
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))
