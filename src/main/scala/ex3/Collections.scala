package ex3

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable.ListBuffer
import java.util.Random
import scala.collection.mutable.ArrayBuffer

object PerformanceUtils:
  case class MeasurementResults[T](objectName: String, result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
    override def toString(): String = "Result from the object: " + objectName + " is : " + duration

  def measure[T](msg: String, className: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(className, res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("", "")(expr)

  def separate: Unit = println("<><><><><><><><><>")

  def measureAll[A, T](operation: String)(expr: scala.collection.Seq[A]=> T)(lists: List[scala.collection.Seq[A]]): List[MeasurementResults[T]] = 
    lists.map(list => measure(operation, list.getClass().toString())(expr(list))).toList
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
  measureAll("Get a specific element")(list => list(position))(List(immList, mutList))
    .sorted
    .foreach(m => println(m))
  separate
  measureAll("Update element")(list => list.updated(position, 2))(List(immList, mutList))
    .sorted
    .foreach(m => println(m)) 
  separate
  measureAll("Drop an element")(list => list.drop(randomUpdateIndex))(List(immList, mutList))
    .sorted
    .foreach(m => println(m))

  //assert(measure("[Immutable List] Get element")(immList(position)) > measure("[Mutable List] Get element")(mutList(position)))
  separate
  println("Random position index: " + randomUpdateIndex)
  //assert(measure("[Immutable List] Update element")(immList.updated(randomUpdateIndex, 2)) > measure("[Mutable List] Update Element")(mutList.update(randomUpdateIndex, 1))) 
  separate
  //assert(measure("[Immutable List] Remove element")(immList.drop(randomUpdateIndex)) < measure("[Mutable List] Remove element")(mutList.drop(randomUpdateIndex))) 
  immList = null
  mutList = null
  /* Indexed sequences: Vector, Array, ArrayBuffer */
  separate
  println("[Indexed Sequences]")
  var vector = Vector.range(lowerBound, upperBound)
  var array = Array.range(lowerBound, upperBound)
  var arrayBuffer = ArrayBuffer.range(lowerBound, upperBound)
  separate
  measureAll("[Indexed sequences]: Get element at position")(list => list(position))(List[scala.collection.Seq[Int]](vector, array, arrayBuffer))
    .sorted
    .foreach(m => println(m))
  separate 
  measureAll("[Indexed sequences]: Update element at position")(list => list.updated(randomUpdateIndex, 2))(List(vector, array, arrayBuffer))
    .sorted
    .foreach(m => println(m))
  separate
  measureAll("[Indexed sequences]: Drop an element")(list => list.drop(randomUpdateIndex))(List(vector, array, arrayBuffer))
    .sorted
    .foreach(m => println(m))
  vector = null
  array = null
  arrayBuffer = null
  /* Sets */
  println("[Sets]")
  /* Maps */

  /* Comparison */
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  //assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))
