package ex1

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option


  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None
  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = foldRight(List[(A, B)]())
    ((elm, list) => (elm, value)::list)
  
  final def length(): Int =  foldLeft(0)((x, _) => x + 1)
  def count(e: A): Int = foldLeft(0)((counter, curr_el) => counter + (if curr_el == e then 1 else 0))
  
  
  def zipWithIndex: List[(A, Int)] = foldRight(List[(A, Int)]())((elm, list) => 
    (if list.head.isDefined then (elm, list.head.get._2 + 1) else (elm, 0))::list)

  def partition(predicate: A => Boolean): (List[A], List[A]) = foldWithPredicate(predicate)(this)
  def span(predicate: A => Boolean): (List[A], List[A]) = foldWithPredicate(predicate)(this)

  private def foldWithPredicate(predicate: A => Boolean)(list: List[A]): (List[A], List[A]) =
    var listA = List[A]()
    var listB = List[A]()
    var continue = true
    for element <- this do 
      if (predicate(element) && continue) then
        listA = listA.append(element::Nil())
      else
        continue = false
        listB = listB.append(element::Nil())
    (listA, listB)
  
  def takeRight(n: Int): List[A] = _take(0, this) 
    private def _take(n: Int, list: List[A]): List[A] = list match
      case h::t if (n > 0) => _take(n - 1, t)
      case h::t if (n == 0) => t
      case _ => list
    
    
  def collect(predicate: PartialFunction[A, A]): List[A] = foldRight((List[A]()))(
    (el, list) => (if predicate.isDefinedAt(el) then predicate(el)::list else list))
// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)
object Test extends App:

  import List.*
  val reference = List(1, 2, 3, 4)
  println("zipWithValue: " + reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println("zipWithIndex: " + reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println("partition: " + reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println("span: " + reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println("span: " + reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println("reduce: " + reference.reduce(_ + _)) // 10
  println("reduce: " + List(10).reduce(_ + _)) // 10
  println("take right: " + reference.takeRight(3)) // List(2, 3, 4)
  println("collect: " + reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)