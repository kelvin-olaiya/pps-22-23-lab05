package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def recursiveZipRight: List[(A, Int)] =
    def zip(l: List[A], idx: Int): List[(A, Int)] = l match
      case h :: t => (h, idx) :: zip(t, idx + 1)
      case _ => Nil[(A, Int)]()
    zip(this, 0)

  def zipRight: List[(A, Int)] = foldRight(Nil())((elem, acc) => (elem, acc match
    case Nil() => length - 1
    case (_, i) :: _ => i - 1) :: acc
  )

  def foldLeftRight[B, C](z: B)(s: C)(left: (A, B) => B)(right: (C, B) => C): C = this match
    case h :: t =>
      val r = left(h, z)
      right(t.foldLeftRight(r)(s)(left)(right), r)
    case _ => s

  def mapFoldLeftRight[B, C, D](z: B)(s: D)(map: (A, B) => C)(left: B => B)(right: (D, C) => D): D = this match
    case h :: t => right(t.mapFoldLeftRight(left(z))(s)(map)(left)(right), map(h, z))
    case _ => s

  def zipRight2: List[(A, Int)] = head.map(h =>
    foldLeftRight((h, -1))(Nil[(A, Int)]())((elem, prev) => (elem, prev._2 + 1))((acc, elem) => elem :: acc)
  ).getOrElse(Nil())

  def zipRight3: List[(A, Int)] = mapFoldLeftRight(0)(Nil[(A, Int)]())((_, _))(_ + 1)((acc, elem) => elem :: acc)

  def partition(pred: A => Boolean): (List[A], List[A]) = foldRight((Nil(), Nil()))((elem, acc) => acc match
    case (matching, other) if pred(elem) => (elem :: matching, other)
    case (matching, other) => (matching, elem :: other)
  )

  def span(pred: A => Boolean): (List[A], List[A]) = foldLeft((Nil(), Nil())) ((acc, elem) => acc match
    case (matching, Nil()) if pred(elem) => (matching.append(elem :: Nil()), Nil())
    case (matching, other) => (matching, other.append(elem :: Nil()))
  )

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw UnsupportedOperationException()
    case h :: t => t.foldLeft(h)(op)

  def takeRight(n: Int): List[A] = zipRight filter (_._2 >= length - n) map (_._1)

  def collect[B](pf: PartialFunction[A, B]): List[B] = filter(pf.isDefinedAt) map pf

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)
