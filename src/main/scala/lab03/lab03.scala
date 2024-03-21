package lab03

import scala.collection.View.FlatMap

object lab03 {

  /**
   * Task 1
  */
  object Sequences:
    enum Sequence[A]:
        case Cons(head: A, tail: Sequence[A])
        case Nil()
    
    object Sequence:

        def take[A](l: Sequence[A], n: Int): Sequence[A] = l match
            case Cons(h, t) if n > 0 => Cons(h, take(t, n - 1))
            case _ => Nil()
        
        def zip[A, B](l: Sequence[A], r: Sequence[B]): Sequence[(A, B)] = (l, r) match
            case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
            case _ => Nil()
        
        def concat[A](l: Sequence[A], r: Sequence[A]): Sequence[A] = l match
            case Cons(h, t) => Cons(h, concat(t, r))
            case _ => r

        def flatMap[A, B](l: Sequence[A])(f: A => Sequence[B]): Sequence[B] = l match
            case Cons(h, t) => concat(f(h), flatMap(t)(f))
            case _ => Nil()

        def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(a => Cons(mapper(a), Nil()))
        
        def filter[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = flatMap(l)(a => a match
            case i if pred(a) => Cons(a, Nil())
            case _ => Nil()    
        )
            
  /**
   * Task 2
  */
  import u03.Optionals.*
  import Optional.*
  import lab03.Sequences.*
  import Sequence.* 

  def min(l: Sequence[Int]): Optional[Int] = l match
    case Cons(h, t) => min(t) match
        case Just(value) if value < h => Just(value)
        case _ => Just(h)      
    case _ => Empty()

  /**
   * Task 3
  */
  import u02.Modules.Person
  import u02.Modules.Person.*
  import lab03.Sequences.*
  import Sequence.*

  def matchPersonCourse(p: Sequence[Person]): Sequence[String] = p match
    case Cons(Person.Teacher(_, course), t) => Cons(course, matchPersonCourse(t))
    case Cons(_: Person.Student, t) => matchPersonCourse(t)
    case _ => Nil()

  def matchPersonCourseWithFlatMap(p: Sequence[Person]): Sequence[String] =
    flatMap(p)(p => p match 
        case Person.Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
    )

  /**
   * Task 4
  */
  def foldLeft[A, B](l: Sequence[B])(initialValue: A)(f: (A, B) => A): A = l match
    case Cons(h, t) => foldLeft(t)(f(initialValue, h))(f)
    case _ => initialValue
  
  /**
   * Task 5
  */


}