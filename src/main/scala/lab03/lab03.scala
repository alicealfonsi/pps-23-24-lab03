package lab03

object Sequences03:
  
  enum Sequence[A]:
    case Cons(head: A, tail: Sequence[A])
    case Nil()
  
  object Sequence:
    
    /**
      * Task 1
    */
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
    def foldLeft[A, B](l: Sequence[A])(initialValue: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(initialValue, h))(f)
      case _ => initialValue      

end Sequences03

/**
  * Task 5
*/
object ExtensionMethods:

  import Sequences03.*
  import Sequences03.Sequence.*
  
  extension [A](s: Sequence[A])

    def take(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => Cons(h, t.take(n - 1))
      case _ => Nil()

    def zip[B](r: Sequence[B]): Sequence[(A, B)] = (s, r) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
      case _ => Nil()

    def concat(r: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => Cons(h, t.concat(r))
      case _ => r

    def flatMap[B](f: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => f(h).concat(t.flatMap(f))
      case _ => Nil()

    def map[B](mapper: A => B): Sequence[B] = s.flatMap(a => Cons(mapper(a), Nil()))
      
    def filter(pred: A => Boolean): Sequence[A] = s.flatMap(a => a match
      case i if pred(a) => Cons(a, Nil())
      case _ => Nil()  
    )

    def foldLeft[B](initialValue: B)(f: (B, A) => B): B = s match
      case Cons(h, t) => t.foldLeft(f(initialValue, h))(f)
      case _ => initialValue

end ExtensionMethods

object Streams03:

  import Sequences03.*
  import Sequences03.Sequence.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    /**
     * Task 6
    */
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    /**
     * Task 7
    */
    def fill[A](n: Int)(element: A): Stream[A] = n match
      case n if n > 0 => cons(element, fill(n - 1)(element))
      case _ => Empty()
    
    /**
     * Task 8
    */
    def pell(n: Int): Stream[Int] =
      def computePellNumber(i: Int): Int = i match
        case 0 => 0
        case 1 => 1
        case i if i > 0 => 2 * (computePellNumber(i - 1)) + computePellNumber(i - 2)
        lazy val str1 = Stream.iterate(0)(_ +1)
        lazy val str2 = Stream.take(str1)(n)
        Stream.map(str2)(computePellNumber)   
    
end Streams03