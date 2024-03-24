package lab03

import org.junit.*
import org.junit.Assert.*

class lab03Test:
  
  /**
   * Task 1
  */
  import Sequences03.*
  import Sequence.*

  val lst1 = Cons(10, Cons(20, Cons(30, Nil())))
  val lst2 = Cons("a", Cons("b", Cons("c", Nil())))
  val lst3 = Cons(10, Cons(20, Nil()))
  val lst4 = Cons(30, Cons(40, Nil()))

  @Test def testTake(): Unit = {
    assertEquals(Cons(10, Cons(20, Nil())), take(lst1, 2))
    assertEquals(Nil(), take(lst1, 0))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(lst1, 5))
    import lab03.ExtensionMethods.take
    assertEquals(Sequence.take(lst1, 5), lst1.take(5)) // Task 5
  }

  @Test def testZip(): Unit = {
    assertEquals(Cons((10, "a"), Cons((20, "b"), Cons((30, "c"), Nil()))), zip(lst1, lst2))
    import lab03.ExtensionMethods.zip
    assertEquals(Sequence.zip(lst1, lst2), lst1.zip(lst2)) // Task 5
  }

  @Test def testConcat(): Unit = {
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), concat(lst3, lst4))
    import lab03.ExtensionMethods.concat
    assertEquals(Sequence.concat(lst3, lst4), lst3.concat(lst4)) // Task 5
  }

  @Test def testFlatMap(): Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst1)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
    flatMap(lst1)(v => Cons(v + 1, Cons(v + 2, Nil()))))
    import lab03.ExtensionMethods.flatMap
    assertEquals(Sequence.flatMap(lst1)(v => Cons(v + 1, Nil())), lst1.flatMap(v => Cons(v + 1, Nil()))) // Task 5
  }

  @Test def testMap(): Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(lst1)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(lst1)(_ + ""))
    import lab03.ExtensionMethods.map
    assertEquals(Sequence.map(lst1)(_ + 1), lst1.map(_ + 1)) // Task 5
  }

  @Test def testFilter(): Unit = {
    assertEquals(Cons(20, Cons(30, Nil())), filter(lst1)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(lst1)(_ != 20))
    import lab03.ExtensionMethods.filter
    assertEquals(Sequence.filter(lst1)(_ >= 20), lst1.filter(_ >= 20)) // Task 5
  }
  
  /**
   * Task 2
  */
  import u03.Optionals.*
  import Optional.*

  @Test def testMin(): Unit = {
    assertEquals(Just(10), min(lst1))
    assertEquals(Just(10), min(lst3))
    assertNotEquals(Just(40), min(lst4))
  }
  
  /**
   * Task 3
  */
  import u02.Modules.Person
  import u02.Modules.Person.*
  
  val listPerson: Sequence[Person]= Cons(Student("Alice", 2015), Cons(Student("Bob", 2016),
    Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Ricci", "PCD"), Nil()))))
  
  @Test def testMatchPersonCourse(): Unit = {
    assertEquals(Cons("PPS", Cons("PCD", Nil())), matchPersonCourse(listPerson))
  }

  @Test def testMatchPersonCourseWithFlatMap(): Unit = {
    assertEquals(Cons("PPS", Cons("PCD", Nil())), matchPersonCourseWithFlatMap(listPerson))
  }

  /**
   * Task 4
  */
  @Test def testFoldLeft(): Unit = {
    val listToFold = Cons(3, Cons(7, (Cons(1, Cons(5, Nil())))))
    val emptyListToFold: Sequence[Int] = Nil()
    assertEquals(-16, foldLeft(listToFold)(0)(_ - _))
    assertEquals(20, foldLeft(listToFold)(4)(_ + _))
    assertEquals(1, foldLeft(emptyListToFold)(1)(_ + _))
    import lab03.ExtensionMethods.foldLeft
    assertEquals(Sequence.foldLeft(listToFold)(4)(_ + _), listToFold.foldLeft(4)(_ + _)) // Task 5
  }
   
  /**
   * Task 6
  */
  import Streams03.*
  import Stream.*
  
  @Test def testTakeWhile(): Unit = {
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.takeWhile(str1)(_ < 5)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))
  }
  
  /**
   * Task 7
  */
  @Test def testFill(): Unit = {
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(Stream.fill(3)("a")))
    assertEquals(Nil(), Stream.toList(Stream.fill(0)("a")))
    assertEquals(Nil(), Stream.toList(Stream.fill(-3)("a")))
  }

  /**
   * Task 8
  */
  @Test def testPell(): Unit = {
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(Stream.pell(5)))
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Cons(29, Cons(70, Nil()))))))), Stream.toList(Stream.pell(7)))
  }