package lab03

import org.junit.*
import org.junit.Assert.*

class lab03Test:
  
  /**
   * Task 1
  */
  import lab03.Sequences.*
  import Sequence.*

  val lst1 = Cons(10, Cons(20, Cons(30, Nil())))
  val lst2 = Cons("a", Cons("b", Cons("c", Nil())))
  val lst3 = Cons(10, Cons(20, Nil()))
  val lst4 = Cons(30, Cons(40, Nil()))

  @Test def testTake(): Unit = {
    assertEquals(Cons(10, Cons(20, Nil())), take(lst1, 2))
    assertEquals(Nil(), take(lst1, 0))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(lst1, 5))
  }

  @Test def testZip(): Unit = {
    assertEquals(Cons((10, "a"), Cons((20, "b"), Cons((30, "c"), Nil()))), zip(lst1, lst2))
  }

  @Test def testConcat(): Unit = {
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), concat(lst3, lst4))
  }

  @Test def testFlatMap(): Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst1)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
    flatMap(lst1)(v => Cons(v + 1, Cons(v + 2, Nil()))))
  }

  @Test def testMap(): Unit = {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(lst1)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(lst1)(_ + ""))
  }

  @Test def testFilter(): Unit = {
    assertEquals(Cons(20, Cons(30, Nil())), filter(lst1)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(lst1)(_ != 20))
  }
  
  /**
   * Task 2
  */
  import lab03.*
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
  }

  /**
   * Task 5
  */
   
