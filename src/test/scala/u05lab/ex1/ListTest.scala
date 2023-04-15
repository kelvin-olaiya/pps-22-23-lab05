package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import u05lab.ex1.List.*

class ListTest:
  val list = "a" :: "b" :: "c" :: "d" :: Nil()
  val list1 = 1 :: 2 :: 3 :: 4 :: Nil()
  val list2 = 3 :: 4 :: 5 :: list1
  val expected = ("a", 2) :: ("b", 4) :: ("c", 8) :: ("d", 16) :: Nil()

  @Test
  def testFoldLeftRight(): Unit =
    assertEquals(
      expected,
      list.foldLeftRight(("", 1))(Nil[(String, Int)]())((a, b) => (a, b._2 * 2))((a, b) => b :: a)
    )

  @Test
  def testMapFoldLeftRight(): Unit =
    assertEquals(
      expected,
      list.mapFoldLeftRight(2)(Nil())(_ -> _)(_ * 2)((s, e) => e :: s)
    )
    assertEquals(
      "a2" :: "b4" :: "c8" :: "d16" :: Nil(),
      list.mapFoldLeftRight(2)(Nil())(_ + _)(_ * 2)((s, e) => e :: s)
    )

  @Test
  def testZipRight(): Unit =
    val zipped = (1, 0) :: (2, 1) :: (3, 2) :: (4, 3) :: Nil()
    assertEquals(zipped, list1.recursiveZipRight)
    assertEquals(zipped, list1.zipRight)
    assertEquals(zipped, list1.zipRight2)
    assertEquals(zipped, list1.zipRight3)

  @Test
  def testPartition(): Unit =
    assertEquals((2 :: 4 :: Nil(), 1 :: 3 :: Nil()), list1.partition(_ % 2 == 0))

  @Test
  def testSpan(): Unit =
    assertEquals((3 :: 4 :: 5 :: Nil(), list1), list2.span(_ >= 3))

  @Test
  def testTakeRight(): Unit =
    assertEquals(3 :: 4 :: Nil(), list1.takeRight(2))

  @Test
  def testReduce(): Unit =
    assertEquals(10, list1.reduce(_ + _))
    assertThrows(classOf[UnsupportedOperationException], () => Nil[Int]().reduce(_ + _))
