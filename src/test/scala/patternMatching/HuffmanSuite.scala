package patternMatching

import org.junit.Assert.assertEquals
import org.junit._

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, quickEncode(t1)("ab".toList)))
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  /// TDD implementation

  @Test def `Times: Empty list returns empty list`: Unit = {
    val empty = List()
    assert(times(empty).isEmpty)
  }

  @Test def `Times: One elem list with one elem count 1`: Unit = {
    val oneElem = List('A')
    val expectedTimes = List(('A', 1))
    assertEquals(times(oneElem), expectedTimes)
  }

  @Test def `Times: Two same elem list with one elem count 2`: Unit = {
    val oneElem = List('A', 'A')
    val expectedTimes = List(('A', 2))
    assertEquals(times(oneElem), expectedTimes)
  }

  @Test def `Times: Two dif elem list with two elem count 1`: Unit = {
    val oneElem = List('A', 'B', 'A', 'B', 'C', 'B')
    val expectedTimes = List(('A', 2), ('B', 3), ('C', 1))
    assertEquals(times(oneElem), expectedTimes)
  }

  @Test def `Combine: Empty returns empty`: Unit = {
    assert(combine(List()).isEmpty)
  }

  @Test def `Combine: One leef returns leef`: Unit = {
    assertEquals(combine(List(Leaf('A', 1))), List(Leaf('A',1)))
  }

  @Test def `Combine: Two leefs are combined in a fork`: Unit = {
    val leaf1 = Leaf('A', 1)
    val leaf2 = Leaf('B', 2)
    val expectedResult = Fork(leaf1,leaf2, List(leaf1.char, leaf2.char), leaf1.weight + leaf2.weight)
    assertEquals(List(expectedResult), combine(List(leaf1, leaf2)))
  }

  @Test def `Combine: Three leefs are combined in a fork + one remainig leaf`: Unit = {
    val leaf1 = Leaf('A', 1)
    val leaf2 = Leaf('B', 2)
    val leaf3 = Leaf('C', 3)
    val fork = Fork(leaf1,leaf2, List(leaf1.char, leaf2.char), leaf1.weight + leaf2.weight)
    assertEquals(List(fork, leaf3), combine(List(leaf1, leaf2, leaf3)))
  }

  @Test def `Combine: Three leefs are combined in a fork + one remainig leaf ordered correctly`: Unit = {
    val leaf1 = Leaf('A', 2)
    val leaf2 = Leaf('B', 3)
    val leaf3 = Leaf('C', 4)
    val fork = Fork(leaf1,leaf2, List(leaf1.char, leaf2.char), leaf1.weight + leaf2.weight)

    val decoded = decode(frenchCode, secret)
    assertEquals(List(leaf3, fork), combine(List(leaf1, leaf2, leaf3)))
  }
}
