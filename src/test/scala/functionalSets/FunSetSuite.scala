package functionalSets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
 @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect returns common elements`: Unit = {
    new TestSets {
      val s = intersect(s1, union(s1, s2))
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

    @Test def `diff returns diff elements`: Unit = {
      new TestSets {
        val s = diff(union(union(s1, s2), s3), s1)
        assert(contains(s, 2), "diff 2")
        assert(contains(s, 3), "diff 3")
        assert(!contains(s, 1), "diff 1")
      }
  }

  @Test def `filter returns filtered elements`: Unit = {
    new TestSets {
      val s = filter(union(union(s1, s2), s3), x => x > 2)
      assert(contains(s, 3), "filter 3 ")
      assert(!contains(s, 2), "filter 2")
      assert(!contains(s, 1), "filter 1")
    }
  }

  @Test def `forAll test elems greater than zero`: Unit = {
    new TestSets {
      val result = forall(union(union(s1, s2), s3), x => x > 0)
      assert(result == true, "all elems are greater than zero")
    }
  }

  @Test def `forAll test elems greater than zero 2`: Unit = {
    new TestSets {
      val negElem = singletonSet(-1)
      val result = forall(union(union(s1, s2), negElem), x => x > 0)
      assert(result == false, "not all elems are greater than zero")
    }
  }

  @Test def `exists test`: Unit = {
    new TestSets {
      val result1 = exists(union(union(s1, s2), s3), x => x > 2)
      assert(result1 == true, "elem exists")

      val result = exists(union(s1, s2), x => x > 2)
      assert(result1 == true, "elem do not exist")
    }
  }

  @Test def `map test`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3)

      val result = map(s, x => x * x)
      assert(contains(result, 1), "mapped 1 to 1")
      assert(contains(result, 4), "mapped 2 to 4")
      assert(contains(result, 9), "mapped 3 to 9")
    }
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
