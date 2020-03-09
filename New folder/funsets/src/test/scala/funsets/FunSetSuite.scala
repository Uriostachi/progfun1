package funsets

import org.junit._
import org.junit.Assert.assertEquals
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

  trait SecondaryTestSets {
    val s1 = Set(1, 2, 3, 4)
    val s2 = Set(3, 4, 5, 6)
    val s3 = Set(1, 2, 5, 6)
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

  @Test def `SingletonSet creates a set`: Unit = {
    val s = singletonSet(1)
    assert(contains(s, 1))
  }


  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `Intersection contains the right elements`: Unit = {
    new SecondaryTestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1))
      assert(!contains(s, 2))
      assert(contains(s, 3))
      assert(contains(s, 4))
    }
  }

  @Test def `Difference returns elements of s that are not in t`: Unit = {
    new SecondaryTestSets {
      val s = diff(s1, s3)
      assert(!contains(s, 1))
      assert(!contains(s, 2))
      assert(contains(s, 3))
      assert(contains(s, 4))
    }
  }

  @Test def `filter returns values that passes the test`: Unit = {
    new SecondaryTestSets {
      val s = filter(s1, x => x < 3)
      assert(contains(s, 1))
      assert(contains(s, 2))
      assert(!contains(s, 3))
      assert(!contains(s, 4))
    }
  }

  @Test def `forall check everything passes the test`: Unit = {
    new SecondaryTestSets {
      assert(forall(s1, x => x < 5))
      assert(!forall(s1, x => x < 2))
    }
  }

  @Test def `exists check that at least one passes the test`: Unit = {
    new SecondaryTestSets {
      assert(!exists(s1, x => x > 5))
      assert(exists(s1, x => x < 2))
    }
  }

  @Test def `map apply the function to each element`: Unit = {
    new SecondaryTestSets {
      val s = map(s1, x => x + 5)
      printSet(s1)
      printSet(s)

      assert(contains(s, 6))
      assert(contains(s, 7))
      assert(contains(s, 8))
      assert(contains(s, 9))
    }
  }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
