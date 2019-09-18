package org.ml4ai

import org.ml4ai.mdp.ExplorationDouble
import org.scalatest.{FlatSpec, Matchers}

class TestMDPElements extends FlatSpec with Matchers {

  "ExplorationDouble Action" should "be commutative on its arguments" in {
    val a = ExplorationDouble(Seq("10"), Seq("20"))
    val b = ExplorationDouble(Seq("20"), Seq("10"))
    val c = ExplorationDouble(Seq("10"), Seq("30"))

    // Test the equals function
    a should equal (b)
    b should equal (a)

    // Test the hashes
    a.## should equal (b.##)
    b.## should equal (a.##)

    // Just to rule out a trivial equivalency
    a should not equal c
    b should not equal c
  }

}
