package domain

import org.scalatest.{FlatSpecLike, Matchers}

class OutputTest extends FlatSpecLike with Matchers {
  "Output" should "generate message in expected format" in {
    Output(1.0,"offer name : offer discount",1.0).toString shouldBe "====================\nSubtotal: 1.0\noffer name : offer discount\nTotal price: 1.0\n===================="
  }

}
