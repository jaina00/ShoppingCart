package domain

import org.scalatest.{FlatSpecLike, Matchers}

class OfferTest extends FlatSpecLike with Matchers {

  "Apple offers" should "get offers on multiple apples" in {
    ApplesOffer(Map(Apple() -> 5)) shouldBe Some(OfferDetails(List(Apple()), "Apple on 10% discount", 5.0, 4.5))
  }

  "Soup offers" should "get no offers only 1 soup" in {
    SoupOffer(Map(Soup() -> 1, Bread() -> 5)) shouldBe None
  }

  it should "get offers if multiple soups" in {
    SoupOffer(Map(Soup() -> 4, Bread() -> 5)) shouldBe Some(OfferDetails(List(Soup(), Bread()), "Multi tins of soup gets loaf at half price", 6.60, 4.60))
  }

  "No offer products" should "get items total" in {
    ItemsWithoutOffer.processItemsWithoutOffer(Map(Soup() -> 1, Bread() -> 1, Milk() -> 1), List()) shouldBe Some(OfferDetails(List(), "", 2.75, 2.75))
  }

  it should "not consider items that have been part of offer" in {
    ItemsWithoutOffer.processItemsWithoutOffer(Map(Soup() -> 2, Bread() -> 1, Milk() -> 1), List(Soup(), Bread())) shouldBe Some(OfferDetails(List(), "", 1.30, 1.30))
  }
}
