package domain

case class OfferDetails(offerItems: List[Item], offerTitle: String, subTotal: BigDecimal, total: BigDecimal)

sealed trait Offers {
  val offerItems :List[Item]
  val offerMessage: String

  def apply(items: Map[Item, Int]): Option[OfferDetails]
}

object ApplesOffer extends Offers {
  val offerItems :List[Item] = List(Apple())
  val offerMessage: String = "Apple on 10% discount"

  def apply(items: Map[Item, Int]): Option[OfferDetails] = {
    items.get(Apple())
      .map(quantity => {
        val subTotal = Apple().unitPrice * quantity
        val total = (Apple().unitPrice * quantity * 90) / 100
        OfferDetails(offerItems, offerMessage, subTotal, total)
      })
  }
}

object SoupOffer extends Offers {
  val offerItems :List[Item] = List(Soup(), Bread())

  val offerMessage: String = "Multi tins of soup gets loaf at half price"

  def apply(items: Map[Item, Int]): Option[OfferDetails] = {
    val prices = for {
      soupCount <- items.get(Soup())
      breadCount <- if (soupCount >= 2) items.get(Bread()) else None
    } yield {
      val subTotal = soupCount * Soup().unitPrice + breadCount * Bread().unitPrice
      val total = (Soup().unitPrice * soupCount) + (Bread().unitPrice * breadCount * 50) / 100
      (subTotal, total)
    }
    prices.map(total => OfferDetails(offerItems, offerMessage, total._1, total._2))
  }
}

object ItemsWithoutOffer {
  private def filterOfferedItems(itemsMap: Map[Item, Int], itemsWithOffers: List[Item]): Map[Item, Int] = {
    val mutableItemMAp = collection.mutable.Map(itemsMap.toSeq: _*)
    itemsWithOffers.foreach(x => mutableItemMAp -= x)
    mutableItemMAp.toMap
  }


  def processItemsWithoutOffer(input: Map[Item, Int], itemsWithOffers: List[Item]): Option[OfferDetails] = {
    val remainingItems = filterOfferedItems(input, itemsWithOffers)
    if (remainingItems.nonEmpty) {
      val remainingItemsTotal: BigDecimal = remainingItems
        .foldLeft(BigDecimal(0)) { (e, acc) =>
          e + (acc._1.unitPrice * acc._2)
        }
      Some(OfferDetails(List.empty, "", remainingItemsTotal, remainingItemsTotal))
    } else {
      None
    }
  }

}

case class Output(subTotal: BigDecimal, discountText: String, totalPrice: BigDecimal) {
  override def toString: String = s"====================\nSubtotal: $subTotal\n$discountText\nTotal price: $totalPrice\n===================="
}