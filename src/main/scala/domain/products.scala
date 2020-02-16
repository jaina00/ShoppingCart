package domain

object Products {
  val APPLES = "apple"
  val SOUP = "soup"
  val BREAD = "bread"
  val MILK = "milk"
}

sealed trait Item {
  val unitPrice: BigDecimal
}
case class Soup(unitPrice: BigDecimal = 0.65) extends Item
case class Bread(unitPrice: BigDecimal = 0.80) extends Item
case class Milk(unitPrice: BigDecimal = 1.30) extends Item
case class Apple(unitPrice: BigDecimal = 1.00) extends Item


object FactoryHelper {
  import Products._
  def getItem(name: String): Either[String, Item] = name.toLowerCase match {
    case SOUP => Right(Soup())
    case BREAD => Right(Bread())
    case MILK => Right(Milk())
    case APPLES => Right(Apple())
    case x => Left("unknown item " + x)
  }
}