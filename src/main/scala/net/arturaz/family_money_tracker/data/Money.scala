package net.arturaz.family_money_tracker.data

case class Money(whole: Int, cents: Int)
object Money {
  val zero = apply(0, 0)

  def apply(v: BigDecimal): Money = numeric.fromInt((v * 100).toIntExact)

  implicit val numeric = new Numeric[Money] {
    override def plus(x: Money, y: Money): Money = fromInt(toInt(x) + toInt(y))
    override def minus(x: Money, y: Money): Money = fromInt(toInt(x) - toInt(y))
    override def times(x: Money, y: Money): Money = fromInt(toInt(x) * toInt(y))
    override def negate(x: Money): Money = Money(-x.whole, -x.cents)
    override def fromInt(x: Int): Money = {
      val cents = x % 100
      val whole = x - cents
      Money(whole / 100, cents)
    }
    override def toInt(x: Money): Int = x.whole * 100 + x.cents
    override def toLong(x: Money): Long = x.whole * 100L + x.cents
    override def toFloat(x: Money): Float = x.whole + x.cents / 100f
    override def toDouble(x: Money): Double = x.whole + x.cents / 100d
    override def compare(x: Money, y: Money): Int = toInt(x) compare toInt(y)
  }
}
