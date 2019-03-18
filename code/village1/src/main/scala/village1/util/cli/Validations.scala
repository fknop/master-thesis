package village1.util.cli

import scopt.OParserBuilder

object Validations {
  def positive[T, C](x: T, message: String)(implicit n: Numeric[T], builder: OParserBuilder[C]): Either[String, Unit] = {
    import builder._
    import n._
    if (x > n.zero) success
    else failure(message)
  }

  def positiveOrZero[T, C](x: T, message: String)(implicit n: Numeric[T], builder: OParserBuilder[C]): Either[String, Unit] = {
    import builder._
    import n._
    if (x >= n.zero) success
    else failure(message)
  }

  def allPositive[T, C](x: Seq[T], message: String)(implicit n: Numeric[T], builder: OParserBuilder[C]): Either[String, Unit] = {
    import builder._
    import n._
    x.find(_ <= n.zero) match {
      case Some(_) => failure(message)
      case None => success
    }
  }

  def probability[C](x: Double, message: String)(implicit builder: OParserBuilder[C]) : Either[String, Unit] = {
    import builder._
    if (x >= 0.0 && x <= 1.0) success
    else failure(message)
  }
}
