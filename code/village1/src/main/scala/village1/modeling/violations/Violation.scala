package village1.modeling.violations

trait Violation {
  val `type`: String
  val description: String
  override def toString: String = `type`
}
