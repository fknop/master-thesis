package village1.modeling.violations

case class WorkingRequirementViolation(worker: Int, min: Option[Int], max: Option[Int], value: Int) extends Violation {
  override val `type`: String = "WorkingRequirementViolation"
  private def workingRange: String = {
    val a = min match {
      case Some(v) => v
      case _ => 0
    }

    val b = max match {
      case Some(v) => v
      case _ => "\u221E"
    }

    s"[$a, $b]"
  }

  override val description = s"Worker should work for $workingRange but worked $value instead"
}
