package village1.modeling.violations

case class WorkerViolation(demand: Int, position: Int, time: Int) extends Violation {
  override val name: String = "Worker Violation"
  override def toString: String = s"Worker for demand $demand at position $position in time $time could not be assigned"
}
