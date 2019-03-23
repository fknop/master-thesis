package village1.modeling

import org.scalatest.{FunSpec, Matchers}

abstract class CommonSpec extends FunSpec with Matchers {
  protected def checkValid (solution: Solution): Unit = {
    solution.valid should matchPattern { case ValidSolution => }
  }

  protected def checkNotPartial(solution: Solution): Unit = {
    solution.partial should equal(false)
  }

  protected def checkPartial(solution: Solution): Unit = {
    solution.partial should equal(true)
  }
}
