package village1.benchmark

import org.apache.commons.lang.StringUtils
import scopt.OParser

case class BenchmarkArgs(
  override val solutionLimit: Int = Int.MaxValue,
  override val timeLimit: Int = 20,
  override val repeat: Int = 1,
  override val dryRun: Int = 1,
  override val noKeep: Boolean = false,
  override val T: Array[Int] = Array(5),
  override val D: Array[Int] = Array(30, 50),
  override val W: Array[Int] = Array(100, 200, 300),
  out: String = "") extends BenchmarkOptions {
  override def toString: String = super.toString
}

/**
  * Base class for benchmark that can be started from the CLI
  * CLI arguments:
  * -o --out: output file for the benchmark
  * -t --timeLimit: time limit for the solver
  * -s --solutionLimit: solution limit for the solver
  * -r --repeat: number of repeat of the benchmark (without dry runs)
  * -d --dryRun: number of dryRun for the benchmark
  */
class CommandLineBenchmark extends App {
  def parseArgs(defaultOptions: BenchmarkArgs = BenchmarkArgs()): BenchmarkArgs = {
    val builder = OParser.builder[BenchmarkArgs]

    def positive(x: Int, message: String): Either[String, Unit] = {
      import builder._
      if (x > 0) success
      else failure(message)
    }

    def positiveOrZero(x: Int, message: String): Either[String, Unit] = {
      import builder._
      if (x >= 0) success
      else failure(message)
    }

    def validateParams(x: Seq[Int], message: String): Either[String, Unit] = {
      import builder._
      x.find(_ <= 0) match {
        case Some(_) => failure(message)
        case None => success
      }
    }

    val parser = {
      import builder._
      OParser.sequence(
        programName("v1-benchmark"),
        opt[Int]('s', "solutionLimit")
          .action((x, c) => c.copy(solutionLimit = x))
          .validate(x => positive(x, "solutionLimit must be positive")),

        opt[Int]('t', "timeLimit")
          .action((x, c) => c.copy(timeLimit = x))
          .validate(x => positive(x, "timeLimit must be positive")),

        opt[Int]('r', "repeat")
          .action((x, c) => c.copy(repeat = x))
          .validate(x => positive(x, "repeat must be positive")),

        opt[Int]('d', "dryRun")
          .action((x, c) => c.copy(dryRun = x))
          .validate(x => positiveOrZero(x, "dryRun must be positive")),

        opt[Seq[Int]]("T")
          .action((x, c) => c.copy(T = x.toArray))
          .validate(x => validateParams(x, "T must only have positive values")),

        opt[Seq[Int]]("D")
          .action((x, c) => c.copy(D = x.toArray))
          .validate(x => validateParams(x, "D must only have positive values")),

      opt[Seq[Int]]("W")
          .action((x, c) => c.copy(W = x.toArray))
          .validate(x => validateParams(x, "W must only have positive values")),

        opt[String]('o', "out")
          .action((x, c) => c.copy(out = x))
          .validate(x => if (x.trim().isEmpty) failure("out cannot be empty") else success),

        opt[Unit]("no-keep")
          .action((_, c) => c.copy(noKeep = true))
      )
    }

    OParser.parse(parser, args, defaultOptions) match {
      case Some(options) =>
        if (options.out.trim().isEmpty) {
          throw new IllegalArgumentException("out cannot be empty")
        }
        options
      case _ =>
        throw new IllegalArgumentException("Wrong options")
    }
  }
}