package village1.benchmark.api

import scopt.{OParser, OParserBuilder}
import village1.util.cli.Validations.{allPositive, positive, positiveOrZero}

case class BenchmarkArgs(
  override val solutionLimit: Int = Int.MaxValue,
  override val timeLimit: Int = 20,
  override val repeat: Int = 1,
  override val dryRun: Int = 1,
  override val noKeep: Boolean = false,
  override val T: Array[Int] = Array(5),
  override val D: Array[Int] = Array(30, 50),
  override val W: Array[Int] = Array(100, 200, 300),
  override val seed: Long = -1L,
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
  * --seed set the seed, otherwise it will be random
  */
class CommandLineBenchmark extends App {
  def parseArgs(defaultOptions: BenchmarkArgs = BenchmarkArgs()): BenchmarkArgs = {
    implicit val builder: OParserBuilder[BenchmarkArgs] = OParser.builder[BenchmarkArgs]

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
          .validate(x => allPositive(x, "T must only have positive values")),

        opt[Seq[Int]]("D")
          .action((x, c) => c.copy(D = x.toArray))
          .validate(x => allPositive(x, "D must only have positive values")),

      opt[Seq[Int]]("W")
          .action((x, c) => c.copy(W = x.toArray))
          .validate(x => allPositive(x, "W must only have positive values")),

        opt[String]('o', "out")
          .action((x, c) => c.copy(out = x))
          .validate(x => if (x.trim().isEmpty) failure("out cannot be empty") else success),

        opt[Unit]("no-keep")
          .action((_, c) => c.copy(noKeep = true)),

        opt[Long]("seed")
          .action((x, c) => c.copy(seed = x))
          .validate(x => positiveOrZero(x, "seed must be postive or zero"))
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