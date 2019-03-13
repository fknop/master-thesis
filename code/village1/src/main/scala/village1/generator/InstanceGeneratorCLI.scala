package village1.generator

import scopt.OParser
import village1.json.JsonSerializer
import village1.util.Utilities


object InstanceGeneratorCLI extends App {

  var out: Option[String] = None

  def parseArgs(): InstanceOptions = {
    val builder = OParser.builder[InstanceOptions]

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

    def probability(x: Double, message: String): Either[String, Unit] = {
      import builder._
      if (x >= 0.0 && x <= 1.0) success
      else failure(message)
    }

    val parser = {
      import builder._
      OParser.sequence(
        programName("v1-generator"),
        opt[Int]('t', "T")
          .action((x, c) => c.copy(t = x))
          .validate(x => positive(x, "T must be positive")),

        opt[Int]('c', "clients")
          .action((x, c) => c.copy(clients = x))
          .validate(x => positive(x, "clients must be positive")),

        opt[Int]('d', "demands")
          .action((x, c) => c.copy(demands = x))
          .validate(x => positive(x, "demands must be positive")),

        opt[Int]('w', "workers")
          .action((x, c) => c.copy(workers = x))
          .validate(x => positive(x, "workers must be positive")),

        opt[Int]('s', "skills")
          .action((x, c) => c.copy(skills = x))
          .validate(x => positive(x, "skills must be positive")),

        opt[Int]('l', "locations")
          .action((x, c) => c.copy(locations = x))
          .validate(x => positive(x, "locations must be positive")),

        opt[Int]('m', "machines")
          .action((x, c) => c.copy(machines = x))
          .validate(x => positive(x, "machines must be positive")),

        opt[Double]( "s-prob")
          .action((x, c) => c.copy(probabilities = c.probabilities.updated("skill", x)))
          .validate(x => probability(x, "s-prob must be between 0.0 and 1.0")),

        opt[Double]( "p-prob")
          .action((x, c) => c.copy(probabilities = c.probabilities.updated("period", x)))
          .validate(x => probability(x, "p-prob must be between 0.0 and 1.0")),

        opt[String]( 'o', "out")
          .action((x, c) => {
            out = Some(x)
            c
          })
          .validate(x => if (x.trim().isEmpty) failure("out cannot be empty") else success),
      )
    }

    val default = InstanceOptions(
      t = 5,
      clients = 10,
      demands = 50,
      workers = 300,
      skills = 20,
      locations = 20,
      machines = 30
    )

    OParser.parse(parser, args, default) match {
      case Some(options) =>
        options
      case _ =>
        throw new IllegalArgumentException("Wrong options")
    }
  }

  val problem = InstanceGenerator.generate(parseArgs())

  val output = out match {
    case Some(path) => path
    case None =>
      val t = problem.T
      val d = problem.demands.length
      val w = problem.workers.length
      val fileName = s"t${t}d${d}w${w}-${Utilities.rand(0, 1000)}.json"
      val folder = "data/instances/generated/"
      s"$folder$fileName"
  }


  JsonSerializer.serialize(problem)(output)
  println(s"Generated instance written to $output")
}


