package village1.format.json

import village1.modeling.Solution

object JsonSerializer {

  def serialize (solution: Solution): String => Unit = {

    val value = "test"

    path: String => {
      println(s"writeTo: $path $value")
    }
  }
}
