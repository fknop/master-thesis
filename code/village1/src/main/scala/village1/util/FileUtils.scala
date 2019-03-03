package village1.util

import java.io.PrintWriter
import java.nio.file.Path

import scala.io.Source

object FileUtils {

  /**
    * Read text file and return content as a string
    * @param path the path of the file
    * @return the content of the file
    */
  def readFile (path: String): String = {
    val file = Source.fromFile(path)
    val content = file.getLines().mkString("\n")
    file.close()
    content
  }

  /**
    * Write file with non existing folder recursively
    * @param path the path
    * @param content the content
    */
  def writeFile (path: String, content: String): Unit = {
    Path.of(path).toFile.getParentFile.mkdirs
    new PrintWriter(path) {
      write(content)
      close()
    }
  }
}


