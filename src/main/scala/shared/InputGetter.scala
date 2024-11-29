package shared

import scala.annotation.tailrec
import scala.util.Try

class InputGetter(year: Int) {

  private val sessionFromEnvFile: String = os.read.lines(os.pwd / ".env").head.split("=").last
  private val session = Try(sys.env("SESSIONID")).getOrElse(sessionFromEnvFile)

  def read(name: String) = {
    val target = os.pwd / "src" / "main" / "resources" / name
    os.read.lines(target)
  }

  @tailrec
  final def get(day: Int): Seq[String] = {
    val target = os.pwd / "src" / "main" / "resources" / f"day$day%02d.txt"
    if (os.exists(target))
      os.read.lines(target)
    else {
      println("Downloading day " + day)
      try {
        os.write(target, requests.get.stream(s"https://adventofcode.com/$year/day/$day/input", check = true, headers = Map("Cookie" -> s"session=$session", "User-Agent" -> "https://github.com/targeter")))
      } finally {
        if (os.size(target) == 0) os.remove(target)
      }

      get(day)
    }

  }

}
