package release.docker

import java.nio.file.Path

object Dockerfile {
  def parse(f: Path, envs:Map[String, String]): Unit = {
    // TODO parse an check 'FROM' lines for missing hostnames
    // TODO add skip logic
  }
}
