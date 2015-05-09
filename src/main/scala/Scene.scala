import akka.actor.{Actor,Props,ActorSystem}
import akka.dispatch.Futures
import akka.pattern.ask


object Scene {

  import java.io.{FileReader, LineNumberReader}

  import scala.annotation.tailrec
  val pixel = Trace.system.actorOf(Props(new Coordinator()), name = "pixel" )

  def fromFile(file: String) = {
    val in = new LineNumberReader(new FileReader(file))
    val (objects, lights) = readLines(in, Nil, Nil)
    new Scene(objects, lights)
  }

  @tailrec
  private def readLines(in: LineNumberReader, objects: List[Shape], lights: List[Light]): (List[Shape], List[Light]) = {
    val line = in.readLine
    if (line == null) {
      (objects, lights) match {
        case (Nil, _) => throw new RuntimeException("no objects")
        case (_, Nil) => throw new RuntimeException("no lights")
        case (os, ls) => (os.reverse, ls.reverse)
      }
    }
    else {
      val fields = line.replaceAll("#.*", "").trim.split("\\s+").filter(_ != "")
      fields.headOption match {
        case Some("sphere") =>
          val Array(x, y, z, rad, r, g, b, shine) = fields.tail.map(_.toFloat)
          readLines(in, Sphere(Vector(x, y, z), rad, Colour(r, g, b), shine) :: objects, lights)
        case Some("light") =>
          val Array(x, y, z, r, g, b) = fields.tail.map(_.toFloat)
          readLines(in, objects, Light(Vector(x, y, z), Colour(r, g, b)) :: lights)
        case None =>
          // blank line
          readLines(in, objects, lights)
        case Some(x) =>
          throw new RuntimeException("unknown command: " + x)
      }
    }
  }
}

class Scene private(val objects: List[Shape], val lights: List[Light]) {

  private def this(p: (List[Shape], List[Light])) = this(p._1, p._2)


  val angle = 90f // viewing angle
  //val angle = 180f // fisheye

  def traceImage(width: Int, height: Int) {

    val frustum = (.5 * angle * math.Pi / 180).toFloat

    val cosf = math.cos(frustum)
    val sinf = math.sin(frustum)

    // Anti-aliasing parameter -- divide each pixel into sub-pixels and
    // average the results to get smoother images.
    val ss = Trace.AntiAliasingFactor

    // TODO:
    // Create a parallel version of this loop, creating one actor per pixel or per row of
    // pixels.  Each actor should send the Coordinator messages to set the
    // color of a pixel.  The actor need not receive any messages.

    for (y <- 0 until height) {

      //Create actor within this loop. One actor per row. Seperate method?
      val rowActor = Trace.system.actorOf(Props(new SceneLoop(height, width, ss, sinf, cosf, objects, lights)), "row" + y)
      val future = rowActor ! (y)

     // Coordinator.set();
    }
  }

}

