import akka.actor.ActorSystem

object Trace {

  val AntiAliasingFactor = 4
  val Width = 800
  val Height = 600

  var rayCount = 0
  var hitCount = 0
  var lightCount = 0
  var darkCount = 0
  var system = ActorSystem("rayTracer")
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("usage: scala Trace input.dat output.png")
      System.exit(-1)
    }

    val (infile, outfile) = (args(0), args(1))
    val scene = Scene.fromFile(infile)

    render(scene, outfile, Width, Height)

  }

  def render(scene: Scene, outfile: String, width: Int, height: Int) = {
    val image = new Image(width, height)
    import akka.actor.{Props}
    //create the actor system

    //Initialise the co-ordinator as an actor. Init takes place on initialization
    val coord = system.actorOf(Props(new Coordinator()), name = "coord")
    Coordinator.init(image,outfile)
    scene.traceImage(width, height)

  }
}