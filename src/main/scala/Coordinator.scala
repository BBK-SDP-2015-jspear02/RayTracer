import akka.actor.{Actor}

// TODO
//
// Make this an actor and write a message handler for at least the
// set method.
//
case class Pixel(x: Int, y :Int, c: Colour)
case class Update(item : String)

object Coordinator {
  def init(im: Image, of: String) = {
    image = im
    outfile = of
    waiting = im.width * im.height
  }

  // Number of pixels we're waiting for to be set.
  var waiting = 0
  var outfile: String = null
  var image: Image = null

  // TODO: make set a message
  def set(x: Int, y: Int, c: Colour) = {
    image(x, y) = c
    waiting -= 1
    if (waiting == 0) {
      Trace.system.shutdown()
      print
    } else {
      println("x: " + x + " y: " + y + " waiting: " + waiting)
    }
  }

  def update(item:String) = {
    item match {
      case "light" => Trace.lightCount += 1
      case "dark" => Trace.darkCount +=1
      case "ray" => Trace.rayCount += 1
      case "hit" => Trace.hitCount += 1
      case _ => ???
    }
  }

  def print = {
    assert(waiting == 0)
    image.print(outfile)
    println("rays cast " + Trace.rayCount)
    println("rays hit " + Trace.hitCount)
    println("light " + Trace.lightCount)
    println("dark " + Trace.darkCount)
  }
}

class Coordinator() extends Actor{
  //Initialise when the actor is created

  def receive = {
    case Pixel(x: Int, y: Int, colour: Colour) => Coordinator.set(x,y,colour)
    case Update(item : String) => Coordinator.update(item)
    case _ => throw new Error("What is this?!")
  }
}
