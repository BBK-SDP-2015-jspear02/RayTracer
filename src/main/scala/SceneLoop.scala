import akka.actor.{Props, Actor}
class SceneLoop(height: Int,width: Int, ss: Int, sinf: Double, cosf: Double, objects: List[Shape], lights: List[Light]) extends Actor{
  val ambient = .2f
  val background = Colour.black
  val eye = Vector.origin

  def receive() = {
    case (y: Int) => xLoop(y)
    case _ => ???
  }

  def xLoop(y:Int):Unit = {
    println("row" + y)
    for (x <- 0 until width) {

      // This loop body can be sequential.
      var colour = Colour.black

      for (dx <- 0 until ss) {
        for (dy <- 0 until ss) {

          // Create a vector to the pixel on the view plane formed when
          // the eye is at the origin and the normal is the Z-axis.
          val dir = Vector(
            (sinf * 2 * ((x + dx.toFloat / ss) / width - .5)).toFloat,
            (sinf * 2 * (height.toFloat / width) * (.5 - (y + dy.toFloat / ss) / height)).toFloat,
            cosf.toFloat).normalized

          val c = trace(Ray(eye, dir)) / (ss * ss)
          colour += c
        }
      }

      if (Vector(colour.r, colour.g, colour.b).norm < 1)
        Trace.darkCount += 1
      if (Vector(colour.r, colour.g, colour.b).norm > 1)
        Trace.lightCount += 1
      //val rowset = Trace.system.actorOf(Props(new Coordinator()), name = "rowset" + y)
     // rowset ! (x,)
      //Now set it
      Coordinator.set(x, y, colour)
    }
  }

  def shadow(ray: Ray, l: Light): Boolean = {
    val distSquared = (l.loc - ray.orig).normSquared
    intersections(ray).foreach {
      case (v, o) =>
        if ((v - ray.orig).normSquared < distSquared)
          return true
    }
    false
  }

  // Compute the color contributed by light l at point v on object o.
  def shade(ray: Ray, l: Light, v: Vector, o: Shape): Colour = {
    val toLight = Ray(v, (l.loc - v).normalized)

    val N = o.normal(v)

    // Diffuse light
    if (shadow(toLight, l) || (N dot toLight.dir) < 0)
      Colour.black
    else {
      // diffuse light
      val diffuse = o.colour * (N dot toLight.dir)

      println("ray " + ray)
      println("diffuse " + diffuse)

      // specular light
      val R = reflected(-toLight.dir, N)
      val len = ray.dir.norm * R.norm

      val specular = if (len > 1e-12) {
        // Want the angle between R and the vector TO the eye

        val cosalpha = -(ray.dir dot R) / len

        val power = if (cosalpha > 1e-12) math.pow(cosalpha, o.shine).toFloat else 0.0f

        if (power > 1e-12) {
          val scale = o.reflect * power
          l.colour * o.specular * scale
        }
        else
          Colour.black
      }
      else
        Colour.black

      println("specular " + specular)

      val color = diffuse + specular

      println("color " + color + " 0x" + color.rgb.toHexString)

      color
    }
  }

  def reflected(v: Vector, N: Vector): Vector = v - (N * 2.0f * (v dot N))

  def intersections(ray: Ray) = objects.flatMap {
    o => o.intersect(ray).map { v => (v, o)}
  }

  def closestIntersection(ray: Ray) = intersections(ray).sortWith {
    case ((v1, o1), (v2, o2)) => {
      val q1 = (v1 - ray.orig).normSquared
      val q2 = (v2 - ray.orig).normSquared
      q1 < q2
    }
  }.headOption

  val maxDepth = 3

  def trace(ray: Ray): Colour = trace(ray, maxDepth)

  private def trace(ray: Ray, depth: Int): Colour = {
    Trace.rayCount += 1

    // Compute the intersections of the ray with every object, sort by
    // distance from the ray's origin and pick the closest to the origin.
    val r = closestIntersection(ray)

    r match {
      case None => {
        // If no intersection, the color is black
        background
      }
      case Some((v, o)) => {
        // Compute the color as the sum of:

        Trace.hitCount += 1

        // The contribution of each point light source.
        var c = lights.foldLeft(Colour.black) {
          case (c, l) => c + shade(ray, l, v, o)
        }

        // The contribution of the ambient light.
        c += o.colour * ambient

        // Return the color.
        c
      }
    }
  }
}
