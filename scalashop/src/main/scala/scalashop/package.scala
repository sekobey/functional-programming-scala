

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  def taskSplits(max: Int, numTasks: Int) = max match {
    case x if x <= numTasks => Vector((0, max))
    case _ =>
      val range1 = 0 to (max - numTasks) by numTasks
      val range2 = numTasks to max by numTasks

      val splits = range1 zip range2
      if (splits.last._2 < max) splits :+ (splits.last._2, max)
      else splits
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    def average(ints: Seq[Int]): Int = {
      ints.sum / ints.size
    }

    val newChannels = for {
      xNew <- clamp(x - radius, 0, src.width - 1) to clamp(x + radius, 0, src.width - 1)
      yNew <- clamp(y - radius, 0, src.height - 1) to clamp(y + radius, 0, src.height - 1)
    } yield (red(src(xNew, yNew)), green(src(xNew, yNew)), blue(src(xNew, yNew)), alpha(src(xNew, yNew)))

    rgba(
      average(newChannels.map(_._1)),
      average(newChannels.map(_._2)),
      average(newChannels.map(_._3)),
      average(newChannels.map(_._4))
    )
  }

}
