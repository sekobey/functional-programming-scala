
import common._

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

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    def computeAveragePixel() = {

      val mask = Array((-1, -1), (0, -1), (1, -1),
                       (-1, 0),           (1, 0),
                       (-1, 1),  (0, 1),  (1, 1))

      var i = 1
      var j = 0
      val max = (src.height - 1) * src.width + (src.width - 1)
      println(s"max:$max")
      var (newRed, newGreen, newBlue, newAlpha) = (0, 0, 0, 0)
      var neighborCount = 0
      while (i <= radius) {
        while (j < mask.length) {
          val maskedX = mask(j)._1*i + x
          val maskedY = mask(j)._2*i + y
          val pos = maskedY * src.width + maskedX
          if (pos == clamp(pos, 0, max) && maskedX < src.width && maskedY < src.height) {
            println(s"($x,$y) pos:($maskedX,$maskedY) -> convertedPos: $pos")
            newRed += red(src(maskedX, maskedY))
            newGreen += green(src(maskedX, maskedY))
            newBlue += blue(src(maskedX, maskedY))
            newAlpha += alpha(src(maskedX, maskedY))
            neighborCount += 1
          }
          j += 1
        }
        i += 1
        j = 0
      }

      rgba(newRed / neighborCount, newGreen / neighborCount, newBlue / neighborCount, newAlpha / neighborCount)
    }

    if (radius <= 0) src(x,y)
    else computeAveragePixel()
  }

}
