package misc
import scala.math.{sqrt, pow, Pi}
import simulator.Pos
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, RenderingHints, Color}
import scala.concurrent._
import ExecutionContext.Implicits.global

package object constants {

  //Mathematical constants
  //-------------------------------------------------------------------------------------//
  val kilo = pow(10, 3)
  val mega = pow(10, 6)
  val giga = pow(10, 9)
  val peta = pow(10, 15)
  val G = 6.6743*pow(10, -11)
  val AU = 149.5979*giga
  val RAD = Pi/180
  val c = 2.99792458*pow(10, 8)
  //-------------------------------------------------------------------------------------//

  //Simulator constants
  //-------------------------------------------------------------------------------------//
  val MAX_ORBIT_LENGTH = 2000
  val MAX_POS_GAP = 10000
  val MIN_AIM_SIZE = 25
  val MAX_BODY_AMOUNT = 20

  val BODY_NAME = "Body"
  val DEFAULT_ZOOM = 4.0*giga
  val ZOOM_DELTA = 0.1388
  val DEFAULT_BSCALE = 50.0
  val ORIGIN = (0.0, 0.0, 0.0)
  val BASE_ANGLES = (0.0, 0.0, 0.0)
  //-------------------------------------------------------------------------------------//
}

//Calculation methods
package object calc {

  //-------------------------------------------------------------------------------------//
  def translate(xoff: Int, yoff: Int, pos: Vector[Int]) = {
    Vector(pos(0) + xoff, pos(1) + yoff, pos(2))
  }
  def translate(xoff: Int, yoff: Int, pos: Future[Vector[Int]]) = {
    pos.map(p => Vector(p(0) + xoff, p(1) + yoff, p(2)))
  }
  //-------------------------------------------------------------------------------------//


  //Vector calculation
  //-------------------------------------------------------------------------------------//
  val NULLV = Vector(0.0, 0.0, 0.0)

  //Vector sum
  def vsum(vectors: Vector[Double]*) = vectors.fold(NULLV)(_.zip(_).map(s => s._1+s._2))
  def vsum(vectors: Vector[Vector[Double]]) = vectors.fold(NULLV)(_.zip(_).map(s => s._1+s._2))
  //Length of  vector
  def vlen(s: Vector[Double]) = sqrt(pow(s(0), 2) + pow(s(1), 2) + pow(s(2), 2))

  //Scalar multiplication
  def smult(a: Double, s: Vector[Double]) = s.map(a * _)
  //Helper method for using vsum to remove a vector from another
  def neg(s: Vector[Double]) = smult(-1, s)

  //Dot product
  def dprod(s: Vector[Double], t: Vector[Double]) = s.zip(t).map(x => x._1*x._2).fold(0.0)(_+_)

  //Determinant (2x2)
  def det(s: Vector[Double], t: Vector[Double]) = s(0)*t(1) - s(1)*t(0)

  //Cross product
  def cprod(s: Vector[Double], t: Vector[Double]) = {
    Vector(s(1)*t(2) - s(2)*t(1),
           s(2)*t(0) - s(0)*t(2),
           s(0)*t(1) - s(1)*t(0))
  }
  //-------------------------------------------------------------------------------------//
}

package object helpers {
  object Scale {
    def apply(rate: Double) = new Scale(rate)
  }
  class Scale(rate: Double){
    private var r = this.rate
    def get = this.r
    def set(rate: Double) = r = rate

    def apply(value: Double) = (value/r).toInt
    def apply(value: Vector[Double]) = value.map(x => (x/r).toInt)
    def apply(value: Future[Vector[Double]]): Future[Vector[Int]] = value.map(pos => this(pos))
    def rev(value: Vector[Int]) = value.map(x => (x*r))
  }
}

/** Helpers for java.awt Image handling, courtesy of Aalto University programming course
  * CS-C2100 and Otso Seppälä (plus a color pallette by myself) */
package object ImageExtensions {

  val BLACK = Color.BLACK
  val DEEP_SPACE = new Color(6, 0, 20)
  val SPACE_MARINE = new Color(0, 0, 15)
  val CARBON = new Color(20, 20, 20)
  val BLUE_CARBON = new Color(10, 20, 30)
  val DARK_BLUE_CARBON = new Color(10, 15, 26)
  val ERROR_RED = new Color(211, 57, 82)
  val INVISIBLE = new Color(255, 255, 255, 0)
  val OFF_WHITE = new Color(190, 190, 210)

   /**
    * This allows easier creation of BufferedImages)
    *
     {{{
        val example = emptyImage(100,100)
     }}}
     @param width the width of the image
     @param height the height of the image
     @return a new, completely transparent image (All pixels are initially Color(0,0,0,0))
     */

  def emptyImage(width: Int, height: Int) = {
    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = img.graphics
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, width, height)
    img
  }

  implicit class S1Image(b: BufferedImage) {

    /**
     * This allows easier copying of a BufferedImage
     *
     {{{
        val example = emptyImage(100,100)

        val copy = example.copy
     }}}

     * return a copy of this image
     */
    def copy = {
      val copied = emptyImage(b.getWidth, b.getHeight)
      copied.getGraphics.drawImage(b, 0, 0, null)
      copied
    }

    /**
     * This allows easier access to the Graphics2D object used to draw on painting surfaces (e.g. BufferedImages)
     *
     {{{
        val example = emptyImage(100,100)

        val g = example.graphics
     }}}
     * @return the graphics object used when drawing on images
     */
    def graphics = {
      val g = b.getGraphics.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g
    }
  }
}

package object ephemerides {
  import constants._
  //==========================================================================================================================================================//
  val SUN_ATTR   = Vector(1.989*pow(10, 30), 1409.0, 6.980*pow(10, 8))
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val MERC_V  = Vector(1.910870720391141E+01*kilo, 4.161363988993137E+01*kilo, 1.647521736880085*kilo)
  val MERC_R  = Vector(4.847340564214430E+07*kilo, -3.535430238105840E+07*kilo, -7.335629911992278E+06*kilo)
  val MERC_O  = Vector(2.056480264628064E-01, 5.790898300602781E+07*kilo, 7.003764961829791*RAD,  4.830610289221121E+01*RAD,  2.918297434643691E+01*RAD)
  val MERC_ATTR  = Vector(3.301*pow(10, 23), 5427.0, 2439.7*kilo, 69.817*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val VENUS_V = Vector(-1.205250971753582*kilo, -3.516040935411390E+01*kilo, -4.129097640999007E-01*kilo)
  val VENUS_R = Vector(-1.074813568729311E+08*kilo, 3.082833742991918E+06*kilo, 6.244702840989727E06*kilo)
  val VENUS_O = Vector(6.744373110935190E-03, 1.082081766978245E+08*kilo, 3.394563554370145*RAD, 7.662441321970006E+0*RAD, 7.662441321970006E+01*RAD)
  val VENUS_ATTR = Vector(4.867*pow(10, 24), 5243.0, 6051.8*kilo, 107.477*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val EARTH_V = Vector(1.356202685572714E+01*kilo, -2.638770024880040E+01*kilo, 1.707266374728178E-03*kilo)
  val EARTH_R = Vector(-1.324505808331120E+08*kilo, -7.086538974507442E+07*kilo, 3.976044113278389E+03*kilo)
  val EARTH_O = Vector(1.693233912954931E-02, 1.496684062856282E+08*kilo, 3.606742080609680E-03*RAD, 1.832836379944875E+02*RAD, 2.814238778697811E+02*RAD)
  val EARTH_ATTR = Vector(5.974*pow(10, 24), 5517.0, 6378.1*kilo, 147.001*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val MOON_V  = Vector(1.403507311459549E+01*kilo, -2.553856437927035E+01*kilo, -4.181163278570210E-02*kilo)
  val MOON_R  = Vector(-1.320943225693448E+08*kilo, -7.105113702931114E+07*kilo, -2.721678549306095E+04*kilo)
  val MOON_O  = Vector(4.123314910955279E-02, 1.441888585742764E+08*kilo, 8.295913385620494E-02*RAD, 2.107558872364711E+01*RAD, 3.550856095696951E+02*RAD)
  val MOON_ATTR  = Vector(7.349*pow(10, 22), 3343.7, 1737.4*kilo, 146.638*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val MARS_V  = Vector(2.514457846188029E+01*kilo, 2.229204849420964*kilo, -5.701793741432185E-01*kilo)
  val MARS_R  = Vector(1.307171528951938E+06*kilo, -2.175570539643828E+08*kilo, -4.590664474067464E+06*kilo)
  val MARS_O  = Vector(9.347939664705711E-02, 2.279551453704509E+08*kilo, 1.847939027662140*RAD, 4.950025137289147E+01*RAD, 2.866120440784564E+02*RAD)
  val MARS_ATTR  = Vector(6.417*pow(10, 23), 3933.5, 3389.5*kilo, 206.700*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val JPTR_V  = Vector(1.249152163181915E+01*kilo, 3.940488996000793*kilo, -2.958956511889403E-01*kilo)
  val JPTR_R  = Vector(1.972183000678289E+08*kilo, -7.506336986352351E+08*kilo, -1.294777234412849E+06*kilo)
  val JPTR_O  = Vector(4.865991240888744E-02, 7.783528864356767E+08*kilo, 1.303802503926140*RAD, 1.005173508210917E+02*RAD, 2.735988660397134E+02*RAD)
  val JPTR_ATTR  = Vector(1.898*pow(10, 27), 1326.0,  69911*kilo, 740.520*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val STRN_V  = Vector(8.198002868599245*kilo, 4.136563433573230*kilo, 3.981742932810628E-01*kilo)
  val STRN_R  = Vector(6.456120960936913E+08*kilo, -1.353235038553703E+09*kilo, -2.169803115013957E+06*kilo)
  val STRN_O = Vector(5.089471777675440E-02, 1.433593764064728E+09*kilo, 2.486741654227463*RAD, 1.135956627597747E+02*RAD, 3.374765764029820E+02*RAD)
  val STRN_ATTR  = Vector(5.683*pow(10, 26),  0.687,  58232*kilo, 1352.550*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val URNS_V  = Vector(-4.063111248919900*kilo, 5.179640515856461*kilo, 7.180693796877735E-02*kilo)
  val URNS_R  = Vector(2.389921928660285E+09*kilo, 1.750988134244362E+09*kilo, -2.445261529390216E+07*kilo)
  val URNS_O  = Vector(4.621009041927530E-02, 2.869799487945364E+09*kilo, 7.715895682608412E-01*RAD, 7.402350910695260E+01*RAD, 9.860112928202868E+01*RAD)
  val URNS_ATTR  = Vector(8.868*pow(10, 25),  1.270,  25362*kilo, 3008.000*giga)
  //----------------------------------------------------------------------------------------------------------------------------------------------------------//
  val NPTN_V  = Vector(1.072530820252146*kilo, 5.361988909411384*kilo, -1.343954425868430E-01*kilo)
  val NPTN_R  = Vector(4.384955979306283E+09*kilo, -9.025567740956695E+08*kilo, -8.248229914926618E+07*kilo)
  val NPTN_O  = Vector(1.071094061445520E-02, 4.520163649474684E+09*kilo, 1.764101016042019*RAD, 1.316280495444178E+02*RAD, 2.456070971119454E+02*RAD)
  val NPTN_ATTR  = Vector(1.024*pow(10, 26),  1.638,  24622*kilo, 4460.000*giga)
  //==========================================================================================================================================================//
}
