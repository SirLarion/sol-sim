/**
  *
  *      Solar system simulator for modeling the movements of celestial bodies.
  *      It was made as a course project for course CS-C2120.
  *
  *      Made by Miska Tammenpää
  *
  *
  */

package simulator

import interface.GUI
import scala.swing.{SimpleSwingApplication, MainFrame, Label}
import java.awt.{DisplayMode, GraphicsEnvironment, GraphicsDevice, Dimension, Color}
import misc.ephemerides._
import misc.calc._

object SolarSimApp extends SimpleSwingApplication {

  private val fps = 144
  def getFPS = this.fps
  val frameDelay  = (1000.0/fps).toInt

  val ge = GraphicsEnvironment.getLocalGraphicsEnvironment
  val display = ge.getDefaultScreenDevice.getDisplayMode

  val width = display.getWidth().toInt
  val height = display.getHeight().toInt

  val top = new MainFrame() {
    background = Color.BLACK
    peer.setOpacity(1.0f)
    peer.setUndecorated(true)
    preferredSize = new Dimension(width, height)
    contents = GUI.view
    centerOnScreen()
    title = "Solar System Simulator"
    visible = true
    resizable = false
  }

  private def loadFallbackSol() = {
    Simulation.name = Some("SOL-MCRF")

    //The solar system at 18.04.2020 00:00:00, with data
    //gotten from JPL:s HORIZONS web-interface; https://ssd.jpl.nasa.gov/horizons.cgi
    //The data is calculated with ICRF (epoch J2000) as the reference frame,
    //the Sun in the origin and the ecliptic as the XY-plane

    //Normally the data is loaded from a file, but if somewhy
    //the file is missing or otherwise unable to be loaded, the system
    //falls back to this method and saves the file again

    def initPath(p: Pos) = scala.collection.mutable.Queue[Pos](p)

    Simulation.allBodies = Simulation.allBodies :+ Body(NULLV,   NULLV, SUN_ATTR(0),   SUN_ATTR(1),   SUN_ATTR(2),   "Sun",     Pos.O,        initPath(Pos.O))
    Simulation.allBodies = Simulation.allBodies :+ Body(MERC_V,  NULLV, MERC_ATTR(0),  MERC_ATTR(1),  MERC_ATTR(2),  "Mercury", Pos(MERC_R),  initPath(Pos(MERC_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(VENUS_V, NULLV, VENUS_ATTR(0), VENUS_ATTR(1), VENUS_ATTR(2), "Venus",   Pos(VENUS_R), initPath(Pos(VENUS_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(EARTH_V, NULLV, EARTH_ATTR(0), EARTH_ATTR(1), EARTH_ATTR(2), "Earth",   Pos(EARTH_R), initPath(Pos(EARTH_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(MOON_V,  NULLV, MOON_ATTR(0),  MOON_ATTR(1),  MOON_ATTR(2),  "Moon",    Pos(MOON_R),  initPath(Pos(MOON_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(MARS_V,  NULLV, MARS_ATTR(0),  MARS_ATTR(1),  MARS_ATTR(2),  "Mars",    Pos(MARS_R),  initPath(Pos(MARS_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(JPTR_V,  NULLV, JPTR_ATTR(0),  JPTR_ATTR(1),  JPTR_ATTR(2),  "Jupiter", Pos(JPTR_R),  initPath(Pos(JPTR_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(STRN_V,  NULLV, STRN_ATTR(0),  STRN_ATTR(1),  STRN_ATTR(2),  "Saturn",  Pos(STRN_R),  initPath(Pos(STRN_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(URNS_V,  NULLV, URNS_ATTR(0),  URNS_ATTR(1),  URNS_ATTR(2),  "Uranus",  Pos(URNS_R),  initPath(Pos(URNS_R)))
    Simulation.allBodies = Simulation.allBodies :+ Body(NPTN_V,  NULLV, NPTN_ATTR(0),  NPTN_ATTR(1),  NPTN_ATTR(2),  "Neptune", Pos(NPTN_R),  initPath(Pos(NPTN_R)))

    IOHandler.saveSimData()
    Simulation.name = None //Set back to None so the user doesn't write over the default file
  }
  //Use fallback if loadSimData returns an error message
  if(IOHandler.loadSimData("SOL-MCRF").isDefined) loadFallbackSol()

  GUI.start(frameDelay)
}
