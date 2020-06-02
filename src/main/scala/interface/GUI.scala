package interface

import simulator._
import misc.ImageExtensions._
import misc.helpers._
import misc.constants._
import misc.calc._

import scala.collection.immutable.Map
import scala.math._
import scala.util.{Success, Failure}
import scala.collection.mutable.Queue
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

import scala.swing.{Component, Reactor, BorderPanel}
import scala.swing.event._
import scala.swing.Swing
import scala.swing.BorderPanel.Position._

import java.awt.{Point, RenderingHints, Font, Graphics2D, BasicStroke, Polygon, RadialGradientPaint}
import java.awt.MultipleGradientPaint.CycleMethod
import java.awt.Color
import java.awt.geom.{AffineTransform, Point2D}
import java.awt.image.BufferedImage
import java.util.{Timer, TimerTask}
import javax.swing.SwingUtilities
import java.io.{File, FileReader, IOException}

/** An enumeration of the current state of the GUI. Used for differentiating between
  * modes of operating between features */
private[interface] object State extends Enumeration {
  type State = Value
  val DEFAULT, ADDING, AIMING, SHUTDOWN = Value
}

/** The GUI is not a UI element in itself but the backbone for the UI elements.
  * It runs the core timer of the animation and the communication between the UI elements,
  * the simulation and the IO handler. It is also the primary event listener.
  */
object GUI extends Component with Reactor {
  import State._

  //Initialization
  //-------------------------------------------------------------------------------------//
  try {
    //Import the fonts used in the program from the resources folder
    val oswald     = Font.createFont(Font.TRUETYPE_FONT, new File("resources/fonts/Oswald-Regular.ttf"))
    val ptsans     = Font.createFont(Font.TRUETYPE_FONT, new File("resources/fonts/PTSans-Regular.ttf"))
    val montserrat = Font.createFont(Font.TRUETYPE_FONT, new File("resources/fonts/Montserrat-Bold.ttf"))

    SolarSimApp.ge.registerFont(oswald)
    SolarSimApp.ge.registerFont(montserrat)
    SolarSimApp.ge.registerFont(ptsans)
  }
  catch {
    case ioe: IOException => println("Couldn't load file")
    case t: Throwable => println("Unexpected error")
  }

  //The fonts used in UI elements
  val fonts = Vector("Oswald", "Montserrat", "PTSans")

  val h = SolarSimApp.height //------------------ The height of the window
  val w = SolarSimApp.width //------------------- The width of the window

  private var bgImage = emptyImage(w, h) //------ The background image of the GUI (generated after Initialization)

  private var state = DEFAULT //----------------- State variable for defining the GUIs current mode of operation

  //The factors by which distances and body sizes are scaled respectively
  private var scaleOff = 0.33 //----------------- A percentage of fullScale. Defines the value of bodyScale
  private var scaleDirty = true //--------------- State variable that tells whether the GUI has been scaled or offsetted
  var fullScale = Scale(DEFAULT_ZOOM) //--------- The rate by which distances are scaled
  var bodyScale = Scale(DEFAULT_ZOOM*scaleOff) // The rate by which the radii of bodies are scaled
  var minBodyRadius = { //----------------------- The current minimum displayed body radius. Changes based on fullScale's value
    if(fullScale.get < 5*pow(10, 6))
      3;
    else if(fullScale.get < pow(10, 9))
      2;
    else
      1;
  }

  /** Containers for values gotten from Simlation. Containers are
    * separate for the sake of clarity and efficiency. */
  private[interface] var bodyData = Simulation.getBodies()
  private[interface] var orbitData = Simulation.getOrbits(true).map(o => o.map(p => translate(w/2, h/2, fullScale(p))))
  private[interface] var posData = Simulation.nextState.map(b => b.map(pos => fullScale(pos())))

  // Helper containers for handling the simulation data
  private[interface] var rScaled = bodyData.map(b => max(2, bodyScale(b.r)))
  private[interface] var translated = posData.zip(rScaled).map{case (p, r) => (translate(w/2 - r, h/2 - r, p), r)}

  //Container for the state of the position calculations (true if finished, false if not)
  private[interface] var awaitRefresh = Array.fill(bodyData.size)(false)

  //Containers for storing the inputs gotten from text fields
  private[interface] var inputInts:    Option[Vector[Int]]    = None //- The coordinates of the mouse when clicking to add a body
  private[interface] var inputDoubles: Option[Vector[Double]] = None //- The attribute values given when adding a body
  private[interface] var inputStrings: Option[Vector[String]] = None //- The name given when adding a body

  //Containers for storing mouse actions
  private var mTrack = Vector(0, 0, 0) //------------------------------- Mouse position
  private var mTrans = fullScale.rev(translate(-w/2, -h/2, mTrack)) //-- Mouse position translated and scaled to 'reality'
  private var aimVector  = Vector(0.0, 0.0, 0.0) //--------------------- Mouse position converted into velocity unit vector when adding a body
  private var mOff = (0, 0) //------------------------------------------ The offset created by the mouse
  private var mDrag: Option[(Int, Int)] = None //-----------------------

  private var inFocus: Option[Body] = None //--------------------------- The current body in focus
  private var xRef = w/2 //--------------------------------------------- The current reference point for offsets on the x axis
  private var yRef = h/2 //--------------------------------------------- The current reference point for offsets on the y axis
  private var xOff = xRef +mOff._1 //----------------------------------- The current graphical offset on the x axis
  private var yOff = yRef +mOff._2 //----------------------------------- The current graphical offset on the y axis

  /** Containers for queueing destructive actions requested by the user.
    * This way the actions can be safely taken once calculation in separate
    * threads has been completed. In practice these queues are either empty
    * or have a single value in them */
  private[interface] val loadQueue = Queue[String]() //----------------- Queue for loading simulation files
  private val deleteQueue = Queue[String]() //-------------------------- Queue for deleting bodies
  private val scaleQueue  = Queue[Int]() //----------------------------- Queue for changing the rate by which distances are scaled

  private var lastLoaded = "SOL-MCRF" //-------------------------------- Container for storing the name of the most recently loaded file
  //-------------------------------------------------------------------------------------//



  //Running the GUI
  //-------------------------------------------------------------------------------------//
  /** Progresses the state of the simulation and handles changes to the graphical elements.
    * Updates only the local variables which are needed for drawing the bodies and their
    * orbits. */
  private def refresh() = {
    if(!loadQueue.isEmpty) sendLoadQuery(loadQueue.dequeue)  //Check the queues and react accordingly
    if(!deleteQueue.isEmpty) dequeueDelete()
    if(!scaleQueue.isEmpty){
      fullScale.set(fScaleConvert(scaleQueue.dequeue))
      scaleDirty = true
    }
    if(UI.lSlider.adjusting){      //Update the maximum length of orbits
      Simulation.setOrbitLength(UI.lSlider.value)
    }
    if(UI.tSlider.adjusting){      //Update the timescale of the simulation
      Simulation.setTimescale(timeConvert())
      UI.Timer.update()
    }
    bodyScale.set(bScaleConvert()) //Update bodyscale

    if(UI.infoPane.visible){       //Update the values in the infopane and change the body in focus
      updateData()
      UI.infoPane.checkTabs()
      checkFocus()
    }
    inFocus.foreach(b => {         //If a body is in focus, offset the graphics to indicate that
      val focus = translate(w/2, h/2, fullScale(b.position()).map(_*(-1)))
      xRef = focus(0)
      yRef = focus(1)
      scaleDirty = true
    })
    xOff = xRef +mOff._1           //Apply offsets
    yOff = yRef +mOff._2
    UI.scaleDisplay.update()       //Update the scale display

    //If there's a message being displayed by toPrint, progress the animation
    if(UI.toPrint.visible) UI.toPrint.changeState()
    if(scaleDirty){  //If the scale or offset has changed, update the whole orbitData container
      orbitData = Simulation.getOrbits(true).map(o => o.map(p => translate(xOff, yOff, fullScale(p))))
      minBodyRadius = { //And body radius
        if(fullScale.get < 5*pow(10, 6))
        3;
        else if(fullScale.get < pow(10, 9))
        2;
        else
        1;
      }
      scaleDirty = false
    }
    else {   //Otherwise updating only the last orbitData points is necessary
      orbitData = orbitData.zip(Simulation.getOrbits(false)
      .map(o => o.map(p => translate(xOff, yOff, fullScale(p)))))
      .map(a => (a._2++a._1).take(Simulation.getOrbitLength()))
    }
    rScaled = bodyData.map(b => max(minBodyRadius, bodyScale(b.r)))


    //Initialize the calculation of the next body positions
    awaitRefresh = Array.fill(bodyData.size)(false)
    posData      = Simulation.nextState().map(b => b.map(pos => fullScale(pos())))
    translated   = posData.zip(rScaled).map{case (p, r) => (translate(xOff - r, yOff - r, p), r)}
  }

  /** Updates the local values of bodies. The values not updated by refresh() can be fetched less
    * frequently so having a separate method improves efficiency.*/
  private def updateData() = {
    bodyData = Simulation.getBodies()
  }

  //Resets the initial values of the GUI and some UI elements.
  private def resetGUI() = {
    inputInts = None
    inputDoubles = None
    inputStrings = None
    mTrack = Vector(0, 0, 0)
    mTrans = Vector(0.0, 0.0, 0.0)
    aimVector  = Vector(0.0, 0.0, 0.0)
    mOff = (0, 0)
    mDrag = None

    xRef = w/2
    yRef = h/2
    xOff = xRef
    yOff = yRef

    UI.tSlider.value = 1
    UI.rSlider.value = 33
    UI.lSlider.value = 750
    UI.Timer.update()
    UI.scaleDisplay.update()
    UI.addBodyInterface.clear()
    UI.infoPane.clearTabs()
    loadQueue.clear()
    deleteQueue.clear()
    scaleQueue.clear()

    fullScale.set(DEFAULT_ZOOM)
    scaleDirty = true

    state = DEFAULT
    updateData()
  }
  //-------------------------------------------------------------------------------------//



  //Communication with the user and the UI elements
  //-------------------------------------------------------------------------------------//

  //Checks if an interface is currently opened
  private def interfaceOpen() = {
    UI.infoPane.visible || UI.addBodyInterface.visible ||
    UI.menuInterface.visible || UI.IOInterface.visible ||
    UI.overwriteSimInterface.visible
  }

  //Helper method for safely hiding all interfaces
  private def clearScreen() = {
    UI.infoPane.close()
    UI.addBodyInterface.close()
    UI.menuInterface.close()
    UI.IOInterface.close()
    UI.overwriteSimInterface.close()
    UI.hotbar.visible = false
  }

  //Starts the process of adding a body with the values given by the user into addBodyInterface
  private[interface] def startAddBody(): Unit = {
    if(inputDoubles.isDefined && inputStrings.isDefined && inputDoubles.get.forall(_ >= 0)){
      clearScreen()
      printMsg("Left-click to place body. Scroll wheel changes Z")
      state = ADDING //Indicate that the user is choosing the coordinates for a body
    }
    else {
      printMsg("Adding failed. Values must be positive.")
    }
  }

  //The second step when adding a body
  private def startAim(): Unit = {
    printMsg("Left-click to confirm velocity vector")
    inFocus = None
    state = AIMING //Indicate that the user is aiming the body's velocity vector
  }

  //Finish adding a body and send its data to the Simulation
  private def finishAddBody(completed: Boolean): Unit = {
    if(completed){ //Check if the process was canceled
      UI.hotbar.visible = true
      val attr = inputDoubles.get
      val v = smult(attr(0)*kilo, aimVector)
      val r = mTrans
      val name = inputStrings.get(0)
      val report = Simulation.addBody(v, r, attr.drop(1), name, true) //Attempt to add the body into the simulation
      printMsg(report) //Print the result on the screen
      updateData()
      UI.infoPane.checkTabs()
    }
    else {
      UI.hotbar.visible = true
      printMsg("Adding cancelled")
    }
    //Reset the inputs and the state of the GUI
    inputInts = None; inputDoubles = None; inputStrings = None
    state = DEFAULT
  }

  //Helpers for safely deleting bodies
  private[interface] def enqueueDelete(body: String) = deleteQueue.enqueue(body)
  private[interface] def dequeueDelete() = openDelBody(deleteQueue.dequeue())

  /** Tries to delete a body. In theory, this function should always
    * be successful since the deletion is initiated from the constantly refreshed
    * info page of the body to be deleted */
  private def openDelBody(body: String) = {
    val page = UI.infoPane.pages.find(_.title == body)

    if(page.isDefined) UI.infoPane.delTab(page.get)              //First remove the info page corresponding to the body
    if(Simulation.removeBody(body)) printMsg(s"Deleted ${body}") //Then remove the body itself
    else printMsg(s"Deleting ${body} failed")

    updateData()
    UI.infoPane.checkTabs()
  }
  //Initiate the shutdown of the program
  private[interface] def openExitDialog() = {
    UI.menuInterface.close()
    state = SHUTDOWN
  }

  //Initiate saving the current state of the simulation into a file
  private[interface] def openSaveDialog() = {
    UI.menuInterface.close()
    /**If the simulation has a name, check whether there's a file corresponding to it.
      *If there is, ask the user if they want to overwrite. Otherwise ask for a name */
    Simulation.name match {
      case Some(n) => UI.overwriteSimInterface.open(n)
      case None => UI.IOInterface.save()
    }
  }
  /** Sets the name for the simulation if it doesn't have one yet and attempts
    * to save the state under that name. */
  private[interface] def sendSaveQuery(name: String) = {
    Simulation.name = Some(name)
    //Check if the name is the name of the default sim. Cancel the process if it is
    if(Simulation.name.exists(_ == "SOL-MCRF")){
      Simulation.name = None
      printMsg("Cannot overwrite default file")
    }
    else {
      val report = IOHandler.saveSimData()
      if(!report.isDefined){
        printMsg(s"Saved $name")
      }
      else {
        report.foreach(m => printMsg(s"Saving $name failed. $m"))
      }
    }
  }

  //Initiate loading the state of a simulation from a file
  private[interface] def openLoadDialog() = {
    UI.menuInterface.close()
    UI.IOInterface.load()
  }
  //Helper methods for opening a new sim and reloading the sim that was last loaded
  private[interface] def openNewSim() = loadQueue.enqueue("")
  private[interface] def reloadSim()  = loadQueue.enqueue(lastLoaded)

  /** Attempt to load a simulation with the given name. If
    * the name is empty, the method simply resets the simulation. */
  private[interface] def sendLoadQuery(name: String) = {
    if(name.isEmpty){
      Simulation.resetSim()
      resetGUI()
      clearScreen()
      UI.hotbar.visible = true
      printMsg("Created new simulation")
    }
    else {
      val report = IOHandler.loadSimData(name)
      if(!report.isDefined){
        resetGUI()
        clearScreen()
        UI.hotbar.visible = true
        updateData()
        UI.infoPane.checkTabs()
        lastLoaded = name
        printMsg(s"Loaded $name")
      }
      else {
        report.foreach(m => printMsg(s"Loading $name failed. $m"))
      }
    }
  }


  //Check if a body's info page is currently opened. Sets the body in focus if true
  private def checkFocus() = {
    bodyData.foreach(b => {
      if(UI.infoPane.selection.page.title == b.name){
        inFocus = Some(b)
      }
    })
  }

  /** Starts the print animation of the toPrint object and sets
    * the message for it to display */
  private[interface] def printMsg(message: String) = {
    UI.toPrint.text = message
    UI.toPrint.start()
  }
  //-------------------------------------------------------------------------------------//



  //Helpers
  //-------------------------------------------------------------------------------------//

  /** Calculates the unit vector for velocity from the movements of the mouse
    * when the user is aiming the direction of the movement. X and Y components
    * are gotten simply from the coordinates of the mouse but as there is no 3rd
    * dimension, the Z component comes as a function of the distance from the mouse
    * to the body's position and the dmax variable defined below. */
  private def getAimVector(): (Double, Int) = {
    val bPos = inputInts.get
    val v = inputDoubles.get(0)
    val dmax = max(MIN_AIM_SIZE, (v*2).toInt) //In practice, this defines a unit circle around the body
    val d = sqrt(pow(bPos(0)-mTrack(0), 2)+pow(bPos(1)-mTrack(1), 2))
    var alpha = Pi/2 - (d*Pi)/(2*dmax) //The z component is then calculated using the unit circle
    var vecx = (mTrack(0)-bPos(0)).toDouble //If inside dmax, X and Y are mouse coordinates in
    var vecy = (mTrack(1)-bPos(1)).toDouble //reference to the body's position
    if(d >= dmax){
      vecx = dmax*((mTrack(0)-bPos(0))/d) //Otherwise they are scaled down and then lengthened to
      vecy = dmax*((mTrack(1)-bPos(1))/d) //reach the edge of dmax
      alpha = 0.0 //z component is only defined inside dmax
    }
    /** This method creates a hemisphere whose depth defines z:s magnitude.
      * However, its sign has to be defined externally (see the event listening section of GUI)*/
    val zsign = if(mTrack(2) == 0.0) 1 else mTrack(2)/abs(mTrack(2))
    aimVector = Vector(vecx/dmax, vecy/dmax, sin(alpha)*zsign)
    /** Returns the current d and dmax for convenience, as they are used
      * to draw the vector */
    (d, dmax)
  }


  /** Function for converting the timescale slider (tSlider) value into the actual timescale
    * for the simulation.*/
  private[interface] def timeConvert() = pow(E, 0.1*pow(UI.tSlider.value, 0.4)).toInt

  /** Function for changing the rate by which distances are scaled. The rate is exponential based on
    * the current scale. Smaller scale -> slower rate, larger scale -> faster rate */
  private def fScaleConvert(x: Int) = {
    max(10000, min(fullScale.get +x*(pow(10, 6) + pow(E, 0.9*pow(fullScale.get, ZOOM_DELTA))), pow(10, 10)))
  }

  /** Function for changing the rate by which bodies' radii are scaled. The rate is always
    * a percentage of fullScale. The percentage is gotten as a user input
    * from the radius slider (rSlider) */
  private def bScaleConvert() = {
    val slider = UI.rSlider.value
    scaleOff = (100-(19*log(1 + slider)))/100.0
    fullScale.get*scaleOff
  }
  //-------------------------------------------------------------------------------------//



  //Graphics
  //-------------------------------------------------------------------------------------//

  //Helper method for drawing the tails of the bodies
  private def drawTails(g: Graphics2D) = {
    g.setStroke(new BasicStroke(1))
    orbitData.foreach(a => {
      var p0 = a.head
      var i = 255.0
      val step = i/a.size
      /** Draw a line between each previous points while incrementally
        * reducing the opacity of the color used */
      a.foreach(p => {
        val opacity = max(0, i.toInt)
        if(i > 200) g.setColor(new Color(100, 100, 100, opacity))
        else g.setColor(new Color(60, 60, 60, opacity))
        g.drawLine(p0(0), p0(1), p(0), p(1))
        p0 = p
        i -= step
      })
    })
  }

  /** Helper method for drawing the coordinate list displayed while
    * the user is choosing the position of a new body */
  private def drawCoords(g: Graphics2D) = {
    val radius = max(3, bodyScale(cbrt(inputDoubles.get(3))))
    val trans1 = translate(-radius, -radius, mTrack)
    g.fillOval(trans1(0), trans1(1), 2*radius, 2*radius)
    val largeoff = 90; val smalloff = 20
    //Where the list is drawn depends on where the mouse is in relation to the middle
    val x = {
      if(mTrack(0) > w/2) mTrack(0) - largeoff
      else mTrack(0) + smalloff
    }
    val y = {
      if(mTrack(1) > h/2) mTrack(1) - largeoff
      else mTrack(1) + smalloff
    }
    val vgap = 17
    g.setFont(new Font(fonts(2), Font.PLAIN, vgap-2))
    mTrans = fullScale.rev(translate(-xOff, -yOff, mTrack))
    //Draw the coordinates in astronomical units
    g.drawString(f"X: ${mTrans(0)/AU}%.3f", x, y+vgap)
    g.drawString(f"Y: ${mTrans(1)/AU}%.3f", x, y+2*vgap)
    g.drawString(f"Z: ${mTrans(2)/AU}%.3f", x, y+3*vgap)
    g.drawString("AU", x+smalloff, y+4*vgap)
  }

  // Helper method for drawing the head of an arrow/vector
  private def drawArrowhead(g: Graphics2D, root: (Int, Int), tip: (Int, Int), alpha: Double) = {
    val d = sqrt(pow(tip._1-root._1, 2)+pow(tip._2-root._2, 2))
    /** If the distance between the root and the tip of the arrow is above 0,
      * take the unit vector pointing from the root to the tip, multiply it
      * some scalar and rotate itby alpha to the left and the right.
      * This defines the coordinates for the base of the arrowhead */
    if(d != 0){
      val u     = Vector((tip._1-root._1)*(3.0/4), (tip._2-root._2)*(3.0/4))
      val left  = ((u(0)*cos(alpha)-u(1)*sin(alpha)).toInt + root._1,   (u(0)*sin(alpha)+u(1)*cos(alpha)).toInt + root._2)
      val right = ((u(0)*cos(-alpha)-u(1)*sin(-alpha)).toInt + root._1, (u(0)*sin(-alpha)+u(1)*cos(-alpha)).toInt + root._2)
      val aHead = new Polygon(Array(tip._1, left._1, right._1), Array(tip._2, left._2, right._2), 3)
      g.fill(aHead)
    }
  }

  /** Helper method for drawing the velocity vector being aimed by the
    * user when adding a body. (see getAimVector()) */
  private def drawAimVector(g: Graphics2D) = {

    //Draw the arrow
    //---------------------------------------------//
    val stats = getAimVector()
    val bPos = inputInts.get
    val attr = inputDoubles.get
    val v = attr(0)
    val d = stats._1
    val dmax = stats._2
    var aHeadAngle = (d*Pi)/(10*dmax) //The angle defining the size of the arrowhead
    var vecx = (aimVector(0)*dmax).toInt + bPos(0)
    var vecy = (aimVector(1)*dmax).toInt + bPos(1)
    if(d >= dmax) aHeadAngle = Pi/10

    g.setColor(OFF_WHITE)
    g.setStroke(new BasicStroke(2))
    g.drawLine(bPos(0), bPos(1), vecx, vecy)
    drawArrowhead(g, (bPos(0), bPos(1)), (vecx, vecy), aHeadAngle)

    //Draw the body and the vector components in numbers
    //---------------------------------------------//
    g.setColor(Color.WHITE)
    val radius = max(3, bodyScale(cbrt(attr(3))))
    val trans = translate(-radius, -radius, bPos :+ 0)
    g.fillOval(trans(0), trans(1), 2*radius, 2*radius)

    val largeoff = 90; val smalloff = 20
    val x = {
      if(vecx > bPos(0)) bPos(0) -largeoff
      else bPos(0) +smalloff
    }
    val y = {
      if(vecy > bPos(1)) bPos(1) -largeoff
      else bPos(1) +smalloff
    }
    val vgap = 17
    g.setFont(new Font(fonts(2), Font.PLAIN, vgap-2))
    g.drawString(f"Vx: ${v*aimVector(0)}%.3f", x, y+vgap)
    g.drawString(f"Vy: ${v*aimVector(1)}%.3f", x, y+2*vgap)
    g.drawString(f"Vz: ${v*aimVector(2)}%.3f", x, y+3*vgap)
    g.drawString("km/s", x+smalloff, y+4*vgap)
  }

  //Helper method for creating the background image.
  private def buildBackground() = {
    val bg = emptyImage(w, h)
    val g  = bg.graphics
    val center = new Point2D.Float(w/2, h/2)
    val radius: Float = w/2
    val dist   = Array(0.0f, 0.7f, 1.0f)
    val colors = Array(BLACK, SPACE_MARINE, DEEP_SPACE)
    val paint  = new RadialGradientPaint(center, radius, dist, colors, CycleMethod.NO_CYCLE)
    g.setPaint(paint)
    g.fillRect(0, 0, w, h)
    g.dispose()
    bg
  }

  /** The main method for drawing graphics in the program. Puts together
    * all the graphical components into an image and returns it to the view
    * object's paintComponent method */
  private def buildImg() = {
    val img = emptyImage(w, h)
    val g = img.graphics

    g.drawImage(bgImage, 0, 0, null) //Draw background
    drawTails(g) //Draw the orbits of the bodies

    /** Draw a circle corresponding to a body's scaled radius immediately once the body's
      * position has been calculated. */
    g.setColor(Color.WHITE)
    translated.zipWithIndex.foreach(b => b._1._1.onComplete{
      case Success(pos) => {
        g.fillOval(pos(0), pos(1), 2*b._1._2, 2*b._1._2)
      }
      case Failure(t)   => {}
      awaitRefresh(b._2) = true   //Mark the current pos as having been "refreshed"
    })

    //Wait for all bodies to finish calculation and drawing their corresponding circles
    val maxAwait = Future {Thread.sleep(SolarSimApp.frameDelay)}     //Max wait time
    while(!(awaitRefresh.fold(true)(_&&_)) && !(maxAwait.isCompleted)){};
    //Depending on the GUIs state, draw the necessary other components
    if(state == ADDING) drawCoords(g)
    else if(state == AIMING) drawAimVector(g)

    g.dispose()  //Dispose temporary graphics environment and return the drawn image
    img          //to view.paintComponent
  }

  //All GUI content is contained in view.
  val view: BorderPanel = new BorderPanel {

    override def paintComponent(g: Graphics2D) = {
      g.drawImage(buildImg(), 0, 0, null)
    }
    layout(UI.infoPane)    = East   //The bodies' info pages
    layout(UI.hotbar)      = South  //The toolbar at the bottom
    layout(UI.centerStage) = Center //Most of the interfaces and the print mechanism
    visible = true
    opaque  = true
  }
  //-------------------------------------------------------------------------------------//


  //Event listening and handling
  //-------------------------------------------------------------------------------------//
  listenTo(view.mouse.wheel)
  listenTo(view.mouse.moves)
  listenTo(view.mouse.clicks)
  listenTo(view.keys)
  listenTo(UI.addButton)
  listenTo(UI.menuButton)
  listenTo(UI.newButton)
  listenTo(UI.saveButton)
  listenTo(UI.reloadButton)
  listenTo(UI.loadButton)
  listenTo(UI.exitButton)
  listenTo(UI.infoButton)

  reactions += {
    /** The mouse wheel does different things depending on
      * the current state value.
      * DEFAULT: Changes fullScale
      * ADDING: Changes the body's position on the z-axis
      * AIMING: Changes the body's velocity vector's z-component's sign
      */
    case wheelEvent: MouseWheelMoved => {
      val rot = wheelEvent.rotation
      if(state == DEFAULT){
        scaleQueue.enqueue(rot)
        updateData()
      }
      else if(state == ADDING) {
        mTrack = Vector(mTrack(0), mTrack(1), mTrack(2)+rot)
        mTrans = fullScale.rev(translate(-xOff, -yOff, mTrack))
      }
      else {
        mTrack = Vector(mTrack(0), mTrack(1), rot/abs(rot))
      }
    }
    /** Clicking with the mouse does different things depending on
      * the current state value.
      * ADDING: Left click confirms the position of the body to be added
      *         others cancel the adding process
      * AIMING: Left click onfirms the velocity vector of the body to be added
      *         others revert the state back to ADDING
      */
    case click: MouseClicked => {
      if(state == ADDING){
        if(SwingUtilities.isLeftMouseButton(click.peer)){
          inputInts = Some(Vector(click.point.x, click.point.y))
          startAim()
        }
        else finishAddBody(false)
      }
      else if(state == AIMING){
        if(SwingUtilities.isLeftMouseButton(click.peer)){
          finishAddBody(true)
        }
        else state = ADDING
      }
    }
    /** Mouse movements are always tracked for convenience. If the state
      * is ADDING, the position is directly converted to it's 'real' values
      * (translated and scaled back) */
    case move: MouseMoved => {
      mTrack = Vector(move.point.x, move.point.y, mTrack(2))
      if(state == ADDING){
        mTrans = fullScale.rev(translate(-xOff, -yOff, mTrack))
      }
    }

    //The offset mechanism when clicking and dragging
    //----------------------------------------------------//
    /** If an interface is not open, clicking and pressing sets the
      * start position of a single dragging action */
    case mpress: MousePressed => {
      if(state == DEFAULT){
        if(!interfaceOpen()){
          mDrag = Some((mpress.point.x, mpress.point.y))
        }
      }
    }
    /** After the start position has been defined, if the mouse is
      * dragged, the current value of the offset is changed by the
      * difference of the drag's start position and its new position */
    case drag: MouseDragged => {
      if(state == DEFAULT){
        mDrag match {
          case Some(m0) => {
            inFocus = None
            mOff = (drag.point.x-m0._1, drag.point.y-m0._2)
            mDrag = Some(m0)
            scaleDirty = true
          }
          case None =>
        }
      }
    }
    /** When the button is released the drag values are reset but the
      * reference point of offsets is changed to match the position to
      * which the graphics were dragged */
    case mrelease: MouseReleased => {
      xRef = xOff
      yRef = yOff
      mDrag = None
      mOff = (0, 0)
    }
    //----------------------------------------------------//


    //Reactions to buttons
    case ButtonClicked(b)   => {
           if(b == UI.addButton)    UI.addBodyInterface.open()
      else if(b == UI.menuButton)   UI.menuInterface.open()
      else if(b == UI.infoButton)   UI.infoPane.open()
      else if(b == UI.newButton)    openNewSim()
      else if(b == UI.exitButton)   openExitDialog()
      else if(b == UI.saveButton)   openSaveDialog()
      else if(b == UI.loadButton)   openLoadDialog()
      else if(b == UI.reloadButton) reloadSim()
    }
  }
//-------------------------------------------------------------------------------------//


//Timer
//-------------------------------------------------------------------------------------//
  //Starts the animation of the program when called by SolarSimApp
  def start(intervalMS: Int) = {
    bgImage = buildBackground()
    updateData()
    val timer = new Timer()
    val task = new TimerTask() {
      def run() = {
        refresh()
        view.repaint()
        /** If shutdown has been initiated via the exit button, cancel the running animation
          * and perform a safe shutdown */
        if(state == SHUTDOWN){
          timer.cancel()
          timer.purge()
          SolarSimApp.quit()
        }
      }
    }
    timer.schedule(task, 0, intervalMS)
  }
}
//-------------------------------------------------------------------------------------//
//End of GUI
