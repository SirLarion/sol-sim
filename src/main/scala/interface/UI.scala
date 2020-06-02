package interface

import simulator.{SolarSimApp, IOHandler}
import misc.ImageExtensions._
import misc.constants._
import scala.swing._
import scala.swing.event._
import scala.swing.BorderPanel.Position._
import java.awt.{Point, Font, Color, Graphics2D, Insets}
import javax.swing.BorderFactory
import java.text.{Format, NumberFormat}

package object UI {

  val h = GUI.h
  val w = GUI.w

  //Helpers
  //-------------------------------------------------------------------------------------//
  private[interface] def outOfFrame(p: Point, component: Component): Boolean = {
    val loc = component.locationOnScreen
    val size = component.size
    p.x < loc.x || p.y < loc.y ||
    p.x > (loc.x + size.width) ||
    p.y > (loc.y + size.height)
  }
  //-------------------------------------------------------------------------------------//



  //Constructor methods and classes for creating UI elements
  //-------------------------------------------------------------------------------------//
  /** Creates objects with a text field for double values and a corresponding
    * error message that is displayed when an invalid value is entered into
    * the field */
  private[interface] class doubleFieldWithError {
    private var value = 0.0
    def apply() = value

    var valid = false //Convenience variable for checking whether the field's value is valid
    val errMessage = new Label("Invalid input value"){
      /** Instead of having its 'visible' value set to false, the message's
        * opacity is 0 because this way the space taken by the message is
        * taken into consideration by swing's layout managers even when it's
        * invisible */
      foreground = INVISIBLE
      font = new Font(GUI.fonts(2), Font.PLAIN, 12)
    }
    val textField = new TextField(12){
      listenTo(this) //text field listens to itself and reacts by displaying error message when necessary
      reactions += {
        case e: EditDone => {
          try { //attempt to convert the text fields contents into a double
            if(!text.isEmpty){
              value = text.toDouble
              errMessage.foreground = INVISIBLE
              valid = true
            }
            else {
              valid = false
            }
          }
          catch { //if the content isn't valid for conversion, display error message
            case nfe: Throwable => {
              errMessage.foreground = ERROR_RED
              valid = false
            }
          }
        }
      }
    }
  }
  private[interface] def inputDoubleField() = new doubleFieldWithError

  /** Constructor for most of the interfaces used in the program.
    * Less repetition is needed when intializing interfaces this way */
  private[interface] class simInterface extends GridBagPanel {
    def open() = {
      visible = true
      listenTo(GUI.view.mouse.clicks)
    }
    def close() = {
      visible = false
      deafTo(GUI.view.mouse.clicks)
    }
    opaque = true
    background = BLUE_CARBON
    border = BorderFactory.createLineBorder(DARK_BLUE_CARBON, 4, true)

    reactions += {
      case click: MouseClicked => {
        if(outOfFrame(click.point, this)){
          close()
        }
      }
    }
    visible = false //The interfaces are invisible by default
  }

  // Constructor for the bodies' info pages.
  private[interface] class infoInterface(body: simulator.Body) extends GridBagPanel {
    import misc.calc._
    maximumSize = new Dimension(w/8, (18*h)/20)
    opaque = true
    background = BLUE_CARBON
    name = body.name

    //Initialize containers for data
    private var vVec   = smult(1.0/kilo, body.v)
    private var v      = vlen(vVec)
    private var aVec   = smult(1.0/kilo, body.a)
    private var a      = vlen(aVec)
    private var pVec   = smult(1/AU, body.position())
    private var d      = vlen(pVec)
    private var m      = body.m
    private var p      = body.p
    private var r      = body.r/kilo

    //Update the data in the text fields
    def update() = {
      vVec = smult(1.0/kilo, body.v)
      v    = vlen(vVec)
      vecF(0).text = f"${vVec(0)}%.3f"; vecF(1).text = f"${vVec(1)}%.3f"; vecF(2).text = f"${vVec(2)}%.3f"
      stdFields(0).text = f"$v%.3f"

      aVec = smult(kilo, body.a)
      a    = vlen(aVec)
      vecF(3).text = f"${aVec(0)}%.3f"; vecF(4).text = f"${aVec(1)}%.3f"; vecF(5).text = f"${aVec(2)}%.3f"
      stdFields(1).text = f"$a%.3f"

      pVec = smult(1/AU, body.position())
      d    = vlen(pVec)
      vecF(6).text = f"${pVec(0)}%.3f"; vecF(7).text = f"${pVec(1)}%.3f"; vecF(8).text = f"${pVec(2)}%.3f"
      stdFields(2).text = f"$d%.3f"

      m    = body.m;      stdFields(3).text = f"$m%E"
      p    = body.p;      stdFields(4).text = f"$p%E"
      r    = body.r/kilo; stdFields(5).text = f"$r%E"
    }

    private val fSize = 10
    private val vecCompSize = 4

    //Inset constructors
    private def labelInsets() = new Insets(5, 10, 5, 0)
    private def attrInsets()  = new Insets(5, 10, 5, 0)
    private def fieldInsets() = new Insets(5, 10, 5, 0)
    private def unitInsets()  = new Insets(0, 5, 0, 0)

    //Text field constructor
    private def infoField(cols: Int, edit: Boolean) = new TextField {
      columns = cols
      editable = edit //Room to enable body editability here
    }
    //Constructor for a grid of vector components
    private def vecComponents(fields: Vector[TextField], labels: Vector[Label]) = {
      new GridPanel(3, 2) {
        opaque = false
        hGap = 5
        vGap = 5
        var i = 0
        while(i < 3){
          contents += labels(i)
          contents += fields(i)
          i += 1
        }
      }
    }

    //Constructor for the elements needed to visualise a single attribute
    private def attrGrid(name: String, fIndex: Int) = {
      val attrName = name
      new GridBagPanel {
        opaque = false
        layout(text(attrName)) = new Constraints() {
          grid = (0, 0)
          weightx = 1.0
          gridwidth = 2
          insets = labelInsets()
          anchor = GridBagPanel.Anchor.LineStart
        }
        layout(stdFields(fIndex)) = new Constraints() {
          grid = (0, 1)
          insets = fieldInsets()
          anchor = GridBagPanel.Anchor.LineStart
        }
        layout(unitL(fIndex +1)) = new Constraints() {
          grid = (1, 1)
          weightx = 0.5
          insets = unitInsets()
          anchor = GridBagPanel.Anchor.LineStart
        }
      }
    }
    //Constraint constructor for the GridBagPanels from attrGrid
    private def attrConstraints(y: Int) = new Constraints {
        grid = (0, y)
        weightx = 1.0
        weighty = 0.5
        anchor = GridBagPanel.Anchor.LineStart
        insets = attrInsets()
    }
    //Initialize the standard text fields
    private val stdFields = Vector(
        infoField(fSize, false), //velocity
        infoField(fSize, false), //acceleration
        infoField(fSize, false), //distance to star
        infoField(fSize, false), //mass
        infoField(fSize, false), //density
        infoField(fSize, false), //radius
    )
    //Initialize the smaller text fields and labels used by vector components
    private val vecF = Vector.fill[TextField](9)(infoField(vecCompSize, false))
    private val vecL = Vector(
      text("X:"), text("Y:"), text("Z:"),
      text("X:"), text("Y:"), text("Z:"),
      text("X:"), text("Y:"), text("Z:")
    )
    //Initialize labels for units
    private val unitL = Vector(
      text("km/s", 15, 0), text("mm/s^2", 15, 0), text("AU", 15, 0), text("AU", 15, 0),
      text("kg", 15, 0),   text("kg/m^3", 15, 0), text("km", 15, 0)
    )

    //Initialize vector component grids
    private val vComponents   = vecComponents(vecF.slice(0, 3), vecL.slice(0, 3))
    private val aComponents   = vecComponents(vecF.slice(3, 6), vecL.slice(3, 6))
    private val posComponents = vecComponents(vecF.slice(6, 9), vecL.slice(6, 9))

    /** Initialize v, a and pos separately as they need vector component fields as well
      * while the other attributes don't */
    private val velocity = new GridBagPanel {
      opaque = false
      layout(text("Velocity:")) = new Constraints() {
        grid = (0, 0)
        weightx = 1.0
        insets = labelInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(stdFields(0)) = new Constraints() {
        grid = (0, 1)
        insets = fieldInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(unitL(0)) = new Constraints() {
        grid = (1, 1)
        weightx = 0.5
        insets = unitInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(vComponents) = new Constraints() {
        grid = (0, 2)
        weightx = 1.0
        gridheight = 3
        insets = fieldInsets()
      }
    }
    private val acceleration = new GridBagPanel {
      opaque = false
      layout(text("Acceleration:")) = new Constraints() {
        grid = (0, 0)
        weightx = 1.0
        insets = labelInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(stdFields(1)) = new Constraints() {
        grid = (0, 1)
        insets = fieldInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(unitL(1)) = new Constraints() {
        grid = (1, 1)
        weightx = 0.5
        insets = unitInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(aComponents) = new Constraints() {
        grid = (0, 2)
        weightx = 1.0
        gridheight = 3
        insets = fieldInsets()
      }
    }
    private val position = new GridBagPanel {
      opaque = false
      layout(text("Position:")) = new Constraints() {
        grid = (0, 0)
        weightx = 0.5
        insets = labelInsets()
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(unitL(2)) = new Constraints() {
        grid = (1, 0)
        weightx = 0.5
        insets = new Insets(0, 15, 0, 0)
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(posComponents) = new Constraints() {
        grid = (0, 1)
        weightx = 0.5
        gridheight = 3
        insets = fieldInsets()
      }
    }

    //Initialize the rest of the attribute fields
    private val distance = attrGrid("Distance to origin:", 2)
    private val mass     = attrGrid("Mass:", 3)
    private val density  = attrGrid("Density:", 4)
    private val radius   = attrGrid("Radius:", 5)

    private val delButton = uiButton("Delete")
    layout(text(body.name, h/25, 0)) = new Constraints() {
      grid = (0, 0)
      weighty = 0.5
      anchor = GridBagPanel.Anchor.PageStart
      insets = new Insets(h/40, 0, 0, 0)
    }
    layout(velocity)     = attrConstraints(2)
    layout(acceleration) = attrConstraints(3)
    layout(position)     = attrConstraints(4)
    layout(distance)     = attrConstraints(5)
    layout(mass)         = attrConstraints(6)
    layout(density)      = attrConstraints(7)
    layout(radius)       = attrConstraints(8)

    layout(delButton)    = new Constraints() {
      grid = (0, 9)
      weighty = 1.0
      anchor = GridBagPanel.Anchor.PageEnd
      insets = new Insets(0, 0, 20, 0)
    }
    listenTo(delButton)

    reactions += {
      case del: ButtonClicked => {
        GUI.enqueueDelete(body.name)
      }
    }
  }
  //End of infoInterface constructor

  //Label constructors
  private[interface] def text(content: String) = new Label(content){
    font = new Font(GUI.fonts(2), Font.PLAIN, 16)
    foreground = Color.WHITE
  }
  private[interface] def text(content: String, fsize: Int, findex: Int) = new Label(content){
    font = new Font(GUI.fonts(findex), Font.PLAIN, fsize)
    foreground = Color.WHITE
  }

  //Button constructors
  private[interface] def uiButton(text: String) = new Button(text){
    minimumSize = new Dimension(50, 30)
    font = new Font(GUI.fonts(2), Font.PLAIN, 13)
  }
  private[interface] def uiButton(text: String, s: (Int, Int)) = new Button(text){
    preferredSize = new Dimension(s._1, s._2)
    font = new Font(GUI.fonts(2), Font.PLAIN, 13)
  }

  //-------------------------------------------------------------------------------------//




  //UI elements
  //-------------------------------------------------------------------------------------//

  /** The sidebar that opens when clicking the 'Bodies' button.
  * Contains infoInterface instances displaying body data. */
  private[interface] object infoPane extends TabbedPane {
    preferredSize = new Dimension(w/8, (10*h)/20)
    tabPlacement = Alignment.Bottom
    tabLayoutPolicy = TabbedPane.Layout.Scroll

    /** Method for updating tabs. Checks that there are info pages
      * for all bodies in bodyData and updates the contents of the
      * page that's currently selected */
    def checkTabs() = {
      GUI.bodyData.foreach(b => {
        if(pages.forall(_.title != b.name)){
          addTab(b)
        }
      })
      selection.page.content.asInstanceOf[infoInterface].update()
    }
    //Helper method for removing all tabs. Used when loading a simulation
    def clearTabs() = {
      while(pages.size > 0){
        pages.remove(0)
      }
    }
    def open() = {
      if(GUI.bodyData.size > 0){
        visible = true
        listenTo(GUI.view.mouse.clicks)
      }
      else GUI.printMsg("No bodies to display")
    }
    def close() = {
      deafTo(GUI.view.mouse.clicks)
      visible = false
    }
    //Helper method for adding and removing tabs
    private[interface] def addTab(body: simulator.Body)  = {
      pages.addOne(new TabbedPane.Page(body.name, new infoInterface(body)))
    }
    private[interface] def delTab(page: TabbedPane.Page) = {
      pages.remove(page.index)
    }

    reactions += {
      case click: MouseClicked => {
        if(outOfFrame(click.point, this)){
          close()
        }
      }
    }
    visible = false
  }
  /** The interface for saving and loading simulations. The state
    * variable 'mode' indicates the current usage of the interface. */
  private[interface] object IOInterface extends simInterface {
    private var mode = true

    def save() = {
      mode = true
      open()
      nameField.requestFocus()
    }
    def load() = {
      mode = false
      open()
      nameField.requestFocus()
    }
    def clear() = {
      nameField.text = ""
      emptyFieldError.foreground = INVISIBLE
    }

    preferredSize = new Dimension(w/7, h/5)
    val title = text("Enter simulation name", 20, 0)
    val nameField = new TextField(10)
    val ok = uiButton("OK")
    val cancel = uiButton("Cancel")

    val emptyFieldError = new Label("Input name"){
      font = new Font(GUI.fonts(2), Font.PLAIN, 16)
      foreground = INVISIBLE
    }

    layout(title) = new Constraints() {
      grid = (0, 0)
      gridwidth = 2
      insets = new Insets(h/50, 0, h/50, 0)
    }
    layout(nameField) = new Constraints() {
      grid = (0, 1)
      gridwidth = 2
      insets = new Insets(0, 0, 3, 0)
      anchor = GridBagPanel.Anchor.PageStart
    }
    layout(emptyFieldError) = new Constraints() {
      grid = (0, 2)
      gridwidth = 2
      anchor = GridBagPanel.Anchor.PageStart
    }
    layout(ok) = new Constraints() {
      grid = (0, 3)
      weighty = 1.0
      weightx = 0.5
      insets = new Insets(0, 5, 0, 5)
      anchor = GridBagPanel.Anchor.LineEnd
    }
    layout(cancel) = new Constraints() {
      grid = (1, 3)
      weighty = 1.0
      weightx = 0.5
      insets = new Insets(0, 5, 0, 5)
      anchor = GridBagPanel.Anchor.LineStart
    }
    listenTo(ok); listenTo(cancel)

    reactions += {
      case ButtonClicked(b) => {
        if(b == ok){
          if(!nameField.text.isEmpty){
            if(mode){
              close()
              if(IOHandler.checkForOverwrite(nameField.text))
                overwriteSimInterface.open(nameField.text)
              else
                GUI.sendSaveQuery(nameField.text);
              clear()
            }
            else {
              close()
              GUI.loadQueue.enqueue(nameField.text)
              clear()
            }
            emptyFieldError.foreground = INVISIBLE
          }
          else {
            emptyFieldError.foreground = ERROR_RED
          }
        }
        else {
          close()
          clear()
        }
      }
    }
  }

  /** Separate interface for asking the user if they want to overwrite an
    * existing simulation file */
  private[interface] object overwriteSimInterface extends simInterface {
    preferredSize = new Dimension(w/8, h/6)
    private var simname = ""
    def open(sName: String){
      visible = true
      listenTo(GUI.view.mouse.clicks)
      simname = sName
    }
    val title = text("Overwrite existing file?", 20, 0)
    val yes = uiButton("Yes")
    val no  = uiButton("No")
    layout(title) = new Constraints() {
      grid = (0, 0)
      gridwidth = 2
      insets = new Insets(20, 0, 10, 0)
    }
    layout(yes) = new Constraints() {
      grid = (0, 1)
      weighty = 1.0
      weightx = 0.5
      insets = new Insets(0, 5, 0, 5)
      anchor = GridBagPanel.Anchor.LineEnd
    }
    layout(no) = new Constraints() {
      grid = (1, 1)
      weighty = 1.0
      weightx = 0.5
      insets = new Insets(0, 5, 0, 5)
      anchor = GridBagPanel.Anchor.LineStart
    }
    listenTo(yes); listenTo(no)

    reactions += {
      case ButtonClicked(b) => {
        if(b == yes){
          GUI.sendSaveQuery(simname)
          close()
        }
        else {
          IOInterface.save()
          close()
        }
      }
    }
  }

  // Interface for starting the process of adding a body to the simulation
  private[interface] object addBodyInterface extends simInterface {
    preferredSize = new Dimension(w/5, h/2)

    override def open() {
      visible = true
      listenTo(GUI.view.mouse.clicks)
      inputName.requestFocus()
    }

    //Helper method for clearing all the input fields and error messages
    def clear() = {
      fields.foreach(f => {
        f.textField.text = ""
        f.errMessage.foreground = INVISIBLE
      })
      inputName.text = ""
      nameLengthError.foreground = INVISIBLE
      initializationError.foreground = INVISIBLE
    }

    /** Constructor for the attribute labels. Technically text fields
      * because that way their size is easier to control */
    private def label(title: String) = new TextField {
      font = new Font(GUI.fonts(2), Font.PLAIN, 15)
      columns = 4
      editable = false
      focusable = false
      opaque = false
      horizontalAlignment = Alignment.Right
      border = BorderFactory.createLineBorder(INVISIBLE)
      foreground = Color.WHITE
      text = title
    }

    val fields = Vector.fill(4)(inputDoubleField())
    val labels = Vector(label("Velocity"), label("Mass"), label("Density"), label("Radius"))
    val units  = Vector(text("km/s", 13, 0), text("kg", 13, 0), text("kg/m^3", 13, 0), text("km", 13, 0))

    /** Constructor for a single attribute's UI elements in the interface
      * Contains the label, text field, error message, and unit for the attribute */
    def attrGrid(index: Int) = new GridBagPanel {
      opaque = false
      layout(labels(index)) = new Constraints(){
        grid    = (0, 0)
        fill    = GridBagPanel.Fill.Horizontal
        anchor  = GridBagPanel.Anchor.LineEnd
        insets  = new Insets(0, 0, 0, 7)
      }
      layout(fields(index).textField) = new Constraints(){
        grid = (1, 0)
        gridwidth = 2
        insets = new Insets(0, 0, 5, 0)
      }
      layout(fields(index).errMessage) = new Constraints(){
        grid = (1, 1)
        gridwidth = 2
        anchor = GridBagPanel.Anchor.LineStart
      }
      layout(units(index)) = new Constraints() {
        grid    = (3, 0)
        fill    = GridBagPanel.Fill.Horizontal
        anchor  = GridBagPanel.Anchor.LineStart
        insets  = new Insets(0, 7, 0, 0)
      }
    }

    //Constraints for the grids generated by attrGrid
    def attrConstraints(y: Int) = new Constraints() {
      grid = (0, y)
      gridwidth = 4
      anchor  = GridBagPanel.Anchor.LineStart
      insets = new Insets(15, 15, 15, 5)
    }

    val labelTop  = text("Add new body", h/40, 0)
    val labelName = label("Name")
    val inputName = new TextField(12)
    val nameLengthError = new Label("Name is too long"){
      font = new Font(GUI.fonts(2), Font.PLAIN, 12)
      foreground = INVISIBLE
    }

    /** Name input initialized separately from the other attributes
      * since it doesn't have a unit */
    val nameGrid = new GridBagPanel {
      opaque = false
      layout(labelName) = new Constraints() {
        grid    = (0, 0)
        weightx = 0.3
        fill    = GridBagPanel.Fill.Horizontal
        anchor  = GridBagPanel.Anchor.LineEnd
        insets  = new Insets(0, 0, 0, 5)
      }
      layout(inputName) = new Constraints(){
        grid = (1, 0)
        weightx = 0.3
        gridwidth = 2
        anchor = GridBagPanel.Anchor.LineStart
        insets = new Insets(0, 0, 5, 0)
      }
      layout(nameLengthError) = new Constraints(){
        grid = (1, 1)
        gridwidth = 2
        anchor = GridBagPanel.Anchor.LineStart
      }
    }

    //Initialize buttons
    val ok     = uiButton("OK", (80, 40))
    val cancel = uiButton("Cancel", (80, 40))
    val buttons = new GridPanel(1, 2){
      opaque = false
      hGap = 7
      contents += ok
      contents += cancel
    }
    val initializationError = new Label("Necessary values not given"){
      font = new Font(GUI.fonts(2), Font.PLAIN, 16)
      foreground = INVISIBLE
    }

    layout(labelTop) = new Constraints() {
      grid = (0, 0)
      gridwidth = 4
      weighty = 0.5
      insets = new Insets(15, 0, 0, 0)
      anchor = GridBagPanel.Anchor.Center
    }
    layout(nameGrid) = new Constraints() {
      grid = (0, 1)
      gridwidth = 3
      insets = new Insets(15, 15, 15, 5)
      anchor = GridBagPanel.Anchor.LineStart
    }
    layout(attrGrid(0)) = attrConstraints(2)
    layout(attrGrid(1)) = attrConstraints(3)
    layout(attrGrid(2)) = attrConstraints(4)
    layout(attrGrid(3)) = attrConstraints(5)

    layout(buttons) = new Constraints() {
      grid = (0, 6)
      gridwidth = 4
      insets = new Insets(26, 0, 0, 0)
      anchor = GridBagPanel.Anchor.Center
    }
    layout(initializationError) = new Constraints() {
      grid = (0, 7)
      gridwidth = 4
      weighty = 0.5
      insets = new Insets(5, 0, 0, 0)
      anchor = GridBagPanel.Anchor.PageStart
    }

    listenTo(ok); listenTo(cancel); listenTo(inputName)

    reactions += {
      //If ok button is pressed, check that all fields meet requirements
      case ButtonClicked(b) => {
        if(b == ok){
          if(fields.forall(_.valid) && inputName.text.length <= 20){
            //Set first input values and then start the rest of the adding process
            GUI.inputDoubles = Some(fields.map(_()))
            GUI.inputStrings = Some(Vector(inputName.text))
            GUI.startAddBody()
            close()
          }
          else {
            initializationError.foreground = ERROR_RED
          }
        }
        else close()
      }
      case e: EditDone => {
        //Check if the length of the name is viable. Improves UI predictability
        if(inputName.text.length > 20) nameLengthError.foreground = ERROR_RED
        else nameLengthError.foreground = INVISIBLE
      }
    }
  }
  //End of addBodyInterface

  //Slider for changing the timescale of the simulation (see timeConvert in GUI)
  private[interface] val tSlider = new Slider {
    orientation = Orientation.Horizontal
    min   = 1
    max   = 200000
    value = 1
    paintTicks = false
    labels = Map(max/7 -> text("Timescale", h/90, 2))
    paintLabels = true
    opaque = false
    preferredSize = new Dimension(w/5, 30)
  }

  //Slider for changing the displayed scale of bodies (see bScaleConvert in GUI)
  private[interface] val rSlider = new Slider {
    orientation = Orientation.Horizontal
    min = 0
    max = 99
    value = 33
    paintTicks = false
    labels = Map((2*max)/7 -> text("Body size", h/90, 2))
    paintLabels = true
    opaque = false
    preferredSize = new Dimension(w/9, 30)
  }

  //Slider for changing the maximum amount of stored points in a body's orbit
  private[interface] val lSlider = new Slider {
    orientation = Orientation.Horizontal
    min = 1
    max = MAX_ORBIT_LENGTH
    value = 750
    paintTicks = false
    labels = Map((2*max)/7 -> text("Tail length", h/90, 2))
    paintLabels = true
    opaque = false
    preferredSize = new Dimension(w/9, 30)
  }

  /** Displays the current timescale in an easily readable format. Changes according to the
    * largest possible unit where the value is an integer. */
  private[interface] object Timer extends TextField {
    private var secs = GUI.timeConvert() * SolarSimApp.getFPS
    font = new Font(GUI.fonts(2), Font.PLAIN, 18)
    columns = 5
    editable = false
    focusable = false
    opaque = false
    horizontalAlignment = Alignment.Right
    border = BorderFactory.createLineBorder(INVISIBLE)
    foreground = Color.WHITE
    text = s"${secs/60} min/s"

    //Refresh the local value of the timescale in seconds/frame
    def update() = {
      secs = GUI.timeConvert() * SolarSimApp.getFPS
      val mins  = secs/60
      val hrs  = mins/60
      val days = hrs/24
      val yrs  = days/365
      mins match {
        case m if(yrs >= 1)  => text = s"$yrs a/s"
        case m if(days >= 1) => text = s"$days d/s"
        case m if(hrs >= 1)  => text = s"$hrs h/s"
        case m if(mins >= 1) => text = s"$mins min/s"
        case m =>               text = s"$secs sec/s"
      }
    }
  }

  /** Displays the current value of fullScale in an easily readable format
    * changes according to the largest power of 10 divided by which the value
    * of fullScale is above 1.0*/
  private[interface] object scaleDisplay extends TextField {
    import scala.math.pow
    private var factor = GUI.fullScale.get/kilo
    font = new Font(GUI.fonts(2), Font.PLAIN, 18)
    columns = 10
    editable = false
    focusable = false
    opaque = false
    horizontalAlignment = Alignment.Right
    border = BorderFactory.createLineBorder(INVISIBLE)
    foreground = Color.WHITE
    text = "4.0 * 10^6 km/px"

    //Refresh the local value of the timescale in seconds/frame
    def update() = {
      factor = GUI.fullScale.get/kilo
      //Calculate the largest power of 10 and it's multiplier
      val toPower = (0 to 7).reverse.zip((0 to 7).reverse.map(x => factor/pow(10, x))).dropWhile(_._2 < 1).headOption
      toPower match {
        case Some(p) => {
          if(p._1 >= 4) text = f"${p._2}%1.1f * 10^${p._1}%d km/px"
          else          text = s"${(p._2*pow(10, p._1)).toInt} km/px"
        }
        case None =>    text = "<1 km/px"
      }
    }
  }

  //Most of the buttons used in the UI
  private[interface] val addButton    = uiButton("Add")
  private[interface] val exitButton   = uiButton("Quit")
  private[interface] val newButton    = uiButton("New")
  private[interface] val saveButton   = uiButton("Save")
  private[interface] val loadButton   = uiButton("Load")
  private[interface] val reloadButton = uiButton("Reload Sim")
  private[interface] val menuButton   = uiButton("Menu")
  private[interface] val infoButton   = uiButton("Bodies")

  //The contents of the hotbar
  private[interface] val hotbarOrganizer = new GridBagPanel {
    opaque = true
    background = BLUE_CARBON
    border = BorderFactory.createLineBorder(BLUE_CARBON, 4)
    layout(menuButton) = new Constraints() {
      grid = (0, 0)
      insets = new Insets(3, 20, 3, 0)
    }
    layout(reloadButton) = new Constraints() {
      grid = (1, 0)
      weightx = 1.0
      insets = new Insets(0, 10, 0, 5)
      anchor = GridBagPanel.Anchor.LineStart
    }
    layout(lSlider) = new Constraints() {
      grid = (3, 0)
      insets = new Insets(0, 10, 0, 5)
    }
    layout(rSlider) = new Constraints() {
      grid = (4, 0)
      insets = new Insets(0, 10, 0, 5)
    }
    layout(tSlider) = new Constraints() {
      grid = (5, 0)
      insets = new Insets(0, 10, 0, 5)
      anchor = GridBagPanel.Anchor.LineStart
    }
    layout(Timer) = new Constraints() {
      grid = (6, 0)
      fill = GridBagPanel.Fill.Horizontal
      new Insets(0, 0, 0, 5)
    }
    layout(scaleDisplay) = new Constraints() {
      grid = (7, 0)
      insets = new Insets(0, 10, 0, 5)
      fill = GridBagPanel.Fill.Horizontal
    }
    layout(addButton) = new Constraints() {
      grid = (8, 0)
      insets = new Insets(0, 10, 0, 5)
    }
    layout(infoButton) = new Constraints() {
      grid = (9, 0)
      insets = new Insets(0, 10, 0, 20)
    }
  }

  //The toolbar at the bottom of the UI
  private[interface] val hotbar = new MenuBar {
    opaque = false
    contents += hotbarOrganizer
    preferredSize = new Dimension(w, h/20)
  }

  /** The popup interface that opens when clicking the Menu button.
    * Contains File IO buttons and 'Exit' button */
  private[interface] val menuInterface = new simInterface {
    preferredSize = new Dimension(w/7, h/3)
    val title1 = text("SOLAR SYSTEM", h/40, 0)
    val title2 = text("SIMULATOR", h/40, 1)
    layout(title1) = new Constraints() {
      grid = (0, 0)
      insets = new Insets(h/54, 0, 1, 0)
    }
    layout(title2) = new Constraints() {
      grid = (0, 1)
      weighty = 0.2
      anchor = GridBagPanel.Anchor.PageStart
    }
    layout(newButton) = new Constraints(){
      grid = (0, 2)
      weightx = 1.0
      insets = new Insets(5, 25, 5, 25)
    }
    layout(saveButton) = new Constraints(){
      grid = (0, 3)
      weightx = 1.0
      insets = new Insets(5, 25, 5, 25)
    }
    layout(loadButton) = new Constraints(){
      grid = (0, 4)
      weightx = 1.0
      insets = new Insets(5, 25, 5, 25)
    }
    layout(exitButton) = new Constraints(){
      grid = (0, 5)
      weightx = 1.0
      weighty = 0.2
      insets = new Insets(5, 25, 5, 25)
    }
  }

  /** The print mechanism of the program. When the method printMsg in GUI
    * is called this object fades in by iteratively increasing the opacity
    * of it's color. After displaying for a moment, (defined by delay)
    * it fades back out by decreasing it's opacity*/
  private[interface] object toPrint extends TextArea {
    private var delay = 500
    visible = false
    font = new Font(GUI.fonts(0), Font.PLAIN, h/25)
    def changeState() = {
      val opacity = foreground.getAlpha()
      if((opacity+5) <= 255 && delay > 0)
      foreground = new Color(255, 255, 255, opacity +5);
      else if(opacity == 255 && delay > 0)
      delay -= 1;
      else if((opacity-5) >= 0 && delay == 0)
      foreground = new Color(255, 255, 255, opacity -5);
      else
      visible = false
    }
    def start() = {
      delay = 500
      visible = true
    }
    editable = false
    opaque = false
    foreground = INVISIBLE

    text = ""
  }

  /** Container for the popup interfaces appearing in the center of
    * the GUI */
  private[interface] val centerStage = new GridBagPanel {
    opaque = false
    layout(addBodyInterface) = new Constraints() {
      gridx   = 0;   gridy   = 0
      weightx = 1.0; weighty = 1.0
      anchor = GridBagPanel.Anchor.Center
    }
    layout(menuInterface) = new Constraints() {
      gridx   = 0;   gridy   = 0
      weightx = 1.0; weighty = 1.0
      anchor = GridBagPanel.Anchor.Center
    }
    layout(IOInterface) = new Constraints() {
      gridx   = 0;   gridy   = 0
      weightx = 1.0; weighty = 1.0
      anchor = GridBagPanel.Anchor.Center
    }
    layout(overwriteSimInterface) = new Constraints() {
      gridx   = 0;   gridy   = 0
      weightx = 1.0; weighty = 1.0
      anchor = GridBagPanel.Anchor.Center
    }
    layout(toPrint) = new Constraints() {
      gridx   = 0;   gridy   = 0
      weightx = 1.0; weighty = 1.0
      insets = new Insets(h/20, 0, 0, 0)
      anchor = GridBagPanel.Anchor.PageStart
    }
  }
}
//-------------------------------------------------------------------------------------//
//End of UI
