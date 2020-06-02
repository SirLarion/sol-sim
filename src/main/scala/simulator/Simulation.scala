package simulator

import scala.math._
import misc.constants._
import misc.helpers._
import misc.calc._
import scala.concurrent._
import scala.collection.mutable.{Map, Queue}
import ExecutionContext.Implicits.global


/** Body: the values of a celestial body that are relevant for celestial mechanics */
//--------------------------------------------------------------------------------------//
object Body {
  private[simulator] def apply(v: Vector[Double], a: Vector[Double], m: Double, p: Double, r: Double, name: String, position: Pos, orbit: Queue[Pos]) = {
    new Body(v, a, m, p, r, name, position, orbit)
  }
}
class Body(
  var v: Vector[Double],  //Velocity vector
  var a: Vector[Double],  //Acceleration vector
  var m: Double,          //Mass
  var p: Double,          //Density
  var r: Double,          //Radius
  var name: String,       //The given unique name of the body
  var position: Pos,      //The position of the body in 3Dspace
  val orbit: Queue[Pos]){ //The path of the body thus far

  //Names are unique, same name == same body
  def ==(other: Body) = this.name == other.name

  //Gets all relevant attributes in vector format for saving into a file
  private[simulator] def getAttributes = {
    Vector(this.v(0), this.v(1), this.v(2), this.m, this.p, this.r, this.position.x, this.position.y, this.position.z)
  }
}
//--------------------------------------------------------------------------------------//


/** The core of the simulator. The simulation object does all the necessary heavy
  * calculation in the program. It also handles the creation, editing and deletion of
  * the bodies being simulated. */
//--------------------------------------------------------------------------------------//
object Simulation {

  //Initialization
  //-------------------------------------------------------------------------------------//
  private[simulator] var allBodies = Vector[Body]()   //Bodies being simulated
  private[simulator] var loadBodies = Vector[Body]()  //Temporary container for storing bodies when a simulation is loaded
  private[simulator] var timescale = 1                //Timescale of the simulation in seconds/update

  private var orbitLength = 750                       //The current maximum length of orbits (amount of points stored)
  private var calcFinished = Map[Body, Option[Pos]]() //A helper container for storing the bodies' new positions while the rest are calculated

  var name: Option[String] = None                     //Name of the simulation  (Only used for file formatting)
  var t = 0                                           //Time since epoch in the simulation (in seconds)
  var systemBarycenter = Vector(0.0, 0.0, 0.0)        //The current barycenter of the bodies in the simulation

  //-------------------------------------------------------------------------------------//


  //Communication with the GUI
  //-------------------------------------------------------------------------------------//
  //Reverts the simulation to initial values
  def resetSim() = {
    allBodies = Vector[Body]()
    name = None
    t = 0
    timescale = 1
  }

  //Setters and getters for several corresponding variables
  def setTimescale(scale: Int) = timescale = scale
  def setOrbitLength(len: Int) = orbitLength = len
  def getOrbitLength() = orbitLength
  def getBodies() = allBodies

  def getOrbits(full: Boolean) = allBodies.map(b => {
    val p0 = b.orbit.last
    val p  = b.position
    val gap = (orbitLength*1.0)/MAX_ORBIT_LENGTH * MAX_POS_GAP
    if(p0.d(p) > vlen(vsum(p0(), neg(p()), smult(gap, b.v)))){
      b.orbit.enqueue(p)
    }
    while(b.orbit.length > orbitLength){b.orbit.dequeue}
    val n = if(full) b.orbit.length else 1
    b.orbit.reverse.take(n).map(_()).toVector
  })

  /** The primary interface between the simulator and the GUI. Changes the current
    * time by the size of the timescale and opens new threads for the calculation
    * of each body's next position.
    * @return:  a vector of the next positions of all bodies wrapped in Future objects
    */
  def nextState() = {
    t += timescale
    calcBC()
    calcFinished = Map[Body, Option[Pos]]()         //Reset the state of calcFinished
    allBodies.foreach(b => calcFinished(b) = None)
    Future{checkFinished()}                         //Open new threads for the calculation of
    allBodies.map(b => Future{nextPos(b)})          //the bodies' next positions and waiting for them all to finish
  }

  /** Waits for all nextPos threads to finish calculating and then
    * updates the locally stored positions of the bodies. This is done
    * so that the concurrent position calculations don't interfere with
    * each other. */
  private def checkFinished() = {
    while(calcFinished.values.exists(p => !(p.isDefined))){}
    allBodies.foreach(b => {
      b.position = calcFinished(b).get
    })
  }

  //Finds the body with the given name if it exists
  def findBody(nameOfBody: String): Option[Body] = allBodies.find(_.name == nameOfBody)


  /** Adds a new celestial body to the simulation. In practice this means initializing a new instance of
    * Body and adding it to the allBodies vector. The simulation will then take the new body into account
    * when calculating positions for bodies.
    *
    * @param v    The velocity vector of the body
    * @param r    The position vector of the body
    * @param attr The values of the physical attributes of the new body (v, m, p, r)
    * @param name Name of the body
    * @param mode Defines whether bodies are added by the user or loaded from a file. true -> add, false -> load
    * @return     A message describing whether the body was added or not and why
    */
  def addBody(v: Vector[Double], r: Vector[Double], attr: Vector[Double], name: String, mode: Boolean): String = {
    val mpv = getMPV(attr)
    val pos   = Pos(r)

    /** Check that the given values are viable and that adding the body
      * doesn't exceed the system's capacity */
    if(vlen(v) >= c)
      return "Adding failed. Body went faster than light";
    if(allBodies.exists(b => (b.position.d(pos) + b.r + mpv(2)) <= 0))
      return "Adding failed. Body has collided";
    if(allBodies.size +1 > MAX_BODY_AMOUNT)
      return "Adding failed. Too many bodies";

    var newName = if(name.isEmpty) BODY_NAME else name              //Default name "Body" if a name wasn't given
    if(mode){
      if(allBodies.exists( _.name.take(newName.length) == newName )){ //Check if a body with the same name exists
        newName = findLatestCopy(newName)
      }
    }
    else {
      if(loadBodies.exists( _.name.take(newName.length) == newName )){
        newName = findLatestCopy(newName)
      }
    }
    val body  = Body(v, NULLV, mpv(0), mpv(1), mpv(2), newName, pos, Queue[Pos](pos))

    if(mode)
      allBodies = allBodies :+ body;
    else
      loadBodies = loadBodies :+ body;

    s"Added $newName"
  }

  /** Applies new attribute values to the body whose name was given if such a body exists.
    * @param nameOfBody The name of the body to be edited
    * @param newAttr    The list of new values to be applied to the attributes of the body
    * @param newName    The new name to be given to the body. The default is the original name
    */
  def editBody(nameOfBody: String, newAttr: Vector[Double], name: String) = {
    val mpv = getMPV(newAttr)
    findBody(nameOfBody) match {
      case Some(body) => {
        body.m = mpv(0)
        body.p = mpv(1)
        body.r = mpv(2)
        var newName = body.name
        if(!name.isEmpty){
          if(allBodies.exists(_.name.take(newName.length) == newName)){
            newName = findLatestCopy(newName)
          }
        }
        true
      }
      case None => false
    }
  }


  /** Removes a body from the simulation. As the bodies aren't saved into a variable, removing them
    * from the allBodies vector is enough for Scala's garbage collector to then free up the space taken
    * by the object.
    * @param nameOfBody The name of the body to be removed
    */
  def removeBody(nameOfBody: String): Boolean = {
    if(findBody(nameOfBody).isDefined){
      allBodies = allBodies.filter(_.name != nameOfBody) //Removes the body with the given name
      true  //Found body and removed it
    }
    else {
      false //Could not find body
    }
  }
  //-------------------------------------------------------------------------------------//



  //Calculation and algorithms
  //-------------------------------------------------------------------------------------//

  //Calculates the barycenter of the current system
  private def calcBC() = {
    if(!allBodies.isEmpty){
      val M = allBodies.foldLeft(0.0)(_+_.m)
      systemBarycenter = smult(1/M, vsum(allBodies.map(b => smult(b.m, b.position()))))
    }
    else systemBarycenter = NULLV
  }

  /** Generates a copy for a given name if bodies with the same name already exist. For example
    * the simulation might contain bodies with the default name "Body". If another body with this name
    * is added the method will find the copy with the largest integer as its suffix and name the new body
    * with a larger integer e.g. "Body(2)" if "Body" and "Body(1)" exist.
    *
    * @param name_base The name whose copies are being searched
    */
  private def findLatestCopy(name_base: String) = {
    var copyName = name_base
    val l = copyName.length
    val copies = allBodies.filter( _.name.take(l) == copyName )
    if(!copies.isEmpty) {
      var i = 0
      while(i < copies.length){
        if(copies(i).name.drop(l) == copyName.drop(l)){
          copyName = name_base + s"(${i+1})"
        }
        i += 1
      }
    }
    copyName
  }

  /** Calculates the mass, density and radius (via volume of a sphere) of a body to make sure its
    * physical attributes are plausible.
    * @param given The mass, density and radius that are given as an input
    */
  private def getMPV(given: Vector[Double]) = {
    val m = given(0)
    val p = given(1)
    val r = given(2)
    val v = (4.0/3)*Pi*pow(r, 3)

    //Find the attribute whose value is 0 (meaning the attribute whose value was left for calculation)
    given.zipWithIndex.find( _._1 == 0.0 ) match {
      case Some(attr) => {
        if(attr._2 == 0){
          Vector(p*v, p, r)
        }
        else {
          Vector(m, p, cbrt((3*m)/(4*Pi*p)))
        }
      }
      /** If no value is found default to ignoring the given density and calculate it using given mass and radius
        * as density is the least relevant attribute in the simulation */
      case None => Vector(m, m/v, r)
    }
  }

  /** Position calculation is done by calculating the acceleration vector function for
    * the body with Cowell's method and then numerically integrating it with 4th order
    * Runge-Kutta method to gain updated position and velocity vectors
    * @return The next position for the given body
    */
  private def nextPos(body: Body): Pos = {

    //-----------Initialization-----------//

    val v0  = body.v   //Current velocity vector
    val pos0 = body.position
    val r0  = pos0()   //Current position vector

    //Get the bodies whose gravitational effect is significant
    val others = allBodies.filterNot(_ == body)
    val BC = systemBarycenter

    //Map the bodies to their position vectors in relation to the barycenter
    val posBC = others.map(b => vsum(b.position(), neg(BC)))
    //Map the bodies to their respective coefficients in Cowell's method (m/(r)^3)
    val coeff = others.map(b => b.m/pow(pos0.d(b.position), 3))

    /** The acceleration function is the vector sum of a given position vector and the attracting bodies'
      * position vectors in relation to the barycenter multiplied by the bodies' coefficients
      * and G (gravitational constant) */
    val a = (ri: Vector[Double]) => smult(G, vsum(coeff.zip(posBC).map(b => smult(b._1, vsum(b._2, neg(ri))))))

    //-----------Calculation-----------//
    val r = vsum(r0, neg(BC))
    val dt = Simulation.timescale //Timestep

    /** Modified RK4 for approximating the 1st and 2nd order integrals (velocity and position)
      * of the acceleration vector */
    val k1v = a(r)
    val k1r = v0

    val k2v = a(vsum(r, smult(dt/2, k1r)))
    val k2r = vsum(v0, smult(dt/2, k1v))

    val k3v = a(vsum(r, smult(dt/2, k2r)))
    val k3r = vsum(v0, smult(dt/2, k2v))

    val k4v = a(vsum(r, smult(dt, k3r)))
    val k4r = vsum(v0, smult(dt, k3v))

    val vweighted = smult(dt/6, vsum(k1v, smult(2.0, k2v), smult(2.0, k3v), k4v))
    body.v = vsum(v0, vweighted) //Next velocity vector

    val rweighted = smult(dt/6, vsum(k1r, smult(2.0, k2r), smult(2.0, k3r), k4r))
    val rBC = vsum(r, rweighted) //Next position vector
    body.a = a(rBC)

    val pos = Pos(vsum(BC, rBC))

    calcFinished(body) = Some(pos)
    pos
  }

  //END OF SIM OBJECT
}
