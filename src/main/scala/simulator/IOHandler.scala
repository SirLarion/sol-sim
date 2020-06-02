package simulator

import java.io._

// Handles the input and output of saved simulation data
object IOHandler {

  /** Convert the data of the bodies into an easily writable array
    * of strings */
  private def dataToString(): Array[String] = {
    val stringData = scala.collection.mutable.ArrayBuffer[String]()
    var strBuffer = ""

    Simulation.getBodies.foreach(b => {
      //First write the name of the body and then it's attributes
      strBuffer  += s"${b.name};"
      strBuffer  += b.getAttributes.mkString(";")
      stringData += strBuffer
      strBuffer   = ""
    })
    stringData.toArray
  }

  /** Attempt to open and read the contents of a file with the given name
    * into the simulation object. If unsuccessful, return a corresponding
    * error message. */
  def loadSimData(name: String): Option[String] = {
    var msg: Option[String] = None
    var simName = ""
    try {
      //Files are looked for in the simdata folder with the .ssf suffix
      val fReader = new FileReader(s"simdata/$name.ssf")
      val bReader = new BufferedReader(fReader)

      try {
        var buffer = bReader.readLine()
        //Check the file header.
        if(buffer.toLowerCase != name.toLowerCase){
          throw new IOException("corrupted or edited manually")
        }
        simName = buffer

        buffer = bReader.readLine()
        while(buffer != null){
          //Read body data into corresponding variables
          val name = buffer.takeWhile(_ != ';')
          val bodyData = buffer.drop(name.length+1).split(";").map(_.toDouble).toVector
          val v = bodyData.take(3)
          val r = bodyData.slice(6, 9)
          Simulation.addBody(v, r, bodyData.drop(3), name, false)
          buffer = bReader.readLine()
        }
        Simulation.resetSim() //Reset the simulation only when loading is successful

        //Replace the sim values with the loaded values
        Simulation.allBodies = Simulation.loadBodies
        Simulation.name = if(simName != "SOL-MCRF") Some(simName) else None
      }
      finally {
        bReader.close()
        fReader.close()
      }
    }
    catch {
        case fnf: FileNotFoundException => msg = Some("File not found")
        case ioe: IOException           => msg = Some("Could not read the file")
        case exc: Throwable             => msg = Some("File corrupted or wrong format")
    }
    Simulation.loadBodies = Vector[Body]() //Reset the temp container
    msg
  }

  //Check if simdata contains a file with the given name
  def checkForOverwrite(name: String) = {
    var found = true
    try {
      val fReader = new FileReader(s"simdata/$name.ssf")
    }
    catch {
      case fnf: FileNotFoundException => found = false
      case exc: Throwable => found = false
    }
    found
  }

  /** Attempt to save the simulation data into a file with the simulation's
    * as it's filename. If unsuccessful, return a corresponding
    * error message. */
  def saveSimData(): Option[String] = {
    var msg: Option[String] = None
    val fName = Simulation.name.get
    //Files are saved in the simdata folder with the .ssf suffix
    val f = new File(s"simdata/$fName.ssf")
    val bData = dataToString

    try {
      val fWriter = new FileWriter(f)
      val bWriter = new BufferedWriter(fWriter)
      try {
        bWriter.write(fName)      //File header is the simulation/file name
        bWriter.newLine()         //It is checked when loading the file to see
                                  //if the file has been corrupted
        for(data <- bData){
          bWriter.write(data)
          bWriter.newLine()
        }
      }
      finally {
        bWriter.close()
        fWriter.close()
      }
    }
    catch {
      case fnf: FileNotFoundException => msg = Some("File not found")
      case ioe: IOException           => msg = Some("Could not write to file")
      case exc: Throwable             => msg = Some("Unexpected exception")
    }
    msg
  }
}
//End of IOHandler
