package eu.liderproject.lixr

import com.twitter.util.Eval
import eu.liderproject.lixr.models._
import java.io.File

object Main {
  def loadModelFromFile(fileName : String) = Eval[Model](new File(fileName))

  def main(args : Array[String]) {
    if(args.length < 2) {
      System.err.println("Usage: java -jar lixr.jar <model> <file> params...")
      System.exit(-1)
    }
    val gen = new DOMGenerator()
    val model = args(0).toLowerCase match {
      case "metashare" => Metashare
      case "tbx" => new TBX(args(2), new java.io.File(args(3)))
      case "graf" => new GrAF(args(2))
      case _ =>  {
        try {
          loadModelFromFile(args(0))
        } catch {
          case x : Exception =>
            x.printStackTrace()
            System.err.println()
            System.err.println("Could not load %s as \"%s\"" format (args(0), 
              x.getMessage()))
            System.exit(-1)
            null
        }
      }
    }
    val in = new java.io.FileReader(args(1))
    val results = gen.read(in,model)
    
    for(result <- results) {
      result match {
        case t : TripleResult =>
          println(t.toString)
        case _ =>
      }
    }


  }
}
