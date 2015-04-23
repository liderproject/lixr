package eu.liderproject.lixr

import com.twitter.util.Eval
import java.io.File

object Main {
  def loadModelFromFile(fileName : String) = Eval[Model](new File(fileName))

  def main(_args : Array[String]) {
    var args = collection.mutable.Buffer(_args:_*)
    for(a <- args) {
      if(a.startsWith("-D")) {
        val Array(p, v) = a.drop(2).split("=")
        System.setProperty(p, v)
      }
    }
    args = args.filterNot(_.startsWith("-D"))
    if(args.length < 2) {
      System.err.println("Usage: java -jar lixr.jar <model> <file> params...")
      System.exit(-1)
    }
    val gen = new DOMGenerator()
    val model = try {
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
