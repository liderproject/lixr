package eu.liderproject.lixr

import eu.liderproject.lixr.models._

object Main {
  def main(args : Array[String]) {
    if(args.length < 2) {
      System.err.println("Usage: java -jar lixr.jar <model> <file> params...")
      System.exit(-1)
    }
    val gen = new DOMGenerator()
    val in = new java.io.FileReader(args(1))
    val model = args(0).toLowerCase match {
      case "metashare" => Metashare
      case "tbx" => new TBX(args(2), new java.io.File(args(3)))
      case "graf" => new GrAF(args(2))
      case _ =>  {
        System.err.println("Bad model: " + args(0))
        System.exit(-1)
        throw new RuntimeException("Unreachable")
      }
    }
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
