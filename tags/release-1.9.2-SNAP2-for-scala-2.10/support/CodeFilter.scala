import scala.io.Source
import java.io.File
import java.io.PrintWriter

//
// This is a simple filter to remove code blocks that
// need to be omitted for a particular condition.  It looks
// for the beginning of code blocks identified by a line
// consisting of "#ifndef Label", where Label identifies blocks
// that need to be removed, and then deletes lines until it
// encounters a line consisting of "#endif".
//
// Usage: scala CodeFilter -Dlabel infile outfile
//
// E.g. if the code consists of a file hello.scala.preprocess:
//
//    def main(args: Array[String]) {
//      println("hello world")
// #ifndef SCALA_2_9
//      println("this is not scala 2.9")
// #endif
//    }
//
// Then this command:
//
//  scala CodeFilter -DSCALA_2_9 hello.scala.preprocess hello.scala
//
// would create a file hello.scala containing:
//
//    def main(args: Array[String]) {
//      println("hello world")
//    }
//
// And this command
//
//  scala CodeFilter -Dxxx hello.scala.preprocess hello.scala
//
// would create a file hello.scala containing:
//
//    def main(args: Array[String]) {
//      println("hello world")
//      println("this is not scala 2.9")
//    }
//
// As long as the -D argument is followed by anything other than
// "SCALA_2_9", then the block is not deleted.
//
object CodeFilter {

  def main(args: Array[String]) {
    println("CodeFilter " + args.mkString(" "))
    if (args.size != 3) usage()

    val labelDef = args(0)
    val infile = new File(args(1))
    val outfile = new File(args(2))

    if (!labelDef.startsWith("-D")) usage()

    val label = labelDef.drop(2)

    createOutDir(outfile)
    filterFile(label, infile, outfile)
  }

  def createOutDir(outfile: File) {
    val outdir = new File(outfile.getPath.dropRight(outfile.getName.size))
    outdir.mkdirs()
  }

  def filterFile(label: String, infile: File, outfile: File) {
    val in = Source.fromFile(infile)
    val out = new PrintWriter(outfile.getPath)

    var dontPrint = false

    for (line <- in.getLines()) {
      if (line.trim.startsWith("#ifndef")) {
        if (line.trim == "#ifndef " + label)
          dontPrint = true
      }
      else if (line.trim == "#endif")
        dontPrint = false
      else if (!dontPrint)
        out.println(line)
    }
    out.close()
  }

  def usage() {
    System.err.println("usage: scala CodeFilter -Dlabel infile outfile")
    sys.exit(1)
  }
}

