package it.unipd.math.atomic.test

import it.unipd.math.atomic._
import it.unipd.math.cpnunf._
import java.io.PrintStream

object CnetGeneratorTest extends Application {
  val files      = List("simple.atom", "race.atom", "dining.atom")
  var outfiles   = List("simple.dot",  "race.dot",  "dining.dot")

  for (file <- files){
    val parseTreeBuilder = new ParseTreeBuilder("samples/" + file)
  	val root   = parseTreeBuilder.getTree
  	val runner = new ReductAbstractRunner
  	val net    = new CnetGenerator(runner.run(root))

    var currentOut = outfiles.head
    outfiles = outfiles.tail
    
    println(net.getGeneratedNet)
    DotWriter.write(new PrintStream(currentOut), net.getGeneratedNet)
    
  }
}