package it.unipd.math.atomic.test

import it.unipd.math.atomic._
import it.unipd.math.cpnunf._
import java.io.PrintStream

object UnfoldingTest extends App {
  val files      = List("simple.atom", "race.atom", "dining.atom")
  var outfiles   = List("simple",  "race",  "dining")

  Unfolder.log = System.out// new PrintStream("out.log")
  
  for (file <- files) {
    val parseTreeBuilder = new ParseTreeBuilder("samples/" + file)
  	val root   = parseTreeBuilder.getTree
  	val runner = new ReductAbstractRunner
  	val net    = new CnetGenerator(runner.run(root))
    // println(net.getGeneratedNet)
        
    val unfold = new Unfolder(net.getGeneratedNet).unfold()

    var currentOut = outfiles.head
    outfiles = outfiles.tail
    
    DotWriter.write(new PrintStream(currentOut + ".dot"), net.getGeneratedNet)
    LLWriter.write(new PrintStream(currentOut + ".ll_net"), net.getGeneratedNet, false)
    Unfolder.log.println("\n ---- \n")
  }
}