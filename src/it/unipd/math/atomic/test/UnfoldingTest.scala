package it.unipd.math.atomic.test

import it.unipd.math.atomic._
import it.unipd.math.cpnunf._
import java.io.PrintStream

object UnfoldingTest extends App {
  val files      = List("atomic_atomic.atom") // , "race.atom", "dining.atom")
  var outfiles   = List("atomic",  "race",  "dining")

  Unfolder.log = System.out// new PrintStream("out.log")
  
  for (file <- files) {
    val parseTreeBuilder = new ParseTreeBuilder("samples/" + file)
  	val root   = parseTreeBuilder.getTree
  	val runner = new ReductAbstractRunner
  	val net    = new CnetGenerator(runner.run(root)).getGeneratedNet
    // println(net.getGeneratedNet)
        
    // val unfold = new Unfolder(net).unfold()
  	val unfold = new AtomicUnfolder(net).unfold()
  	
  	/*
  	for (h1 <- unfold.histories) h1 match {
      case h:AtomicHistory => {
        println ("Psi: " + h.psi)
        println ("Sigma:" + h.sigma + "\n")
        
      }
      
      case _ => // -- Do nothing  
    }
  	*/
  	
    // -- Print data 
    var currentOut = outfiles.head
    outfiles = outfiles.tail
    
    DotWriter.write(new PrintStream(currentOut + ".unf.dot"), unfold)
    DotWriter.write(new PrintStream(currentOut + ".dot"), net)
    LLWriter.write(new PrintStream(currentOut + ".ll_net"), net, false)
    Unfolder.log.println("\n ---- \n")
  }
}