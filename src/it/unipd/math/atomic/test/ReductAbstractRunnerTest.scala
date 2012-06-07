package it.unipd.math.atomic.test

import it.unipd.math.atomic._

object ReductAbstractRunnerTest extends Application {
  val files      = List("simple.atom", "race.atom", "dining.atom")

  for (file <- files){
    val parseTreeBuilder = new ParseTreeBuilder("samples/" + file)
  	val root = parseTreeBuilder.getTree
  	val reduct = new ReductAbstractRunner
  	val red = reduct.run(root)
  	println("File [" + file + "]")
  	Reduct.printReduct(red)
  	println("\n\n")
  }
  
}