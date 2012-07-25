package it.unipd.math.atomic

class ModelChecker(filename:String){
  
  val parseTreeBuilder = new ParseTreeBuilder(filename)
  val root   = parseTreeBuilder.getTree
  val runner = new ReductAbstractRunner
  val net    = new CnetGenerator(runner.run(root)).getGeneratedNet
  val unfold = new AtomicUnfolder(net).unfold()

  // -- Prints model checking informations
  def performCheck() {
    AtomicHistory.printModelCheckingInfos()
  }
  
}

object ModelChecker {
  
  def printUsage = {
    println( "Cpnunf-ModelChecking Copyright (C) 2012  Alberto Franco, " + 
    "Alessandro Bruni\n" + 
    "usage: modelcheck file.atom \n\n" +
    "This program comes with ABSOLUTELY NO WARRANTY.\n" + 
    "This is free software, and you are welcome to redistribute it\n" + 
    "under certain conditions;")
  }
  
  def main(args:Array[String]) {
    it.unipd.math.cpnunf.Unfolder.log = new java.io.PrintStream("modelcheck.log")
    if (args.length < 1) {
      printUsage
    } else {
      val checker = new ModelChecker(args(0))
      checker.performCheck()
    }
    
  }
}