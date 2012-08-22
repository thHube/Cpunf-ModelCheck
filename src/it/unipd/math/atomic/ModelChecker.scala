/** 
 *  Copyright (C) 2012  Alberto Franco
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details. 
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.unipd.math.atomic

import java.util.Calendar
import java.io.PrintStream
import it.unipd.math.cpnunf.DotWriter
import it.unipd.math.cpnunf.LLWriter
import it.unipd.math.cpnunf.AspWriter

class ModelChecker(filename:String){
  
  val timeBefore:Long = Calendar.getInstance().getTimeInMillis()
  
  val parseTreeBuilder = new ParseTreeBuilder(filename)
  val root   = parseTreeBuilder.getTree
  val runner = new ReductAbstractRunner
  val net    = new CnetGenerator(runner.run(root)).getGeneratedNet

  // -- Prints model checking informations
  def performCheck(check:Boolean) {
    if (check) {
      val unfold = new AtomicUnfolder(net).unfold()
      val timeAfter:Long = Calendar.getInstance().getTimeInMillis()
      
      AtomicHistory.printModelCheckingInfos(timeAfter - timeBefore)
      if (ModelChecker.asp) {
        println("Generating asp output...")
        AspWriter.write(new PrintStream(filename + ".asp"), unfold)
      }
    }
    
    if (ModelChecker.dot) {
      println("Generating dot output...")
      DotWriter.write(new PrintStream(filename + ".dot"), net)
    }
    if (ModelChecker.ll) {
      println("Generating ll_net output...")
      LLWriter.write(new PrintStream(filename + ".ll_net"), net, true)
    }
  }  
}

// -- Model checker main procedure 
object ModelChecker {
  
  private var dot   = false
  private var asp   = false
  private var ll    = false
  private var check = true
  
  // -- Usage print 
  def printUsage = {
    println( 
    "Usage: modelcheck <options> file.atom \n\n" +
    "Options:\n" +
    "  -help    Print this message\n" +
    "  -dot     Output generated net to graphviz dot\n" +
    "  -ll      Output generated net to ll_net\n" +
    "  -asp     Output generated net to asp, \n" +
    "  -nocheck Just generate the net do not perfom model checking.\n" +
    "           This option disable automatically -asp option.\n")
  }
  
  // -- Print the license of the program 
  def printLicense = {
    print("Cpnunf-ModelChecking Copyright (C) 2012  Alberto Franco, " + 
    "Alessandro Bruni\n" +"This program comes with ABSOLUTELY NO WARRANTY.\n" + 
    "This is free software, and you are welcome to redistribute it\n" + 
    "under certain conditions;\n")
  }
  
  // -- Parse command line options 
  def parseOptions(args:Array[String]):String = {
    var toReturn = args(0)
    for (str <- args) {
      str match {
        case "-help" => printUsage; System.exit(0)
        
        case "-dot"  => dot = true
        case "-ll"   => ll  = true
        case "-asp"  => asp = true
        
        case "-nocheck" => check = false
        
        case _ => toReturn = str
      }
    }
    return toReturn
  }
  
  // -- Application entry point.
  def main(args:Array[String]) {
    printLicense
    it.unipd.math.cpnunf.Unfolder.log = new java.io.PrintStream("modelcheck.log")
    if (args.length < 1) {
      printUsage
    } else {
      val checker = new ModelChecker(parseOptions(args))
      checker.performCheck(check)
    }
    
  }
}