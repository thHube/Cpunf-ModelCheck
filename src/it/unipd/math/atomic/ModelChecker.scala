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

class ModelChecker(filename:String){
  
  val timeBefore:Long = Calendar.getInstance().getTimeInMillis()
  
  val parseTreeBuilder = new ParseTreeBuilder(filename)
  val root   = parseTreeBuilder.getTree
  val runner = new ReductAbstractRunner
  val net    = new CnetGenerator(runner.run(root)).getGeneratedNet
  val unfold = new AtomicUnfolder(net).unfold()

  val timeAfter:Long = Calendar.getInstance().getTimeInMillis()
  
  // -- Prints model checking informations
  def performCheck() {
    AtomicHistory.printModelCheckingInfos(timeAfter - timeBefore)
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