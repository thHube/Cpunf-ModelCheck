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
package it.unipd.math.atomic.test

import it.unipd.math.atomic._
import it.unipd.math.cpnunf._
import java.io.PrintStream

object CnetGeneratorTest extends App {
  val files      = List("locks.atom", "race.atom", "dining.atom")
  var outfiles   = List("locks.dot",  "race.dot",  "dining.dot")

  for (file <- files) {
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