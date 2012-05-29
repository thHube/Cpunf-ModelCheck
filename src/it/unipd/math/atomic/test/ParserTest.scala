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

import it.math.unipd.atomic.Parser
import it.math.unipd.atomic.Scanner
import it.math.unipd.atomic.ParseTreeBuilder

object ParserTest {
  
  def main(args:Array[String]) {
    val files = List("simple.atom", "race.atom")
    for (file <- files) {
    	val parseTreeBuilder = new ParseTreeBuilder("samples/" + file)
    	val root = parseTreeBuilder.getTree
    	println(root)
    }
  }
  
}