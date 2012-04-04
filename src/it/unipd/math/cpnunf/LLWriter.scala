/** 
 *  Copyright (C) 2012  Alessandro Bruni
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
package it.unipd.math.cpnunf

import java.io._
// -----------------------------------------------------------------------------
// -- LLWriter static class, writes to LL files
// -----------------------------------------------------------------------------
object LLWriter {
    def write[T <: Net[_ <: Node[_ <: Node[_]], _ <: Node[_ <: Node[_]]]](out: PrintStream, net: T, histinf: Boolean) {
        out.println("PEP")
        out.println("PetriBox")
        out.println("FORMAT_N2")
        
        out.println("PL")
        for (place <- net.places) {
            out.print(place.id)
            out.print(""""%s"""".format(place.name))
            if (place.pos != null)
                out.print("%d@%d".format(place.pos._1, place.pos._2))
            if (place.mark > 0)
                out.print("eM%1$dm%1$d".format(place.mark))
            out.println()
        }
        
        out.println("TR")
        for (transition <- net.transitions) {
            out.print(transition.id)
            out.print(""""%s"""".format(transition.name))
            if (transition.pos != null)
                out.print(transition.pos._1 + "@" + transition.pos._2)
            out.println()
        }
        
        out.println("TP")
        for (transition <- net.transitions)
            for (place <- transition.postset)
                out.println("%d<%d".format(transition.id, place.id))
        
        out.println("PT")
        for (place <- net.places)
            for (transition <- place.postset)
                out.println("%d>%d".format(place.id, transition.id))
        
        out.println("RA")
        for (transition <- net.transitions)
            for (place <- transition.readarcs)
                out.println("%d<%d".format(place.id, place.id))
        if (histinf) {
            out.println("TX")
            val unf = net.asInstanceOf[Unfolding]
            for (h <- unf.histories) {
                out.println(h)
            }
        }
    }
}
