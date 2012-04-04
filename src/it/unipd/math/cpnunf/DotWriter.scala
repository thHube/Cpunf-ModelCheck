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

import java.io.PrintStream

// -----------------------------------------------------------------------------
// -- Dot writer static object, write to DOT files.
// -----------------------------------------------------------------------------
object DotWriter {
    def write[T <: Net[_ <: Node[_ <: Node[_]], _ <: Node[_ <: Node[_]]]](out: PrintStream, net: T) {
        out.println("digraph PetriNet {")
        
        out.println("// Places")
        for (place <- net.places) {
            out.print("p%d [shape=circle label=\"%s\"".format(place.id, place.name))
            if (place.pos != null)
                out.print(" pos=\"%d,%d!\"".format(place.pos._1, place.pos._2))
            if (place.mark > 0)
                out.print(" color=green")
            out.println("];")
        }
        
        out.println("// Transitions")
        for (transition <- net.transitions) {
            out.print("t%d [shape=box label=\"%s\"".format(transition.id, transition.name))
            if (transition.pos != null)
                out.print(" pos=\"%d,%d!\"".format(transition.pos._1, transition.pos._2))
            out.println("];")
        }
        
        out.println("// Edges")
        for (place <- net.places) {
            for (transition <- place.postset)
                out.println("p%d -> t%d;".format(place.id, transition.id))
        }
        for (transition <- net.transitions) {
            for (place <- transition.postset)
                out.println("t%d -> p%d;".format(transition.id, place.id))
            for (place <- transition.readarcs)
                out.println("p%d -> t%d [dir=none];".format(place.id, transition.id))
        }
        
        out.println("}")
    }
}
