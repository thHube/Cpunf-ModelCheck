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
// -- AspWriter static object. Write to asp files.
// -----------------------------------------------------------------------------
object AspWriter {
    def write(out: PrintStream, net: Unfolding) {
        for (condition <- net.places) {
            out.println("b(%d).".format(condition.id))
            out.println("s(%d,%d).".format(condition.id, condition.image.id))
        }
        
        for (event <- net.transitions) {
            out.println("e(%d).".format(event.id))
            out.println("t(%d,%d).".format(event.id, event.image.id))
        }
        
        for (event <- net.transitions)
            for (condition <- event.postset)
                out.println("post(%d,%d).".format(condition.id, condition.id))
        
        for (condition <- net.places)
            for (event <- condition.postset)
                out.println("pre(%d,%d).".format(event.id, condition.id))
        
        for (event <- net.transitions)
            for (condition <- event.readarcs)
                out.println("read(%d,%d).".format(event.id, condition.id))
        
        for (event <- net.cutoffs.map(_.event)) {
            out.println("e(%d).".format(event.id))
            out.println("cutoff(%d).".format(event.id))
            out.println("t(%d,%d).".format(event.id, event.image.id))
            for (condition <- event.preset)
                out.println("pre(%d,%d).".format(event.id, condition.id))
            for (condition <- event.postset)
                out.println("post(%d,%d).".format(event.id, condition.id))
        }
    }
}
