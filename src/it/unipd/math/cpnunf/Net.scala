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

import scala.collection.immutable.IntMap

// -----------------------------------------------------------------------------
// -- Abstract class Node.
// -----------------------------------------------------------------------------
abstract class Node[Other <: Node[_]](val id: Int) {
    // -- Neighbour nodes informations ----------------------------------------- 
    var preset: Set[Other] = Set[Other]()
    var postset: Set[Other] = Set[Other]()
    var readarcs: Set[Other] = Set[Other]()
    
    // -- Additional informations ----------------------------------------------
    def name: String
    def pos: (Int, Int) = null
    def mark: Int = 0

    // -- Modifiers for neighbours node ---------------------------------------- 
    def addPre(o: Other) = preset += o
    def addPost(o: Other) = postset += o
    def addRead(o: Other) = readarcs += o

    // -- Print current node to a string ---------------------------------------
    override def toString = this.getClass.getSimpleName + "(" + id + ": " + 
            name + "," + 
            " pre: " + (for (n <- preset) yield n.id) +
            " post: " + (for (n <- postset) yield n.id) +
            " read: " + (for (n <- readarcs) yield n.id) + ")"
}

// -----------------------------------------------------------------------------
// -- Abstact class Net
// -----------------------------------------------------------------------------
abstract class Net[P <: Node[_], T <: Node[_]] {
    // -- Nodes informations ---------------------------------------------------
    var places = Set[P]()
    var transitions = Set[T]()
    
    // -- Initial marking ------------------------------------------------------ 
    var m0 = Set[P]()
    
    // -- Print to string current net ------------------------------------------
    override def toString = 
      "Places: " + places.toString + "\nTransitions: " + transitions.toString        
}

// -----------------------------------------------------------------------------
// -- Class place. Represent a petri net place.  
// -----------------------------------------------------------------------------
class Place(id: Int, val name: String, override val pos: (Int, Int), args: Map[String, Any])
        extends Node[Transition](id) {
    // -- Update max id 
    if (id > PetriNet.place_max_id) {
      PetriNet.place_max_id = id
    }
    
    // -- Read from parameters initial marking
    override val mark = try { 
      args("init-mark").asInstanceOf[Int] 
    } catch { 
      case _: NoSuchElementException => 0 
    }
    
    // -- Conditions preimage
    var preimage = Set[Condition]()

    // -- Modifier for preimage
    def addPreimage(c: Condition) = preimage += c
    
    // -- To string method
    override def toString = name
}

// -----------------------------------------------------------------------------
// -- Class Transition. Represent a petri net transition.
// -----------------------------------------------------------------------------
class Transition(id: Int, val name: String, override val pos: (Int, Int), args: Map[String, Any])
        extends Node[Place](id) {
    
    // -- Update transitions id 
    if (id > PetriNet.trans_max_id) {
      PetriNet.trans_max_id = id
    }
    
    // -- Events preimage 
    var preimage = Set[Event]()

    // -- Add a preimage 
    def addPreimage(e: Event) = preimage += e
    
    // -- Remove a preimage
    def removePreimage(e: Event) = preimage -= e
    
    // -- To string method
    override def toString = name
}

// -----------------------------------------------------------------------------
// -- Petri net singleton object holds static informations about petri nets.
// -----------------------------------------------------------------------------
object PetriNet {
    var place_max_id = 0 //< -- Current max id for places 
    var trans_max_id = 0 //< -- Current max id for transitions
}

// -----------------------------------------------------------------------------
// -- Class PetriNet. Represent a contextual petri net. Support read arcs. 
// -----------------------------------------------------------------------------
class PetriNet extends Net[Place, Transition] {
  
    // -------------------------------------------------------------------------
    // -- Non-default constructor. Create a petri net starting from (P, T, F, C) 
    // -- and assign an empty marking.
    // -- '_places' and '_transitions' are P and T. 'tr_pl' is set of 
    // -- connections s.t. (t, p) with t \in T and p \in P. 'pl_tr' is the 
    // -- opposite. 'ra' stands for read arcs and its C. 
    // -------------------------------------------------------------------------
    def this(_places: List[(Place, Map[String, Any])], _transitions: List[(Transition, Map[String, Any])],
        tr_pl: List[(Int, Int)], pl_tr: List[(Int, Int)], ra: List[(Int, Int)]) {
      
        // -- Build current object with delegation 
        this()
        
        // -- Parse all places  
        for ((place, opts) <- _places) {
            places += (place)
            if (opts.contains("entry") || opts.contains("init-mark"))
                m0 += place
        }
        
        // -- Parse transitions 
        for ((transition, opts) <- _transitions) {
            transitions += (transition)
        }
        
        // -- Parse (p, t) \in F 
        for ((a, b) <- pl_tr) {
            val p = places.find(_.id == a).get
            val t = transitions.find(_.id == b).get
            p.addPost(t)
            t.addPre(p)
        }
        
        // -- Parse (t, p) \in F
        for ((a, b) <- tr_pl) {
            val p = places.find(_.id == b).get
            val t = transitions.find(_.id == a).get
            p.addPre(t)
            t.addPost(p)
        }
        
        // -- Parse context.
        if (ra != null)
            for ((tr, pl) <- ra) {
                val p = places.find(_.id == pl).get
                val t = transitions.find(_.id == tr).get
                p.addRead(t)
                t.addRead(p)
            }
    }
}
