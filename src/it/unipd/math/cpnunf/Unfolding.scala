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

import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue

// -----------------------------------------------------------------------------
// -- Class EnrichedCondition. Used to build a CEP. 
// -----------------------------------------------------------------------------
class EnrichedCondition(val c: Condition, val h: History)
    extends Ordered[EnrichedCondition] {
  
    // -- Concurrent places 
    var co = Set[EnrichedCondition]()
    
    // -- Checks if two enriched conditions are concurrent.
    def concurrentWith(that: EnrichedCondition): Boolean =
        this != that && (this.co.contains(that))
        
    // -- Use subsumption, lazy approach.
    def sub = h.subsumed
    
    // -- Causal relationship
    def causal = {
        if (c.pre == h.event) this
        else h.read.find(c.pre == _.h.event).get
    }
    
    // -- Find out if this enriched condition is causal for condition c 
    def isCausal: Boolean = this.h causalFor this.c
    
    // -- Print and compare utility methods.
    override def toString = "<%s,h%s[%s]>".format(c.name, h.id, h.event.name)
    def compare(that: EnrichedCondition) = this.co.size compare that.co.size
}

// -----------------------------------------------------------------------------
// -- Singleton Color. For depth first searching 
// -----------------------------------------------------------------------------
object Color {
    val WHITE = 0
    val BLACK = 1
    val GRAY  = 2
}

// -----------------------------------------------------------------------------
// -- Class condition. Events and Conditions are instance of transitions and 
// -- places in the unfolding. Implicitly is defined mapping function from 
// -- conditions to places (through image)
// -----------------------------------------------------------------------------
class Condition(val image: Place, val pre: Event)
        extends Node[Event](Condition.get_next_id) {
  
    // -- Name of the condition
    def name = "c%d (%s)".format(id, image.name)
    
    // -- Set this preset and parent post set.
    preset = Set[Event](pre)
    pre.postset += this

    def compare(that: Condition) = this.id compare that.id
    override def toString = name 
}

// -----------------------------------------------------------------------------
// -- Static data for conditions
// -----------------------------------------------------------------------------
object Condition {
    var next_id = 0 //< -- Next available condition id
    
    // -- Generate next id. 
    def get_next_id = {
        next_id += 1
        next_id
    }
}

// -----------------------------------------------------------------------------
// -- Class Event. Same as in condition class.
// -----------------------------------------------------------------------------
class Event(val image: Transition, _preset: Set[Condition])
        extends Node[Condition](Event.get_next_id) {
  
    // -- Sets name of the event. 
    def name = if (image != null) "e%d (%s)".format(id, image.name) else "root_event"
    
    // -- Sets event color 
    var color = Color.WHITE
    this.preset = _preset
    
    // -- Set postset only if there is a transition. 
    if (image != null) {
      postset = image.postset.map(new Condition(_, this))
    }
    
    // -- Add to transition preimage this node.
    if (image != null)
        image.addPreimage(this)
    
    // -- Remove a link to current node. 
    def unlink() = image.removePreimage(this)
    
    // -- Alternative constructor with preset and node context.
    def this(image: Transition, _preset: Set[Condition], _readarcs: Set[Condition]) {
        this(image, _preset)
        this.readarcs = _readarcs
    }

    // -- Compare mehtod definition 
    def compare(that: Event) = this.id compare that.id

    def preasconfl = preset.map(
            c => c.postset union c.readarcs union c.preset
        ).reduceOption(_ union _).getOrElse(Set())

    def postasconfl = (
            postset.map(_.postset) union preset.map(_.postset) union readarcs.map(_.postset)
        ).reduceOption(_ union _).getOrElse(Set())
}

// -----------------------------------------------------------------------------
// -- Event class static data.
// -----------------------------------------------------------------------------
object Event {
    var next_id = 0 //< -- Next available condition id
    
    // -- Generate next id.
    def get_next_id = {
        next_id += 1
        next_id
    }
}

// -----------------------------------------------------------------------------
// -- History Class static data and methods.  
// -----------------------------------------------------------------------------
object History {
    
    // -- Travese given history
    def traverse(h: History, f: History => Unit) {
        // -- Depth first search
        def dfs(h: History) {
            if (h.color == Color.WHITE) {
                h.color = Color.BLACK
                f(h)
                for (s <- h.consumed) dfs(s.h)
                for (s <- h.read) dfs(s.h)
            }
        }
        
        // -- Reset colors to white.
        def clear_color(h: History) {
            if (h.color != Color.WHITE) {
                h.color = Color.WHITE
                for (s <- h.consumed) clear_color(s.h)
                for (s <- h.read) clear_color(s.h)
            }
        }
        
        dfs(h)          //< -- Depth first searching
        clear_color(h)  //< -- Clear back all colors
    }
    
    // -- Next available id and next id generator
    var next_id = 0
    def get_next_id = {
        next_id += 1
        next_id
    }
    
    var count_comp = 0
    var count_comp_equal = 0
    
    // -- Height ordering, keeps ordered with height elements that are inside 
    // -- this data structure. 
    val heightOrdering = new Ordering[History] {
        def compare(x: History, y: History) = x.height compare y.height 
    }
    
    // -- Orderer histories with height and events.
    val heightEventOrdering = new Ordering[History] {
        // -- Compare function for events and height
        def compare(y: History, x: History) = {
            val comp = x.height compare y.height
            if (comp == 0)
                x.event.id compare y.event.id
            else comp
        }
    }
}

// -----------------------------------------------------------------------------
// -- History class.
// -----------------------------------------------------------------------------
class History(var event: Event, val consumed: Set[EnrichedCondition], val read: Set[EnrichedCondition])
        extends Ordered[History] {
  
	// -- Each history has an id.
    val id = History.get_next_id
    
    // -- Calculate history height
    val height: Int = (consumed ++ read).map(_.h.height).reduceOption(scala.math.max(_, _)).getOrElse(-1) + 1

    // -- Calculate the set of concurrent places and subsumed
    val (concurrent: Set[EnrichedCondition], subsumed: Set[EnrichedCondition]) = {
      
        val read_sub = event.readarcs.map(s => new EnrichedCondition(s, this)) 
        val post_co = (event.postset.map(s => new EnrichedCondition(s, this)) ++ read_sub)
        
        // -- Concurrent places. 
        val concurrent = (((consumed ++ read).map(_.co).reduceOption(_ intersect _).getOrElse(Set())).filter(
            σ => ((σ.sub -- consumed).map(_.causal) intersect consumed).isEmpty)
            ++ post_co
            ++ this.read)
            
        // -- Set concurrent places. 
        for (s <- concurrent) {
            s.co ++= post_co
        }
        
        // -- Filter concurrent places 
        for (s <- post_co) {
            s.co = concurrent.filter(_ != s)
        }
        
        // -- Calculate subsumed places  
        val subsumed = (((consumed ++ read).map(_.sub).reduceOption(_ ++ _).getOrElse(Set())
                intersect concurrent) ++ read_sub)
        (concurrent, subsumed)
    }
    
    // -- Some debug printing 
    if (Unfolder.DEBUG) {
        Unfolder.log.println("sub[h%s[%s]] = %s".format(id, event.name, subsumed))
        Unfolder.log.println("consumed: %s, read: %s".format(consumed, read))
    }
    
    // -- Default color is white
    var color = Color.WHITE
    
    // -- Set size, marking and parikh
    val (size, marking, parikh) = {
        var sz = 0
        var prk = new Parikh()
        var munf = new Array[Int](PetriNet.place_max_id + 1)
        
        // -- Traverse the history with the given function. Histories are 
        // -- traversed in a depth-first fashion
        History.traverse(this, h => {
            sz += 1
            
            if (h.event.image != null) {
              prk(h.event.image.id) += 1
            }
            
            for (c <- h.event.preset) {
                munf(c.image.id) -= 1
            }
            
            for (c <- h.event.postset) {
                munf(c.image.id) += 1
            }
        })
        
        // -- Initial marking of history
        var mark = Set[Int]()
        
        // -- Add elements to marking if are mapped.    
        for (i <- 0 until munf.length) {
            if (munf(i) > 0)
                mark += i
        }
        
        (sz, mark, prk)
    }
    
    // -- Unreachable code, thought it was intended for debugging.
    if (false && Unfolder.DEBUG) {
        Unfolder.log.println("h%s[%s] -> %s".format(id, event.name, marking))
    }
    
    // -- Tells if the history is causal for the condition
    def causalFor(c: Condition): Boolean = c.pre == this.event
    
    // -- Compare size of two histories
    def sizeCompare(that: History): Int = that.size compare this.size
    
    // -- Compare function.  
    def parikhCompare(that: History): Int = {
        // -- First compare size
        val comp = that.size compare this.size
        
        // -- If size are equal use parikh vector to compare
        if (comp != 0) comp
        else that.parikh compare this.parikh
    }
    
    // -- Foata compare is even more fine-grained than parikh vector comparison
    // -- here it is not utilized. Create a total order from histories. 
    def foataCompare(that: History): Int = {
        val comp = that.size compare this.size
        if (comp != 0) comp
        else {
            val comp = that.parikh compare this.parikh
            if (comp == 0) {
                val q1 = PriorityQueue[History]()(History.heightEventOrdering)
                val q2 = PriorityQueue[History]()(History.heightEventOrdering)
                History.traverse(that, q1.enqueue(_))
                History.traverse(this, q2.enqueue(_))
                while(!q1.isEmpty && !q2.isEmpty) {
                    val h1 = q1.dequeue()
                    val h2 = q2.dequeue()
                    val comp = History.heightEventOrdering.compare(h2, h1)
                    if (comp != 0)
                        return comp
                }
                if (!q1.isEmpty)
                    return 1
                if (!q2.isEmpty)
                    return -1
                Unfolder.log.println("Foata comparison failed: this should be a total order!")
                return 0
            } else comp
        }
    }
    
    // -- Compare function. This is implemented in order to implement Ordered[K]
    // -- trait. Use parikhCompare as comparison function. 
    def compare(that: History): Int = {
        History.count_comp += 1 //< -- Benchmarking
        this parikhCompare that //< -- Actual comparison
    }
    
    override def toString = "h%d[%s] |%d| = *%s _%s".format(id, event.name, size, consumed, read)
}

// -----------------------------------------------------------------------------
// -- Class Parikh. Used for fine-grained comparison between history. Simple 
// -- set cardinality comparison is not adeguate
// -----------------------------------------------------------------------------
class Parikh() {
    var vector = new Array[Int](PetriNet.trans_max_id + 1)
    def apply(i: Int) = vector(i)
    def update(i: Int, v: Int) = vector(i) = v
    
    // -- Comparison using parikh vector.
    def compare(that: Parikh): Int = {
        // -- Loop through the vector.
        for (i <- 0 to PetriNet.trans_max_id) {
            // -- Compare each element of the vector
            val comp = this(i) compare that(i)
            if (comp != 0)
                return comp
        }
        // -- Parikh vector are equals.
        return 0
    }
}

// -----------------------------------------------------------------------------
// -- class Unfolding, represent an unfolded net.
// -- TODO: extends this? 
// -----------------------------------------------------------------------------
class Unfolding(net: PetriNet) extends Net[Condition, Event] {
    
    // -- Set a root for the net.
    val root_event: Event = new Event(null, Set())
    
    // -- Initial marking
    m0 = net.m0.map(new Condition(_, root_event))
    
    // -- Add all places in the marking to current conditions. 
    places ++= m0
    
    // -- Add a root for histories
    val root_history: History = new History(root_event, Set(), Set())
    
    // -- Do not care about root event.
    transitions -= root_event

    // -- Markings, histories and cutoffs.
    val markings = Map[Set[Int], Set[History]]()
    var histories = Set[History]()
    var cutoffs = List[History]()
    
    // -- Modifier for histories, append an history at the list. Also update 
    // -- markings with the newly inserted history.
    def addHistory(h: History) {
        histories += h
        var hists = markings.getOrElse(h.marking, Set[History]()) + h
        markings += ((h.marking, hists))
        if (!transitions.contains(h.event)) {
            transitions += (h.event)
            for (pl <- h.event.preset)
                pl.addPost(h.event)
            for (pl <- h.event.readarcs)
                pl.addRead(h.event)
            places ++= h.event.postset
        }
    }
    
    // -- Add a cutoff to list.
    def addCutoff(h: History) = cutoffs ::= h
    
    // -- Test if the given history is a cutoff. 
    def isCutoff(h: History): Boolean = {
        // -- Get the set of histories that refer to the marking.
        val compare_to = markings.getOrElse(h.marking, Set())
        for (h2 <- compare_to) {
            if (h2 > h)
                return true
        }
        false
    }
}
