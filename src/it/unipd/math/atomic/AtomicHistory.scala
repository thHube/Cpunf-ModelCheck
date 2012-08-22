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

import scala.collection.mutable.BitSet
import scala.collection.mutable.Map
import it.unipd.math.cpnunf._

// -----------------------------------------------------------------------------
// -- Sigma function class, an utility class to represent the sigma set defined
// -- in chapter 5: Finite Prefix Calculation
// -----------------------------------------------------------------------------
class SigmaFunction {
  // -- {i | \exists e.f(e') = I \in A_i : s \in context(e)}
  val readFirst    :BitSet = BitSet()
  // -- {i | \exists e.f(e') = I \in A_i : s \in post(e)}
  var writeFirst   :BitSet = BitSet()
  // -- {i | \exists e.f(e') = I \in A_i, \exists e_B1 .. e_Bk, k >= 1, 
  // -- \forall j . e_Bj \notin A_i, e_I /^ e_B1 /^ ... /^ e_Bk and s \in context(e_Bk)}
  val readContinue :BitSet = BitSet()
  // -- {i | \exists e.f(e') = I \in A_i, \exists e_B1 .. e_Bk, k >= 1, 
  // -- \forall j . e_Bj \notin A_i, e_I /^ e_B1 /^ ... /^ e_Bk and s \in post(e_Bk)}
  val writeContinue:BitSet = BitSet()
 
  // -- Exists function in the whole 
  def exists(fn : Int => Boolean):Boolean = 
    readFirst.exists(fn)    || writeFirst.exists(fn) || 
    readContinue.exists(fn) || writeContinue.exists(fn)
  
  def existsWrite(fn:Int => Boolean):Boolean = 
    writeFirst.exists(fn) || writeContinue.exists(fn)
      
  // -- Update preset for read contiunue
  def updatePreRead(sigma:SigmaFunction, as:Int) = {
    for (item <- sigma.readFirst ++ sigma.writeFirst) {
      if (item != as) { readContinue += item }
    }
    // println("Updated pre condition #3 for atomic => " + as)
  }
    
  // -- Update the context for read continue
  def updateContextRead(sigma:SigmaFunction, as:Int) = {
    for (item <- sigma.writeFirst) {
      if (item != as) { readContinue += item }
    }
    // println("Updated context condition #3 for atomic => " + as)
  }
  
  // -- Update preset for write contiunue
  def updatePreWrite(sigma:SigmaFunction, as:Int, codeLine:Int) = {
    for (item <- sigma.readFirst ++ sigma.writeFirst) {
      if (item != as) { 
        writeContinue += item
        
        AtomicHistory.totalPsi += item
        AtomicHistory.breakCouples += ((AtomicHistory.atomicLineMap(item), codeLine)) 
      }
    }
    // println("Updated pre condition #4 for atomic => " + as)
  }
    
  // -- Update the context for write continue
  def updateContextWrite(sigma:SigmaFunction, as:Int, codeLine:Int) = {
    for (item <- sigma.writeFirst) {
      if (item != as) { 
        writeContinue += item
        
        AtomicHistory.totalPsi += item
        AtomicHistory.breakCouples += ((AtomicHistory.atomicLineMap(item), codeLine))
      }
    }
    // println("Updated context condition #4 for atomic => " + as)
  }
  
  // -- Comparison operator 
  def ==(that:SigmaFunction):Boolean = {
    readFirst  == that.readFirst   && 
    writeFirst == that.writeFirst  && 
    readContinue  == that.readContinue  && 
    writeContinue == that.writeContinue
  }
  
  // -- To string method
  override def toString = "[1r: "  + readFirst + 
  	                      ", 1w: " + writeFirst + 
  	                      ", 2r: " + readContinue + 
  	                      ", 2w: " + writeContinue + "]" 
}


// -----------------------------------------------------------------------------
// -- AtomicHistory class: represent an history decorated with atomic
// -- violation informations as defined
// -----------------------------------------------------------------------------
class AtomicHistory(var evt:Event, override val consumed:Set[EnrichedCondition],
    override val read:Set[EnrichedCondition]) extends History(evt, consumed, read) {
  
  private val sigma:Map[Condition, SigmaFunction] = Map()
  private val psi  :BitSet = BitSet()

  // -- Update the psi set with atomicity breaks
  private def updatePsi(event:Event, as:Int, codeLine:Int) {
    if (as == -1) return 
    
    for (se <- event.preset) sigma.get(se) match {
      case Some(sgm) => {
        // println("Checking atomic section #" + as +": " + sgm)
        if (sgm.readContinue.contains(as) || sgm.writeContinue.contains(as)) {
          println("Atomic section #" + as + " is broken")
          psi += as
          AtomicHistory.totalPsi += as
          AtomicHistory.breakCouples += ((AtomicHistory.atomicLineMap(as), codeLine))  
        }
      }
      case None => 
    }
  }
  
  // -- Update sigma function  
  private def updateSigma(event:Event, as:Int, codeLine:Int) {
    // -- Update context first
    for (s <- event.readarcs) {
      val sgm = sigma.getOrElse(s, new SigmaFunction)
      // -- Condition 1 of context update is satisfied
      if (as != -1) {
        sgm.readFirst += as
      }
      
      // -- Condition 3 of context update is satisfied
      for (se <- event.preset) sigma.get(se) match {
        case Some(sgmFn) => sgm.updatePreRead(sgmFn, as)
        case None => // -- Skip
      }
      for (se <- event.readarcs) sigma.get(se) match {
        case Some(sgmFn) => sgm.updateContextRead(sgmFn, as)
        case None =>
      }
      
      // -- Update sigma set
      sigma += (s -> sgm)
    }
    
    // -- Update post set.
    for (s <- event.postset) {
      val sgm = new SigmaFunction
      // -- Condition 2 of postset update is satisfied
      if (as != -1) {
        sgm.writeFirst += as
      }
      
      // -- Condition four of update is satisfied 
      for (se <- event.preset) sigma.get(se) match {
        case Some (sgmFn) => sgm.updatePreWrite(sgmFn, as, codeLine)
        case None => 
      }
      for (se <- event.readarcs) sigma.get(se) match {
        case Some(sgmFn) => sgm.updateContextWrite(sgmFn, as, codeLine)
        case None =>
      }
      
      sigma += (s -> sgm)
    }
  }
  
  var contribution:Set[History] = Set()
    
  // -- Update the marking traversing the history. 
  // -- TODO: improve this function by doing a single pass over the net. 
  def getMarking(ep:EnrichedCondition) = {
    // -- Add first all of the history in the contribution
    History.traverse(ep.h, x => {
      contribution += x
    })
    
    // -- Remove all of the histories that does not matters 
    History.traverse(ep.h, x => {
      for (ep <- consumed) contribution -= ep.h
      for (ep <- read)     contribution -= ep.h
    })
  }
  
  // -- Recalculate sigma and psi value for current history 
  private def recalcSigmaAndPsi = {
    for (h <- contribution) h match {
        case ah:AtomicHistory => {
          sigma ++= ah.sigma
          psi   ++= ah.psi
        }
        case _ => println("skippin' non atomic history")
      }
  } 
  
  
  // -- Add an event to 
  private def addEvent(event:Event) {
    
    var codeLine = 0
    
    // -- Support method to use in getting run-time atomic section id   
    def getRuntimeAtomicSection(event:Event):Int = event.image match {
      case p:Atomic => {
        codeLine = p.codeLine
        if (p.atomicSection != -1) {
          var as:Int = -1
          event.preset.foreach(c => c.image match {
            case t:Atomic => {
              if (t.atomicSection == p.atomicSection) {
                for (e1 <- c.preset) e1.image match {
                  case p1:Atomic => {
                    if (p1.atomicSection == p.atomicSection) {
                      p.runtimeAtomic = p1.runtimeAtomic 
                    } else {
                      p.runtimeAtomic = AtomicHistory.getNextRuntimeId()
                      AtomicHistory.atomicLineMap += (p.runtimeAtomic -> p.codeLine)
                    }
                    as = p.runtimeAtomic
                  }
                  case  _ => {
                    p.runtimeAtomic = AtomicHistory.getNextRuntimeId()
                    as = p.runtimeAtomic
                    AtomicHistory.atomicLineMap += (p.runtimeAtomic -> p.codeLine)
                  }
                }
              } else {
                as = -1
              }             
            } 
            case _ => as -1
          })
          return as
        } else {
          return -1
        }
      }
      case _ => return -1
    }
    
    val as:Int = getRuntimeAtomicSection(event)
    
    // -- TODO find histories that contribute to this atomic
    for (ep <- consumed ++ read) getMarking(ep)
    
    recalcSigmaAndPsi
    updateSigma(event, as, codeLine)
    updatePsi(event, as, codeLine)
    
    // println(contribution)
    // println(AtomicHistory.psi)
    // println(AtomicHistory.sigma)
    // println(" ___ ")
  }
  addEvent(evt)
  
  // -- Checks for equivalence 
  def >=(that:AtomicHistory):Boolean = {
    return true
    
    var condsThat:Set[Condition] = Set()
    var condsThis:Set[Condition] = Set()
    
    // -- Gather conditions from the comparing history
    History.traverse(that, h => {
      condsThat ++= h.event.preset
      condsThat --= h.event.readarcs
      condsThat --=  h.event.postset
    })
    
    // -- Gather conditions from this history
    History.traverse(this, h => {
      condsThis ++= h.event.preset
      condsThis --= h.event.readarcs
      condsThis --= h.event.postset
    })
    
    // -- There must be some thing to do the comparison on.
    if (condsThis.isEmpty || condsThat.isEmpty) {
      return false
    }
    
    // -- Check one against the other  
    for (cond <- condsThat) {
      condsThis.find(_.image == cond.image) match {
        case None        => {
          return false
        }
        case Some(cond2) => {
          try {
            // -- Check sigma function equality
        	val test = that.sigma(cond) == this.sigma(cond2)
        	if (!test) { return false }
          } catch {
            // -- The test has failed, there is not 
            case _ => return false 
          }
        }
      }
    }
    // -- Ok, the two given histories are equal 
    return (that.psi subsetOf this.psi)
  }
  
}

object AtomicHistory {
  // private val sigma:Map[Condition, SigmaFunction] = Map()
  val totalPsi :BitSet = BitSet()
  
  var breakCouples:Set[(Int, Int)] = Set()
  var runtimeAtomic:Map[Event, Int] = Map()
  var atomicLineMap:Map[Int, Int]   = Map()
  
  
  // -- Runtime history counting
  private var nextRuntimeAtomicId:Int = 0; 
  
  // -- Return the next runtime identifier
  def getNextRuntimeId():Int = {
    val id = nextRuntimeAtomicId
    nextRuntimeAtomicId += 1
    return id
  }
  
  // -- 
  def printModelCheckingInfos(time:Long) {
    if (totalPsi.size == 0)
      println(">> No atomic section found broken during model checking")
    else {
      println(">> Found atomic break:")
      for ((asLine, breakLine) <- breakCouples) { 
    	  println("  [" + asLine + "] >> Interruption by code at line " + breakLine)
      }
      println(">> Done in " + time + "ms")
    }
  }
}
