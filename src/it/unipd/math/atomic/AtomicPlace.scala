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

import it.unipd.math.cpnunf._
import scala.collection.mutable.BitSet

// -----------------------------------------------------------------------------
// -- Trait to decorate places with their atomic sections 
// -----------------------------------------------------------------------------
trait Atomic {
  // -- Atomic section number  
  var atomicSection:Int = -1
  
  // -- Runtime atomic section number
  var runtimeAtomic:Int = -1
  
  // -- Line of code of the atomic decoration
  var codeLine:Int = 0
  
  // -- Hash code for each atomic place. 
  var hash:Int = 0
  
  // -- Returns if the place is atomic
  def isAtomic(): Boolean = atomicSection != -1
}

class SigmaFunctionOld(val condition:Condition)  {
  
  // -- Optimized with bit sets. 
  var func:(BitSet, BitSet, BitSet, BitSet) = (BitSet(), BitSet(), BitSet(), BitSet())
  
  // -- Add to component the given atomic section 
  def addTo(index:Int, atomic:Int) =  index match {
    case 1 => func._1 += atomic; case 2 => func._2 += atomic
    case 3 => func._3 += atomic; case 4 => func._4 += atomic
    case n => sys.error("Could not get index " + n)
  }
  
  def addToWrite() = func._4 ++= func._1 ++ func._2 ++ func._3
  def addToRead()  = func._3 ++= func._1 ++ func._2 ++ func._4
  
  // -- Get if an elements exists in the second component of the function. 
  // -- The second component holds informations about incomplete chain with 
  // -- more than one element.
  def existsInSecond(as:Int) = func._3.contains(as) || func._4.contains(as) 
  
  // -- Get if an element exists in the first component of the function. 
  // -- First element contains connections with atomic sections
  def existsInFirst(as:Int) = func._1.contains(as) || func._2.contains(as)
  
  // -- Return if exists in write components of the function
  def existsInWrite(as:Int) = func._2.contains(as) || func._4.contains(as)
  
  // -- Return if exists in all of the function 
  def exists(as:Int) = existsInFirst(as) || existsInSecond(as)
  
  def nonEmpty = func._1.nonEmpty || func._2.nonEmpty || 
                 func._3.nonEmpty || func._4.nonEmpty 
  
                 
                 
  // -- Print function
  override def toString = func.toString
  
}

/*
// -----------------------------------------------------------------------------
// -- Implements the concept of cutting context associated with this one.
// -----------------------------------------------------------------------------
class AtomicHistoryOld(var evt:Event, override val consumed :Set[EnrichedCondition], 
                    override val read :Set[EnrichedCondition]) extends History(evt, consumed, read) {
  
  var hist:History = null
  
  // -- Interrupted atomic secions
  var psi:BitSet = BitSet()
  
  // -- Incomplete chains of interference
  var sigma:Map[Int, SigmaFunctionOld] = Map()
  var images:Map[Int, Place] = Map()
  
  // -- FIXME: This function does not do its job, why?
  def checkInterference(e:Event, as:Int):Boolean = {
    for (cond <- e.preset) {
      if (sigma.contains(cond.id) && (!sigma(cond.id).exists(as))) {
        
        return true 
      } 
    }
    
    for (cond <- e.readarcs) {
      if (sigma.contains(cond.id) && (!sigma(cond.id).existsInWrite(as))) 
        return true
    }
    return false
  } 
  
  // -- add an event to current configuration
  def addEvent(e:Event) {
    var sgm:SigmaFunctionOld = null
    val as = e.image match {
      case t:Atomic => t.atomicSection
      case _ => -1
    }
    
    // -- If the atomic section has been broken before this point do not check
    if (psi.contains(as)) return 
    
    // -- Update psi function 
    for (cond <- e.preset if as != -1) {
	  sgm = sigma.getOrElse(cond.id, null)
	  
	  if (sgm != null && sgm.existsInSecond(as)) {
	    psi += as
	  } 
	}
    
    // -- Update sigma with e's post 
    for (cond <- e.postset) {
      sgm = sigma.getOrElse(cond.id, new SigmaFunctionOld(cond))
      if (as != -1) {
        sgm.addTo(1, as)
      }
      
      if(checkInterference(e, as)) {
         sgm.addToRead()
      }
      
      if (sgm.nonEmpty && !sigma.contains(cond.id)) {
        println("Updating sigma [postset] for " + e.name)
        sigma  += (cond.id -> sgm)
        images += (cond.id -> cond.image)
      }
    }
    
    // -- Update sigma with event's context
    for (cond <- e.readarcs) {
      sgm = new SigmaFunctionOld(cond)
      if (as != -1) {
        sgm.addTo(2, as)
      }
      
      if (checkInterference(e, as)){
        sgm.addToWrite()
      }
      
      if (sgm.nonEmpty) {
    	  println("Updating sigma [context] for " + e.name)
    	  sigma += (cond.id -> sgm)
    	  images += (cond.id -> cond.image)
      }
    }
  } 
  
  // -- Compare two different atomic configuration
  def equalsTo(conf:AtomicHistoryOld):Boolean = {
      if (psi != conf.psi) return false
      // TODO: this is not enough right now, after we got the two place with the
      // TODO: same image we need to compare them on their sigma function.  
      for (cond <- sigma) {
        if (!conf.images.exists(_ == images(cond._1))) return false
      }
      return true
  }
 
  // -- Update current history with enriched condition's information 
  def updateSets(h:History) {
	h match {
	  case ah:AtomicHistoryOld => {
	    sigma ++= ah.sigma
	    psi   ++= ah.psi
	  }
	  case _ => sys.error("Trying to update a non atomic configuration") 
	}
  }
  
  // -- Update current history with the related decorations
  def updateHistory(h:History) {
    hist = h
    var contribution:Set[History] = Set()
    
    // -- Update the marking traversing the history
    def getMarking(ep:EnrichedCondition) = History.traverse(ep.h, x => {
      contribution += x
      for (ep <- consumed) 
        contribution -= ep.h
      for (ep <- read)	   
        contribution -= ep.h
    })
    
    // -- Traverse all enriched pairs in consumed + read histories 
    for(ep <- consumed ++ read) getMarking(ep)
    
    // println(h.event.name + " Contribution: " + contribution)
    
    // -- Add contribution from current marking's histories
    for (ep <- contribution) updateSets(ep)
    
    // -- Add maximal event from history
    // FIXME: the error is in the creation of a new event!
    // val newEvent =  new Event(hist.event.image, hist.event.preset) with Atomic
    // This should fix 
    addEvent(h.event)
    
    // -- TODO: Remove this 
    debugPrint
  }
  
  def debugPrint() = {
    println("Psi   => " + psi)
    print("Sigma => ")
    for ((k, i) <- sigma) {
      print("(" + k + " -> " + i + ") ")
    }
    println("")
  }
  
  
}
*/
