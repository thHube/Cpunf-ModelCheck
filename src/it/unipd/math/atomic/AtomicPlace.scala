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
  
  // -- Hash code for each atomic place. 
  var hash:Int = 0
  
  // -- Returns if the place is atomic
  def isAtomic(): Boolean = atomicSection != -1
}

class SigmaFunction(val condition:Condition)  {
  
  // -- Optimized with bit sets. 
  var func:(BitSet, BitSet, BitSet, BitSet) = (BitSet(), BitSet(), BitSet(), BitSet())
  
  // -- Get the appropriate component of the sigma function 
  def getComponent(index:Int) = index match {
    case 1 => func._1; case 2 => func._2
    case 3 => func._3; case 4 => func._4
    case n => sys.error("Could not get index " + n)
  }
  
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
  
}

// -----------------------------------------------------------------------------
// -- Implements the concept of cutting context associated with this one.
// -----------------------------------------------------------------------------
trait AtomicConfiguration extends History {
  
  // -- Interrupted atomic secions
  var psi:BitSet = BitSet()
  
  // -- Incomplete chains of interference
  var sigma:Map[Condition, SigmaFunction] = Map()
  
  def checkInterference(e:Event, as:Int):Boolean = {
    for (cond <- e.preset) {
      if (sigma.contains(cond) && sigma(cond).exists(as)) 
        return true
    }
    
    for (cond <- e.readarcs) {
      if (sigma.contains(cond) && sigma(cond).existsInWrite(as)) 
        return true
    }
    return false
  } 
  
  // -- add an event to current configuration
  def addEvent(e:Event with Atomic) {
    var sgm:SigmaFunction = null
    val as = e.atomicSection
    
    // -- If the atomic section has been broken before this point do not check
    if (psi.contains(as)) return 
    
    // -- Update psi function 
    for (cond <- e.preset if as != -1) {
	  sgm = sigma.getOrElse(cond, null)
	  if (sgm != null && sgm.existsInSecond(as)) {
	    psi += as     
	  } 
	}
    
    // -- Update sigma with e's post 
    for (cond <- e.postset) {
      sgm = sigma.getOrElse(cond, new SigmaFunction(cond))
      if (as != -1) {
        sgm.getComponent(1) += as
      } else if(checkInterference(e, as)) {
         sgm.getComponent(3) += as
      }
      
      if (!sigma.contains(cond)) {
        sigma += (cond -> sgm)
      }
    }
    
    // -- Update sigma with event's context
    for (cond <- e.readarcs) {
      sgm = new SigmaFunction(cond)
      if (as != -1) {
        sgm.getComponent(2) += as
      } else if (checkInterference(e, as)){
        sgm.getComponent(4) += as
      }
      sigma += (cond -> sgm)
    }
  } 
  
  // -- Compare two different atomic configuration
  def equalsTo(conf:AtomicConfiguration):Boolean = {
      if (psi != conf.psi) return false
      // TODO: this is not enough right now, after we got the two place with the
      // TODO: same image we need to compare them on their sigma function.  
      for (cond <- sigma) {
        if (!conf.sigma.exists(_._1.image == cond._1.image)) return false
      }
      return true
  }
  
  // -- Comparison method implementation
  def compare(that:AtomicConfiguration):Int = {
    var psiContained:Int = 0
    if ((psi subsetOf that.psi) && !(that.psi subsetOf psi)) psiContained = 1  
    else if (!(psi subsetOf that.psi) && (that.psi subsetOf psi)) psiContained = -1
    
    // ?? 
    
    return psiContained
  }
  
  // -- Update current history with enriched condition's information 
  def updateEnrichedCondition(ep:EnrichedCondition) {
	ep.h match {
	  case ah:AtomicConfiguration => {
	    sigma ++= ah.sigma
	    psi   ++= ah.psi
	  }
	  case _ => sys.error("Trying to update a non atomic configuration") 
	}
  }
  
  // -- Update current history with the related decorations
  def updateHistory() {
    event.image match {
      case t:Transition with Atomic => {
        val evt = new Event(t, consumed.map(_.c), read.map(_.c)) with Atomic
        evt.atomicSection = t.atomicSection
        for (e <- consumed) updateEnrichedCondition(e)
        for (e <- read) updateEnrichedCondition(e)
        addEvent(evt)
      }
      case _ => sys.error("Could not add events without atomic decoration")
    }
  }
  
}

// -----------------------------------------------------------------------------
// -- Bridge class between history and atomic configuration
// -----------------------------------------------------------------------------
class AtomicHistory(e:Event, c:Set[EnrichedCondition], r:Set[EnrichedCondition]) 
    extends History(e, c, r) with AtomicConfiguration 


