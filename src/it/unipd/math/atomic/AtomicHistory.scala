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
    for (item <- sigma.readFirst ++ sigma.writeFirst ++ sigma.readContinue ++ sigma.writeContinue) {
      if (item != as) { readContinue += item }
    }
  }
    
  // -- Update the context for read continue
  def updateContextRead(sigma:SigmaFunction, as:Int) = {
    for (item <- sigma.writeContinue ++ sigma.writeFirst) {
      if (item != as) { readContinue += item }
    }
  }
  
  // -- Update preset for write contiunue
  def updatePreWrite(sigma:SigmaFunction, as:Int) = {
    for (item <- sigma.readFirst ++ sigma.writeFirst ++ sigma.readContinue ++ sigma.writeContinue) {
      if (item != as) { writeContinue += item }
    }
  }
    
  // -- Update the context for write continue
  def updateContextWrite(sigma:SigmaFunction, as:Int) = {
    for (item <- sigma.writeContinue ++ sigma.writeFirst) {
      if (item != as) { writeContinue += item }
    }
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
  
//  val sigma:Map[Condition, SigmaFunction] = Map()
//  val psi  :BitSet = BitSet()

  // -- Update the psi set with atomicity breaks
  def updatePsi(event:Event, as:Int) {
    for (se <- event.preset) AtomicHistory.sigma.get(se) match {
      case Some(sgm) => if (sgm.readContinue.exists(_ == as) || 
          sgm.writeContinue.exists(_ == as)) {
        AtomicHistory.psi += as
      }
      case None => 
    }
  }
  
  // -- Update sigma function  
  def updateSigma(event:Event, as:Int) {
    // -- Update context first
    for (s <- event.readarcs) {
      val sgm = AtomicHistory.sigma.getOrElse(s, new SigmaFunction)
      // -- Condition 1 of context update is satisfied
      if (as != -1) {
        sgm.readFirst += as
      }
      
      // -- Condition 3 of context update is satisfied
      for (se <- event.preset) AtomicHistory.sigma.get(se) match {
        case Some(sgmFn) => sgm.updatePreRead(sgmFn, as)
        case None => // -- Skip
      }
      for (se <- event.readarcs) AtomicHistory.sigma.get(se) match {
        case Some(sgmFn) => sgm.updateContextRead(sgmFn, as)
        case None =>
      }
      
      // -- Update sigma set
      AtomicHistory.sigma += (s -> sgm)
    }
    
    // -- Update post set.
    for (s <- event.postset) {
      val sgm = new SigmaFunction
      // -- Condition 2 of postset update is satisfied
      if (as != -1) {
        sgm.writeFirst += as
      }
      
      // -- Condition four of update is satisfied 
      for (se <- event.preset) AtomicHistory.sigma.get(se) match {
        case Some (sgmFn) => sgm.updatePreWrite(sgmFn, as)
        case None => 
      }
      for (se <- event.readarcs) AtomicHistory.sigma.get(se) match {
        case Some(sgmFn) => sgm.updateContextWrite(sgmFn, as)
        case None =>
      }
      
      AtomicHistory.sigma += (s -> sgm)
    }
  }
  
  var contribution:Set[History] = Set()
    
  // -- Update the marking traversing the history
  def getMarking(ep:EnrichedCondition) = History.traverse(ep.h, x => {
    contribution += x
	for (ep <- consumed) 
	  contribution -= ep.h
	for (ep <- read)	   
	  contribution -= ep.h
  })
  
  /*
  def recalcSigmaAndPsi = {
    for (h <- contribution) h match {
        case ah:AtomicHistory => {
          sigma ++= ah.sigma
          psi   ++= ah.psi
        }
        case _ => println("skippin' non atomic history")
      }
  } 
  */
  
  // -- 
  def addEvent(event:Event) {
    val as:Int = event.image match {
      case t:Atomic => t.atomicSection;
      case _ => -1
    }
    
    // -- TODO find histories that contribute to this atomic
    for (ep <- consumed ++ read) getMarking(ep)
    
    // recalcSigmaAndPsi
    updateSigma(event, as)
    updatePsi(event, as)
    
    
    // println(contribution)
    // println(AtomicHistory.psi)
    // println(AtomicHistory.sigma)
    // println(" ___ ")
  }
  addEvent(evt)
  
  // -- Checks for equivalence 
  def ==(that:AtomicHistory):Boolean = {
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
    if (condsThis.isEmpty || condsThat.isEmpty) false
    
    // -- Check one against the other  
    for (cond <- condsThis) {
      condsThat.find(_.image == cond.image) match {
        case None        => {
          false
        }
        case Some(cond2) => {
          try {
            // -- Check sigma function equality
        	val test = AtomicHistory.sigma(cond) == AtomicHistory.sigma(cond2)
        	if (!test) { false }
          } catch {
            // -- The test has failed, there is not 
            case _ => false 
          }
        }
      }
    }
    // println("Looks like we have a cutoff here!")
    // -- Ok, the two given histories are equal 
    true
  }
  
}

object AtomicHistory {
  val sigma:Map[Condition, SigmaFunction] = Map()
  val psi  :BitSet = BitSet()
}
