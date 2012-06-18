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

// -----------------------------------------------------------------------------
// -- Create the petri net with factory method pattern
// -----------------------------------------------------------------------------
class PetriFactory {
  
  // -- Private data fields 
  private var net                        = new PetriNet()
  private var lastPlace:Place            = null
  private var lastTransition:Transition  = null
  private var danglingIf:Transition      = null
  private var varMap: Map[String, Place] = Map()
  private var closePlace:Place 			 = PetriFactory.createPlace(0, "end", Nil, -1) 
  
  // -- add close place
  net.places += closePlace
  
  private def getPlaceFromHash(hash:Int): Place = 
    net.places.find(p => if (p.id == hash) true; else false).getOrElse(null)
  
  // -- Returns the net created 
  def cNet = net
  
  // -- Returns last place inserted 
  def getLastPlace = lastPlace
  
  def createVariableMap(vars:Set[String]) = {
    for (v <- vars) {
      val p = PetriFactory.createPlace(v.hashCode(), v, Nil, -1)
      varMap += v -> p
      net.places += p
    }
  }
    
  // -- Add the place to the net
  def addPlace(hash:Int, name:String, as:Int) {
    var p:Place = null
    if (lastPlace == null)
      p = PetriFactory.createPlace(hash, name, Nil, as)
	else if (danglingIf == null) 
	  p = PetriFactory.createPlace(hash, name, List(lastTransition), as)
	else {
	  p = PetriFactory.createPlace(hash, name, List(lastTransition, danglingIf), as)
	  danglingIf = null
	} 
		
    // -- Add the newly created place. 
    net.places += p
    lastPlace = p
  }
 
  // -- Close a loop from the last transition given to the given place.
  def closeLoop(hash:Int) = {
    val p = getPlaceFromHash(hash)
    
    if (p != null) {
      // -- Close the loop and set place hash as last place. 
      lastPlace = p
      p.addPre(lastTransition)
      lastTransition.addPost(p)
    } else {
      error("Could not close loop!")
    }    
  }
  
  // -- Start building an else brach
  def elseBranch(hash:Int):Transition = {
	  val p = getPlaceFromHash(hash)
	  if (p != null) lastPlace = p
	  else error("Could not build else brach") 
	    
	  return lastTransition
  }
  
  // -- Closes an if branch 
  def endIf(t:Transition) = danglingIf = t
  
  // -- Return the last transition
  def asynch():Transition = lastTransition
  
  // -- Restore the last transition to continue with the generation, used when
  // -- creating a new parallel flow of control. 
  def continue(t:Transition) = {
    close
    lastTransition = t
  }
  
  // -- Close the last transition with the closing place.  
  def close() {
    closePlace.addPre(lastTransition)
    lastTransition.addPost(closePlace)
  }
  
  // -- Add a transition to the net 
  def addTransition(hash:Int, name:String, read:Set[String], write:String, as:Int) {
    var preset:List[Place] = List(lastPlace)
    if (write != null) {
      preset ::= varMap(write) 
    }
    
    var context:List[Place] = List()
    for (v <- read) { context ::= varMap(v) }
    
    val t = PetriFactory.createTransition(hash, name, preset, context, as)
    
    for (v <- read) { varMap(v).addRead(t) }
    
    // -- A variable is written  
    if (write != null) {
      t.addPost(varMap(write))
    }
    
    lastTransition = t
    net.transitions += t
  }
  
}

// -----------------------------------------------------------------------------
// -- Companion object for factoring places and transitions
// -----------------------------------------------------------------------------
object PetriFactory {
  
  // -- Create a new place with the given data. 
  def createPlace(hash:Int, name:String, preset:List[Transition], as:Int):Place = {
    val place = new Place(hash, name, (0, 0), Map()) with Atomic
    place.atomicSection = as;
    for (t <- preset) {
      place.addPre(t)
      t.addPost(place)
    }
    return place
  }
  
  // -- Create a transition from the given data. 
  def createTransition(hash:Int, name:String, preset:List[Place], 
      context:List[Place], as:Int): Transition = {
    val transition = new Transition(hash, name, (0, 0), Map()) with Atomic
    transition.atomicSection = as
    for (p <- preset)  {
      transition.addPre(p)
      p.addPost(transition)
    }
    for (p <- context) {
      transition.addRead(p)
      p.addRead(transition)
    }
    
    return transition
  }
  
}


