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
  private var initialMark:Boolean        = true
  private var varMap: Map[String, Place] = Map()
  private var closePlace:Place 			 = PetriFactory.createPlace(0, "end", Nil, -1, false) 
  
  // -- add close place
  net.places += closePlace
  
  private def getPlaceFromHash(hash:Int): Place = 
    net.places.find(p => p match {
      case p:Place with Atomic if (p.hash == hash) => true
      case _=> false
    }).getOrElse(null)
    
  
  // -- Returns the net created 
  def cNet = net
  
  // -- Returns last place inserted 
  def getLastPlace = lastPlace
  
  // -- Create a variable and assign it to the net.
  def createVariableMap(vars:Set[String]) = {
    for (v <- vars) {
      val p = PetriFactory.createPlace(v.hashCode(), v, Nil, -1, true)
      varMap += v -> p
      // -- Add to net and to initial marking
      net.places += p
      net.m0 += p
    }
  }
    
  // -- Add the place to the net
  def addPlace(hash:Int, name:String, as:Int) {
    // -- First check if there is the place 
    var p:Place = getPlaceFromHash(hash)
    if (p != null) {
      if (lastPlace != null) {
        if (danglingIf == null) {
          p.addPre(lastTransition)
        } else {
          p.addPre(lastTransition) 
          p.addPre(danglingIf)
        }
      }
      lastPlace = p
    } else {
      // -- Place is null, go on and add the current place. 
      if (lastPlace == null)
        p = PetriFactory.createPlace(hash, name, Nil, as, initialMark)
      else if (danglingIf == null) 
	    p = PetriFactory.createPlace(hash, name, List(lastTransition), as, initialMark)
      else {
	    p = PetriFactory.createPlace(hash, name, List(lastTransition, danglingIf), as, initialMark)
	    danglingIf = null
	  } 
		
      // -- Add the newly created place. 
      net.places += p
      lastPlace = p
      // -- Generate initial marking 
      if (initialMark) {
        initialMark = false;
        net.m0 += p
      }
    }
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
      sys.error("Could not close loop!")
    }    
  }
  
  // -- Start building an else brach
  def elseBranch(hash:Int):Transition = {
	  val p = getPlaceFromHash(hash)
	  if (p != null) lastPlace = p
	  else sys.error("Could not build else brach") 
	    
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
    
    var context:List[Place] = List()
    for (v <- read) { context ::= varMap(v) }
    
    val t = PetriFactory.createTransition(hash, name, preset, context, as)
    
    // -- TODO: This should be removed
    // -- Add all read variables 
    for (v <- read) {
      t.addRead(varMap(v))
      varMap(v).addRead(t) 
    }
    
    // -- A variable is written  
    if (write != null) {
      t.addPre(varMap(write))
      t.addPost(varMap(write))
      // -- TODO
      varMap(write).addPre(t)
      varMap(write).addPost(t)
    }
    
    lastTransition = t
    net.transitions += t
  }
  
}

// -----------------------------------------------------------------------------
// -- Companion object for factoring places and transitions
// -----------------------------------------------------------------------------
object PetriFactory {
  
  var codeLine:Int = 0
  
  // -- Create a new place with the given data. 
  def createPlace(hash:Int, name:String, preset:List[Transition], as:Int, init:Boolean):Place = {

    var argmap:Map[String, Any] = Map()
    if (init) {
      argmap += ("init-mark" -> 1)
    }
    
    val place = new Place(PetriNet.place_max_id, name, (0, 0), argmap) with Atomic
    place.atomicSection = as;
    // -- Set hash and increment id 
    place.hash = hash
    PetriNet.place_max_id += 1
    
    for (t <- preset) {
      place.addPre(t)
      t.addPost(place)
    }
    
    place.codeLine = codeLine
    return place
  }
  
  // -- Create a transition from the given data. 
  def createTransition(hash:Int, name:String, preset:List[Place], 
      context:List[Place], as:Int): Transition = {
    val transition = new Transition(PetriNet.trans_max_id, name, (0, 0), Map()) with Atomic
    transition.atomicSection = as
    // -- Set hash and increment id  
    transition.hash = hash
    PetriNet.trans_max_id += 1
    
    for (p <- preset)  {
      transition.addPre(p)
      p.addPost(transition)
    }
    for (p <- context) {
      transition.addRead(p)
      p.addRead(transition)
    }
    
    transition.codeLine = codeLine
    return transition
  }
  
}


