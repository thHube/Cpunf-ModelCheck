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
  
  private def getPlaceFromHash(hash:Int): Place = 
    net.places.find(p => if (p.id == hash) true; else false).getOrElse(null)
  
  // -- Returns the net created 
  def cNet = net
  
  // -- Returns last place inserted 
  def getLastPlace = lastPlace
  
  def createVariableMap(vars:Set[String]) = {
    for (v <- vars) {
      val p = PetriFactory.createPlace(v.hashCode(), v, Nil)
      varMap += v -> p
      net.places += p
    }
  }
    
  // -- Add the place to the net
  def addPlace(hash:Int, name:String) {
    var p:Place = null
    if (lastPlace == null)
      p = PetriFactory.createPlace(hash, name, Nil)
	else if (danglingIf == null) 
	  p = PetriFactory.createPlace(hash, name, List(lastTransition))
	else {
	  p = PetriFactory.createPlace(hash, name, List(lastTransition, danglingIf))
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
  // -- Restore the last transition to continue with the generation
  def continue(t:Transition) = lastTransition = t
  
  // -- Add a transition to the net 
  def addTransition(hash:Int, name:String, read:Set[String], write:String) {
    var preset:List[Place] = List(lastPlace)
    if (write != null) {
      preset ::= varMap(write) 
    }
    
    var context:List[Place] = List()
    for (v <- read) { context ::= varMap(v) }
    
    val t = PetriFactory.createTransition(hash, name, preset, context)
    
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
  def createPlace(hash:Int, name:String, preset:List[Transition]):Place = {
    val place = new Place(hash, name, (0, 0), Map())
    for (t <- preset) {
      place.addPre(t)
      t.addPost(place)
    }
    return place
  }
  
  // -- Create a transition from the given data. 
  def createTransition(hash:Int, name:String, preset:List[Place], 
      context:List[Place]): Transition = {
    val transition = new Transition(hash, name, (0, 0), Map())
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


