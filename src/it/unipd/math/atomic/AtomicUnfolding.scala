package it.unipd.math.atomic

import it.unipd.math.cpnunf._

class AtomicUnfolding(net: PetriNet) extends Unfolding(net) {

  // override val root_event:Event with Atomic = new Event(null, Set()) with Atomic
  // root_event.atomicSection = -1
  override val root_history:AtomicHistory = new AtomicHistory(root_event, Set(), Set())
  
  
  // -- Test if the given history is a cutoff. 
  override def isCutoff(h: History): Boolean = {
    // super.isCutoff(h)
    
    // -- Get the set of histories that refer to the marking.
    val compare_to = markings.getOrElse(h.marking, Set())
    for (h2 <- compare_to) {
      if (h2 > h) h match {
        case ah: AtomicHistory => h2 match {
          case ah2:AtomicHistory  => {
            if (ah == ah2) {
              println("??")
              true 
            }
          }
          case _ => {
            println("Discarding history 2")
            false
          }
        }
        case _ => println("Discarding history 1"); false
      }
    }
    false
  }
}


class AtomicUnfolder(net:PetriNet) extends Unfolder(net) {
  
  override val unfolding:AtomicUnfolding = new AtomicUnfolding(net)
  
  // -- Create an history from preset, read arcs and transitions
  override def make_history(preset  : Set[EnrichedCondition], 
                            readarcs: Set[EnrichedCondition], 
                            trans   : Transition): AtomicHistory = {
      // -- Get events in preset. 
      val evt_pre = preset.map(_.c)
      // -- Get events in readarcs
      val evt_read = readarcs.map(_.c)
      
      // -- For each event in transition preimage
      for (evt <- trans.preimage) {
          if (evt.preset == evt_pre && evt.readarcs == evt_read) {
            return  new AtomicHistory(evt, preset, readarcs)
          }
      }
      
      return new AtomicHistory(new Event(trans, evt_pre, evt_read), preset, readarcs)
  }  
}

