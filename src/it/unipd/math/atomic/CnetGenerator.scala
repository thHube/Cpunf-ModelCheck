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
// -- Generate a contextual net from a parse tree. 
// -----------------------------------------------------------------------------
class CnetGenerator(val root:ProgramNode with Reduct) {
 
  private var petriFactory = new PetriFactory()
  
  private def generate(node:ProgramNode with Reduct) {
    generateInner(node)
    petriFactory.addPlace(0, "end")
  }
  
  // -- Generate the net from the given program node 
  private def generateInner(node:ProgramNode with Reduct):Unit = node match {
    
    // -- Contatenation of statements  
    case BlockNode(list)          => listToNet(list)
      
    // -- While net generation 
    case WhileNode(cond, body) => {
        val fvb = FreeVariables.get(cond)
        val condName = Reduct.prettyPrinter(cond)
        
        petriFactory.addPlace(node.hash, node.strid)
        petriFactory.addTransition(node.hash, condName, fvb, null)
        
        generateInner(body)
        
        petriFactory.closeLoop(node.hash)
        petriFactory.addTransition(node.hash + 1, condName, fvb, null)
    }
    
    // -- If then else net generation  
    case IfNode(cond, ifb, elseb) => {
      val fvb      = FreeVariables.get(cond)
      val condName = Reduct.prettyPrinter(cond)
      val ifName   = Reduct.prettyPrinter(ifb)
      val elseName = Reduct.prettyPrinter(elseb)
      
      // -- Generate if branch net
      petriFactory.addPlace(node.hash, node.strid)
      petriFactory.addTransition(node.hash, condName, fvb, null)
      generateInner(ifb)
      
      val t = petriFactory elseBranch node.hash
      
      // -- Generate else branch net
      petriFactory.addTransition(node.hash + 1, condName, fvb, null)
      generateInner(elseb)
      
      petriFactory endIf t 
    }
    
    // -- Atomic statement 
    case AtomicNode(body) => generateInner(body)
    
    // -- Asynch thread spawning 
    case AsynchNode(body) => {
      val name = Reduct.prettyPrinter(node)
      petriFactory.addPlace(node.hash, node.strid)
      petriFactory.addTransition(node.hash, name, Set(), null)

      // -- Generate asynch thread and restore current thread to continue 
      // -- the net generation
      val t = petriFactory.asynch
      generateInner(body)
      petriFactory continue t
    }
    
    // -- Assignment net generation 
    case AssignNode(VarNode(write), expr) => {
      val name = Reduct.prettyPrinter(node)
      petriFactory.addPlace(node.hash, node.strid)
      petriFactory.addTransition(node.hash, name, FreeVariables.get(expr), write)
    }

    // -- Error handling 
    case _ => error("Cannot generate c-net for the program.")
  } 
  
  // -- Convert a list of reducts to a net. 
  private def listToNet(list:List[ProgramNode with Reduct]):Unit = list match {
    case Nil => { } // Do nothing at the end of the list   
    
    case _   => {
      generateInner(list.head)
      listToNet(list.tail)
    }
  }
  
  // -- Generate next atomic section id
  private var nextAtomic = 0
  def nextAtomicSection(): Int = { val n = nextAtomic; nextAtomic += 1; n }
  
  // -- Return the generated petri net.
  var vars = FreeVariables.getAll(root)
  println(vars)
  petriFactory.createVariableMap(vars)
  generate(root)
  def getGeneratedNet = petriFactory.cNet 
}

// -- Companion object to track 
object CnetGenerator {
  
}

