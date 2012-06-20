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

// -----------------------------------------------------------------------------
// -- Perform an abstract run on the given program node and augment parse tree
// -- with its reduct.
// -----------------------------------------------------------------------------
class ReductAbstractRunner {
  
  private def runStmtList(list:List[ProgramNode with Reduct], queue:String, as:Int):List[Reduct] = list match {
    case fst::rest => {
      val nameRest   = Reduct.prettyPrinter(BlockNode(rest)) + queue  
      val currentRed = run(fst, nameRest, as)
      currentRed :: runStmtList(rest, queue, as)
    }
    
    case Nil => Nil
  } 
  
  // -- Perform an abstract run over the given program node
  def run(node:ProgramNode, queue:String = "", as:Int = -1):Reduct = {
    val name = Reduct.prettyPrinter(node) + queue
    val hash = scala.math.abs(name.hashCode)
    node match {
      
      case BlockNode(list) => {
        val reductList = runStmtList(list, queue, as)
        val reduct = new BlockNode(reductList) with Reduct
        reduct.hash = hash
        reduct.strid = name
        reduct.atom = as
        reduct
      }
      case WhileNode(cond, body) => { 
        val newBody = run(body, name, as)
        var reduct  = new WhileNode(cond, newBody) with Reduct
        // reduct.node = reduct
        reduct.hash = hash
        reduct.strid = name
        reduct.atom = as
        
        reduct
      }
      
      case IfNode(cond, ifb, elseb) => {
        val newIfb   = run(ifb, queue, as)
        val newElseb = run(elseb, queue,as)
        var reduct   = new IfNode(cond, newIfb, newElseb) with Reduct
        
        reduct.hash = hash
        reduct.strid = name
        reduct.atom = as
        
        reduct
      }
      
      case AsynchNode(body) => {
        val newBody = run(body, queue, as)
        var reduct = new AsynchNode(newBody) with Reduct
        reduct.strid = name
        reduct.hash = hash
        reduct.atom = as
        reduct
      }
      
      case AtomicNode(body) => {
        
        if (as != -1) sys.error("Could not nest atomic sections")
          
        val atomicId = getNextAtomicSection
        val newBody = run(body, queue, atomicId)
        val reduct  = new AtomicNode(newBody) with Reduct
        reduct.strid = name
        reduct.hash = hash
        reduct.atom = atomicId
        reduct
      }
      
      case AssignNode(v, e) => {
        val reduct = new AssignNode(v, e) with Reduct
        reduct.hash = hash
        reduct.strid = name
        reduct.atom = as 
        reduct
      }
      
      case SkipNode() => {
        val reduct = new SkipNode() with Reduct
        reduct.strid = name
        reduct.hash = hash
        reduct.atom = as
        reduct
      }
    }
  }
  
  private var nextAtomic = 0
  private def getNextAtomicSection = { val n = nextAtomic; nextAtomic += 1; n }
}

// -----------------------------------------------------------------------------
// -- Reduct trait is composed by a program node, its hash, the name and the 
// -- atomic section where its belong
// -----------------------------------------------------------------------------
trait Reduct extends ProgramNode {
  // var node:ProgramNode
  var hash:Int    = 0
  var strid:String = ""
  var atom:Int    = 0
  
}

// -----------------------------------------------------------------------------
// -- Utility object of printing reducts and doing other stuffs onto parse trees
// -----------------------------------------------------------------------------
object Reduct {
  
  // -- Pretty printer for programs node. This method may be useful in 
  // -- calculating hashes of reducts. 
  def prettyPrinter(node:ProgramNode):String = node match {
    // -- List of statement 
    case BlockNode(list) => {
      var str = "{";
      for (stmt <- list) str += prettyPrinter(stmt) + ";" 
      str + "}"
    }
    
    // -- While b do C
    case WhileNode(cond, body) => 
      "while " + prettyPrinter(cond) + " do " + prettyPrinter(body)
      
    // -- If b then C else D
    case IfNode(cond, ifb, elseb) => 
      "if " + prettyPrinter(cond) + " then " + prettyPrinter(ifb) + 
      " else " + prettyPrinter(elseb)
      
    // -- Asynch C and Atomic C 
    case AsynchNode(body) => "asynch " + prettyPrinter(body)
    case AtomicNode(body) => "atomic " + prettyPrinter(body) 
    
    // -- Var := Expr
    case AssignNode(VarNode(s), expr) => s + " := " + prettyPrinter(expr)
    
    // -- Skip 
    case SkipNode() => "skip"
    
    // -- Variable 
    case VarNode(name) => name
    
    // -- Binary operator
    case BinaryOp(op, fst, snd) => prettyPrinter(fst) + op + prettyPrinter(snd)
      
    // -- Unary operator 
    case UnaryOp(op, fst) => op + prettyPrinter(fst)
    
    case LiteralNode(i) => i.toString
    
    // -- Default case
    case _ => sys.error("There were an error while printing the program")   
  }
  
  // -- Prints in a readable way an abstract run from a reduct. 
  def printReduct(red:Reduct):Unit = red match {
    case BlockNode(list) =>
      for (red <- list)  printReduct(red)  
    case WhileNode(cond, body) => { 
      print(red.strid); print(" -> "); printReduct(body); print("\n ->")
    }
    case IfNode(cond, ifb, elseb) => {
      print(red.strid + " [true] -> "); printReduct(ifb)
      print("\n [false] -> "); printReduct(elseb)
    }
    case AsynchNode(body) => {
      print(red.strid + " [spawn] -> "); printReduct(body); print(" [end]\n ==>")
    }
    case AtomicNode(body) => {
      print(red.strid + " [atomic " + red.atom + "] -> "); printReduct(body);
      print("[end atomic " + red.atom + "]")
    }
    case _ => print(red.strid + " -> ")
  }
  
  
}

