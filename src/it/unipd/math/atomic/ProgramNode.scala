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

// -- Represent a generic program node.
class ProgramNode

// -- Represent a block of instructions 
case class BlockNode(stmt:List[ProgramNode with Reduct]) extends ProgramNode with Reduct

// -- Represent a while loop
case class WhileNode(cond:ProgramNode, body:ProgramNode with Reduct) extends ProgramNode with Reduct

// -- Represent an if statement
case class IfNode(cond:ProgramNode,thenBranch:ProgramNode with Reduct, elseBranch:ProgramNode with Reduct) extends ProgramNode with Reduct

// -- Represent an asynchronous thread start
case class AsynchNode(body:ProgramNode with Reduct) extends ProgramNode with Reduct

// -- Representn an atomic section
case class AtomicNode(body:ProgramNode with Reduct) extends ProgramNode with Reduct

// -- Variable assigment statement 
case class AssignNode(name:VarNode, exp:ProgramNode) extends ProgramNode with Reduct 

// -- Skip statement 
case class SkipNode extends ProgramNode with Reduct

// -- Variable node.
case class VarNode(name:String) extends ProgramNode 
 
// -- Number literals 
case class LiteralNode(number:Int) extends ProgramNode 

// --  Operators, generic, binary and unary 
case class Operator(name:String) extends ProgramNode 
case class BinaryOp(override val name:String, fst:ProgramNode, snd:ProgramNode) extends Operator(name)
case class UnaryOp(override val name:String, fst:ProgramNode) extends Operator(name)

// -- Free variable singleton
object FreeVariables {
  
  // -- Returns the free variables of the program
  def get(node:ProgramNode):Set[String] = node match {
    case BlockNode(l)    => {
      var fv:Set[String] = Set()
      for (s <- l) fv ++= get(s)
      return fv
    }
    case WhileNode(c, b)  => get(c) ++ get(b)
    case IfNode(c, i, e)  => get(c) ++ get(i) ++ get(e) 
    case AsynchNode(b)    => get(b)
    case AtomicNode(b)    => get(b)
    case AssignNode(v, e) => get(e) - v.name
    case VarNode(n)       => Set(n)
    case LiteralNode(_) | SkipNode() => Set()
    case BinaryOp(_, fst, snd) => get(fst) ++ get(snd)
    case UnaryOp(_, fst) => get(fst)
  }
  
  // -- Returns all of the variables, including bounded ones.
  def getAll(node:ProgramNode):Set[String] = node match {
    case BlockNode(l)    => {
      var fv:Set[String] = Set()
      for (s <- l) fv ++= getAll(s)
      return fv
    }
    case WhileNode(c, b)  => getAll(c) ++ getAll(b)
    case IfNode(c, i, e)  => getAll(c) ++ getAll(i) ++ getAll(e) 
    case AsynchNode(b)    => getAll(b)
    case AtomicNode(b)    => getAll(b)
    case AssignNode(v, e) => getAll(e) + v.name
    case VarNode(n)       => Set(n)
    case LiteralNode(_) | SkipNode() => Set()
    case BinaryOp(_, fst, snd) => getAll(fst) ++ getAll(snd)
    case UnaryOp(_, fst) => getAll(fst)
  }
  
}

