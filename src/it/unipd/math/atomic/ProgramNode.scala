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
case class BlockNode(stmt:List[ProgramNode]) extends ProgramNode

// -- Represent a while loop
case class WhileNode(cond:ProgramNode, body:ProgramNode) extends ProgramNode

// -- Represent an if statement
case class IfNode(cond:ProgramNode,thenBranch:ProgramNode, elseBranch:ProgramNode) extends ProgramNode

// -- Represent an asynchronous thread start
case class AsyncNode(body:ProgramNode) extends ProgramNode

// -- Representn an atomic section
case class AtomicNode(body:ProgramNode) extends ProgramNode

// -- Variable assigment statement 
case class AssignNode(name:VarNode, exp:ProgramNode) extends ProgramNode 

// -- Skip statement 
case class SkipNode extends ProgramNode

// -- Variable node.
case class VarNode(name:String) extends ProgramNode
 
// -- Number literals 
case class LiteralNode(number:Int) extends ProgramNode

// --  Operators, generic, binary and unary 
case class Operator(name:String) extends ProgramNode 
case class BinaryOp(override val name:String, fst:ProgramNode, snd:ProgramNode) extends Operator(name)
case class UnaryOp(override val name:String, fst:ProgramNode) extends Operator(name)
