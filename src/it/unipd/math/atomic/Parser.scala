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
// -- Language parser class. Here is the grammar:
// 
// stmt ::= if_stmt | while_stmt  | async_stmt | atomic_stmt 
//        | 'skip'  | assign_stmt | block
// 
// if_stmt     ::= 'if' expr 'then' stmt 'else' stmt 
// while_stmt  ::= 'while' expr 'do' stmt
// async_stmt  ::= 'async' stmt 
// atomic_stmt ::= 'atomic' stmt 
// assign_stmt ::= ID ':=' expr
// block       ::= '{' stmt_list '}'
// lock        ::= 'acquire' ID | 'release' ID
//
// stmt_list   ::= stmt ';' (stmt_list | \epsilon)
// expr        ::= ID | NUM | '(' expr ')' | UNA_OP expr | expr BIN_OP expr 
// -----------------------------------------------------------------------------
class Parser(tokens:List[Token]) {
  
  var list = tokens
  
  def error(msg:String):Nothing = {
    sys.error("[line " + list.head.getLine + "] >> " + msg)
  }
  
  // -- General statement production 
  private def stmt():ProgramNode with Reduct = { 
    var s:ProgramNode with Reduct = if_stmt() 
    if (s == null) s = while_stmt() 
    if (s == null) s = asynch_stmt() 
    if (s == null) s = atomic_stmt() 
    if (s == null) s = assign_stmt()
    if (s == null) s = skip_stmt() 	
    if (s == null) s = block_stmt()
    if (s == null) s = lock_stmt()
    if (s == null) error("Could not parse statement.")
    return s
  }
  
  // -- If production parsing
  private def if_stmt():ProgramNode with Reduct = list match { 
    case Keyword("if")::rest => {
      
      val lineOfCode = list.head.getLine
      list = rest
      val e = expr()
      
      list match {
        case Keyword("then")::rest => {
          list = rest
          val tb = stmt()
          list match {
            case Keyword("else")::rest => {
              list = rest
              
              var eb = stmt()
              
              // -- added line of code count 
              val newNode = IfNode(e, tb, eb)
              newNode.codeLine = lineOfCode
              return newNode
            }
            case _ => error("Missing else branch")
          }
        }
        case _ => error("Expected then keyword")
      }
    }
    // -- Return null if we can't parse production
    case _ => null
  }
    
  // -- While statment production parsing 
  private def while_stmt():WhileNode = list match {
    case Keyword("while")::rest => {
      val lineOfCode = list.head.getLine
      list = rest
      
      val e = expr()
      list match {
        // -- match the rest
        case Keyword("do")::rest => {
          list = rest
          var s = stmt()
          
          val newNode = WhileNode(e, s) 
          newNode.codeLine = lineOfCode
          return newNode
        }
        case _ => error("Expected do keyword")
      }
    }
    case _ => null
  }
  
  // -- Asynch thread spawn production parsing
  private def asynch_stmt():AsynchNode = list match {
    case Keyword("asynch")::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      val e = stmt()
      
      val newNode = AsynchNode(e)
      newNode.codeLine = lineOfCode
      return newNode
    }
    case _ => null
  }
  
  // -- Atomic section production parsing 
  private def atomic_stmt():AtomicNode = list match {
    case Keyword("atomic")::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      val e = stmt()
      
      val newNode = AtomicNode(e)
      newNode.codeLine = lineOfCode
      return newNode
    }
    case _ => null
  }
  
  // -- Skip statement
  private def skip_stmt():SkipNode  = list match {
    case Keyword("skip")::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      
      val newNode = SkipNode()
      newNode.codeLine = lineOfCode
      return newNode
    }
    case _ => null
  }
  
  // -- Statment block 
  private def block_stmt():BlockNode = list match {
    case UnaryOpToken("{")::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      
      val newNode = BlockNode(stmt_list())
      newNode.codeLine = lineOfCode 
      return newNode
    }
    case _ => null
  }
  
  // -- Statment list 
  private def stmt_list():List[ProgramNode with Reduct] = list match {
    case UnaryOpToken("}")::rest => list = rest; Nil
    case rest => {
      val e = stmt()
      list match {
        case Separator() :: rest => list = rest; e :: stmt_list()
        case _ => error("Expected semi-colon at the end of statements") 
      }
    } 
  }
  
  // -- Lock statement
  private def lock_stmt():LockNode = list match {
    case Keyword("acquire")::Id(id)::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      
      val newNode = LockNode(VarNode(id), true)
      newNode.codeLine = lineOfCode
      return newNode
    }
    
    case Keyword("release")::Id(id)::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      
      val newNode = LockNode(VarNode(id), false)
      newNode.codeLine = lineOfCode
      return newNode
    }
    
    case _ => return null
    
  }
  
  // -- Assignment statement 
  private def assign_stmt():AssignNode = list match {
    case Id(id)::BinaryOpToken(":=")::rest => {
      val lineOfCode = list.head.getLine
      
      list = rest
      
      val newNode = AssignNode(VarNode(id), expr)
      newNode.codeLine = lineOfCode
      return newNode
    }
    
    case _ => return null
  }
  
  // -- Parsing expresion
  private def expr():ProgramNode = list match {
    // -- Parenthesized expression
    case UnaryOpToken("(")::rest => {
      list = rest
      val e = expr()
      list match {
        case UnaryOpToken(")")::rest => {
          list = rest
          return e
        }
        // -- No match for parenthesis
        case _ => error("Unmatched parenthesis")
      }
    }
    // -- Unary operations parsing 
    case UnaryOpToken(o)::rest => {
      list = rest
      return UnaryOp(o, expr)
    }
    
    // -- Id op expression
    case Id(v)::BinaryOpToken(op)::rest => {
      list = rest
      return BinaryOp(op, VarNode(v), expr)
    }
    case Id(v)::rest  => list = rest; VarNode(v)
    
    // -- number and other stuffs
    case Num(i)::BinaryOpToken(op)::rest => {
      list = rest
      return BinaryOp(op, LiteralNode(i), expr);
    }
    case Num(i)::rest => list = rest; LiteralNode(i)
    
    // -- Error 
    case _ => error("Could not parse expression")
  }
  
  // -- Parse the list of tokens
  def parse() = stmt()
  
}
