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

import scala.util.matching.Regex

import java.util.StringTokenizer
import java.io.File

// -----------------------------------------------------------------------------
// -- Token case class hierarchy 
// -----------------------------------------------------------------------------
abstract class Token {
  private var linenum:Int = 0
  def setLine(line:Int):Token = { linenum = line; return this }
  def getLine() = linenum
}

// -----------------------------------------------------------------------------
// -- AVAILABLE TOKENS 
// -----------------------------------------------------------------------------
case class Keyword(s:String) extends Token
case class Id(s:String) extends Token
case class Num(i:Int) extends Token
case class UnaryOpToken(s:String) extends Token
case class BinaryOpToken(s:String) extends Token
case class Separator() extends Token

// -----------------------------------------------------------------------------
// -- Scanner class. Reads one by one tokens 
// -----------------------------------------------------------------------------
class Scanner(input:String) {
  
  // -- List of string token generated by StringTokenizer class and line num
  private var substr = new StringTokenizer(input, " \t\n!(){};+-<>*/=:#", true) 
  private var lineNumber = 0
  
  // ---------------------------------------------------------------------------
  // -- Regex used for lexical analysis
  // ---------------------------------------------------------------------------
  // -- Keywords regex
  private val keyword = new Regex("""(while|if|then|else|skip|asynch|atomic|do)""", "keyword")
  
  // -- Identifier regexp 
  private val ids     = new Regex("""([a-zA-Z]\w*)""", "id")
  
  // -- Integer number regexp 
  private val numbers = new Regex("""(\d+)""", "num")
  
  // -- Binary operators regexp   
  private val binOp   = new Regex("""([+-<>\*/=]|!=|:=)""", "op")
  
  // -- Unary operators regexp
  private val unaOp   = new Regex("""([!(){}])""", "op")
  
  // -- Spacers 
  private val whitesp = new Regex("""(\s+)""", "sp")
  private val newLine = new Regex("""(\n)""", "endl")
  
  
  // -- Convert the list of string into tokens 
  private def tokenize(list:List[String]):List[Token] = list match {
    // -- Commnets 
    case "#"::rest        => {
      var list = rest
      while (list.head != "\n") {
        list = list.tail
      }
      lineNumber += 1
      tokenize(list.tail)
    }   
    // -- Separators
    case ";"::rest        => Separator().setLine(lineNumber) :: 
                             tokenize(rest)
    // -- Keywords 
    case keyword(k)::rest => Keyword(list.head).setLine(lineNumber) :: 
                             tokenize(rest)
    // -- Ids 
    case ids(id)::rest    => Id(list.head).setLine(lineNumber) :: 
                             tokenize(rest)
    // -- Numbers
    case numbers(n)::rest => Num(list.head.toInt).setLine(lineNumber) :: 
                             tokenize(rest)
    // -- Special cases for binary operators
    case ":"::"="::rest   => BinaryOpToken(":=").setLine(lineNumber) :: 
                             tokenize(rest)
    case "!"::"="::rest   => BinaryOpToken("!=").setLine(lineNumber) :: 
                             tokenize(rest)
    // -- Binary operators
    case binOp(op)::rest  => BinaryOpToken(list.head).setLine(lineNumber) :: 
                             tokenize(rest)
    // -- Unary operators 
    case unaOp(op)::rest  => UnaryOpToken(list.head).setLine(lineNumber) ::
                             tokenize(rest) 
    // -- Whitespaces and other stuff
    case newLine(n)::rest => lineNumber += 1; tokenize(rest)
    case whitesp(s)::rest => tokenize(rest)
    case _ :: rest        => println("Unrecognized token, skipping"); 
                             tokenize(rest)
    case Nil 			  => Nil
  }
  
  // -- Convert tokenizer to a list of string 
  private def tokenizerToList():List[String] = {
    var strList:List[String] = List()
    while (substr.hasMoreTokens()) {
      strList ++= List(substr.nextToken())
    }
    return strList
  }
  
  // -- Convert to tokens the given list 
  def lex():List[Token] = tokenize(tokenizerToList())
}

// -----------------------------------------------------------------------------
// -- Build a parse tree from program in the given file.
// -----------------------------------------------------------------------------
class ParseTreeBuilder(filename:String) {
  private val scanner   = new Scanner(scala.io.Source.fromFile(filename).mkString)
  private val parser    = new Parser(scanner.lex)
  
  // -- Parse the given file
  def getTree():ProgramNode = {
    try {
      val parseTree = parser.parse()
      return parseTree
    } catch {
      case e:RuntimeException => {
        println("(!) Compilation ended unexpectly ")
        println(e.getMessage())
      }
    }
    return null
  }
}

