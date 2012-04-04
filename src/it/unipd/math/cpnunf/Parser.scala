/** 
 *  Copyright (C) 2012  Alessandro Bruni
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
package it.unipd.math.cpnunf

import scala.util.parsing.combinator._
import java.io.PrintStream

// -----------------------------------------------------------------------------
// -- Parser class. Parse a net from file.  
// -----------------------------------------------------------------------------
class Parser (log: PrintStream) extends JavaTokenParsers {
    private var place_count = 0
    private var trans_count = 0

    // Complete llnet file structure, including optional parts
    def llnet =
        ((header_n2 | header_n | header_pt) ~ opt(blocks) ~ places ~ transitions ~ opt(ptr) ~
                tr_pl ~ pl_tr ~ opt(ra) ~ opt(ptp) ~ opt(ppt) ~ opt(tx)) ^^
        { case header ~ blocks ~ places ~ transitions ~ ptr ~ tr_pl ~ pl_tr ~ ra ~ ptp ~ ppt ~ tx =>
            log.println("%d places and %d transitions parsed".format(place_count, trans_count))
            new PetriNet(places, transitions, tr_pl, pl_tr, ra.getOrElse(List())) }
    
    // Useful definitions
    private def number = wholeNumber ^^ (n => n.toInt)
    private def string = stringLiteral ^^ (s => s.toString().substring(1, s.length-1))

    // Parts definition
    private def header_n = "PEP" ~> "PetriBox" ~> "FORMAT_N" ~> default_val
    private def header_n2 = "PEP" ~> "PetriBox" ~> "FORMAT_N2" ~> default_val
    private def header_pt = "PEP" ~> "PTNet" ~> "FORMAT_N" ~> default_val
    
    private def default_val = opt(default_pl) ~ opt(default_tr) ~ opt(default_pt)
    private def default_pl = "DPL" ~> "s" ~> number ~ ("n" ~> number ~ ("@" ~> number ~ ("t" ~> number)))
    private def default_tr = "DTR" ~> "s" ~> number ~ ("n" ~> number ~ ("@" ~> number ~ ("t" ~> number)))
    private def default_pt = "DPT" ~> "w" ~> number ~ ("t" ~> number)

    private def blocks = "BL" ~> rep(item) ^^ (_ => ()) // Ignore them
    private def places = "PL" ~> rep(place)
    private def place = item ^^ { case id ~ name ~ pos ~ args =>
        place_count += 1
        (new Place(id.getOrElse(place_count), name, pos.getOrElse(null), args), args)
    }
    private def transitions = "TR" ~> rep(transition)
    private def transition = item ^^ { case id ~ name ~ pos ~ args =>
        trans_count += 1
        (new Transition(id.getOrElse(trans_count), name, pos.getOrElse(null), args), args)
    }
    private def ptr = "PTR" ~> rep(item) ^^ (_ => ()) // Ignore them
    
    private def tr_pl = "TP" ~> rep(lt_link)
    private def pl_tr = "PT" ~> rep(gt_link)
    private def ra = "RA" ~> rep(lt_link)
    private def ptp = "PTP"
    private def ppt = "PPT"
    private def tx = "TX" ~> (".*".r)
    
    // Mandatory information
    private def id = number
    private def name_string = string
    private def pos = coord_x ~ ("@" ~> coord_y) ^^ { case x ~ y => (x, y) }

    // Optional information
    private def coord_x = number 
    private def coord_y = number
    private def rel_pos = "n" ~> pos ^^ { case pos => ("rel-pos", pos) }
    private def meaning_string = "b" ~> string ^^ { case s => ("meaning", s) }
    private def image_string = "u" ~> string ^^ { case s => ("image", s) }
    private def rel_meaning_pos = "a" ~> pos ^^ { case pos => ("rel-meaning-pos", pos) }
    private def init_mark = "M" ~> number ^^ { case m => ("init-mark", m) }
    private def cur_mark = "m" ~> number ^^ { case m => ("cur-mark", m) }
    private def capacity = "k" ~> number ^^ { case k => ("capacity", k) }
    private def ref_string = "R" ~> string ^^ { case s => ("ref-str", s) }
    private def entry_place = "e" ^^ (Unit => ("entry", true))
    private def exit_place = "x" ^^ (Unit => ("exit", true))
    // what is this?
    private def v = "v" ~> number ^^ { case n => ("v", n) }
    private def P = "P" ~> string ^^ { case s => ("P", s) }
    private def S = "S" ^^ (Unit => ("S", true))

    // Item definition
    private def item = (opt(id) ~ name_string ~ opt(pos) ~ (rep(rel_pos | meaning_string | image_string | rel_meaning_pos | init_mark
        | cur_mark | capacity | ref_string | v | P | S | entry_place | exit_place) ^^
        { case list => list.toMap }))

    private def lt_link = number ~ ("<" ~> number <~ opt(v)) ^^ { case a ~ b => (a, b) }
    private def gt_link = number ~ (">" ~> number <~ opt(v)) ^^ { case a ~ b => (a, b) }
}
