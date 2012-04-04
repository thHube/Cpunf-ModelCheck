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

import java.io._
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.Stack

// -----------------------------------------------------------------------------
// -- Class Unfolder, unfolds a petri net.
// -----------------------------------------------------------------------------
class Unfolder(val net: PetriNet) {
  
    // -- Create an history from preset, read arcs and transitions
    def make_history(preset: Set[EnrichedCondition], readarcs: Set[EnrichedCondition], trans: Transition): History = {
        // -- Get events in preset. 
        val evt_pre = preset.map(_.c)
        // -- Get events in readarcs
        val evt_read = readarcs.map(_.c)
        
        // -- For each event in transition preimage
        for (evt <- trans.preimage) {
            if (evt.preset == evt_pre && evt.readarcs == evt_read) {
                // -- Debug print ----------------------------------------------
                if (false && Unfolder.DEBUG)
                    Unfolder.log.println("existing event found")
                // ------------------------------------------------------------
                return new History(evt, preset, readarcs)
            }
        }
        if (false && Unfolder.DEBUG)
            Unfolder.log.println("inserting new event")
        return new History(new Event(trans, evt_pre, evt_read), preset, readarcs)
    }
    
    // -------------------------------------------------------------------------
    // -- Update possible extension. Seems lazy but not sure.
    // -------------------------------------------------------------------------
    def update_pe(h: History, pe: PriorityQueue[History]) {
        // Get the enabled transitions
        val trans_enabled = (h.event.postset.map(s => s.image.postset ++ s.image.readarcs).reduceOption(_ ++ _).getOrElse(Set())// FIXME check this
                ++ h.event.readarcs.map(_.image.postset).reduceOption(_ ++ _).getOrElse(Set()))
                
        // -- Loop through all enabled transitions
        for (trans <- trans_enabled) {
            val pre = h.concurrent.filter(σ => trans.preset contains σ.c.image)
            val read = h.concurrent.filter(σ => σ.isCausal && (trans.readarcs contains σ.c.image))
            if (pre.map(_.c.image) == trans.preset && read.map(_.c.image) == trans.readarcs) {
                val preList = pre.toList.sort(_ < _)
                val readList = read.toList.sort(_ < _)
                var preSet: Set[EnrichedCondition] = null
                var readSet: Set[EnrichedCondition] = null
                
                // -- 
                def choosePre(in: List[EnrichedCondition], out: List[EnrichedCondition]) {
                    if (in.isEmpty) {
                        preSet = out.toSet
                        // -- Check that every place in the original net is 
                        // -- covered by one enriched condition in the unfolding. 
                        // -- Check also that each non causal enriched condition 
                        // -- has its causal history inserted in the preset.
                        if (preSet.map(_.c.image) == trans.preset &&
                            preSet.forall(s1 => (s1 isCausal)     || 
                            preSet.exists(s2 => s2.c == s1.c && (s2 isCausal)))) {
                                chooseRead(readList, List())
                        }
                        return
                    }
                    
                    val current = in.head
                    if (out.forall(_ concurrentWith current))
                        choosePre(in.tail, current :: out)
                    choosePre(in.tail, out)
                }
                
                // -- 
                def chooseRead(in: List[EnrichedCondition], out: List[EnrichedCondition]) {
                    if (in.isEmpty) {
                        readSet = out.toSet
                        // -- Check that every place in the original net is 
                        // -- covered by one enriched condition in the unfolding. 
                        // -- Check also that the history that has just been 
                        // -- inserted appears at least once in the preset of 
                        // -- this possible extension. 
                        if (readSet.map(_.c.image) == trans.readarcs &&
                           (preSet.exists(_.h == h)                  || 
                            readSet.exists(_.h == h))) {
                            
                            // -- Check closure of the generated history over 
                            // -- the subsumption relation
                            if (((
                                    preSet.map(_.sub) .reduceOption(_ ++ _).getOrElse(Set()) ++
                                    readSet.map(_.sub).reduceOption(_ ++ _).getOrElse(Set()) --
                                    preSet
                                  ).map(_.causal) intersect preSet
                                ).isEmpty) {
                                // -- Enqueue history
                                pe.enqueue(make_history(preSet, readSet, trans))
                            }
                        }
                        return
                    }
                    
                    val current = in.head
                    if (out.forall(_ concurrentWith current) && preSet.forall(_ concurrentWith current))
                        chooseRead(in.tail, current :: out)
                    chooseRead(in.tail, out)
                }

                choosePre(preList, List())
            }
        }
    }
    
    // -- Main unfoling procedure
    def unfold(): Unfolding = {
        // -- Initialize data 
        val pe = PriorityQueue[History]()
        val unfolding: Unfolding = new Unfolding(net)
        // -- Update possible extensions
        update_pe(unfolding.root_history, pe)
        // -- Loop until it is possible to extend.
        while (!pe.isEmpty) {
            // -- Debug print
            if (Unfolder.DEBUG) Unfolder.log.println(pe.size)
            
            val h = pe.dequeue
            // -- Check if the unfolding is cutoff
            if (!unfolding.isCutoff(h)) {
                // -- Debug print
                if (Unfolder.DEBUG) Unfolder.log.println("inserting h%s[%s]; unfolding size: %s".format(h.id, h.event.name, unfolding.histories.size))
                
                // -- add history and update possible extension
                unfolding.addHistory(h)
                update_pe(h, pe)
            } else {
                // -- Debug print
                if (Unfolder.DEBUG) Unfolder.log.println("cutoff h%s[%s]".format(h.id, h.event.name))
                // -- add cutoff.
                unfolding.addCutoff(h)
            }
        }
        
        // -- LOG print -------------------------------------------------------- 
        Unfolder.log.println("%d events, %d conditions and %d histories (%d cutoffs, %d total)".format(
                unfolding.places.size, unfolding.transitions.size, unfolding.histories.size, unfolding.cutoffs.size,
                unfolding.histories.size + unfolding.cutoffs.size))
        Unfolder.log.println(History.count_comp, History.count_comp_equal)
        
        // -- Return newly created unfolding -----------------------------------
        unfolding
    }
}

// -----------------------------------------------------------------------------
// -- Static data for unfolder class.
// -----------------------------------------------------------------------------
object Unfolder {
    // -- Debug, log and output file
    val DEBUG = false
    var log: PrintStream = null
    var out: PrintStream = null
    
    // -- Help string
    val usage = """
unfolder [parameters] file_name

Parameters:
    -dot         Dot output (default)
    -ll          LLnet output
    -asp         Answer Set Programming output
    -stdin       Read from standard input
    -convert     No net unfolding, just output the original net
    -histinf     Include history information
                 (only applies to ll nets with applied unfolding)
    -o file_name Output to file
"""
    
    // -- Output type
    val OUTPUT_DOT = 0
    val OUTPUT_LLNET = 1
    val OUTPUT_ASP = 2
    // -- Empty net.
    var net: PetriNet = null
    
    // -- Program entry point.
    def main(args: Array[String]): Unit = {
        
        var file_name: String = null
        var stdin: Boolean = false
        var output_fmt: Int = 0
        var out_file: String = null
        var unfold: Boolean = true
        var histinf: Boolean = false
        
        def parse(args: List[String]) {
            args match {
                case "-dot"     :: list => output_fmt = OUTPUT_DOT; parse(list)
                case "-ll"   :: list => output_fmt = OUTPUT_LLNET; parse(list)
                case "-asp"   :: list => output_fmt = OUTPUT_ASP; parse(list)
                case "-stdin"   :: list => stdin      = true; parse(list)
                case "-convert" :: list => unfold     = false; parse(list)
                case "-histinf" :: list => histinf    = true; parse(list)
                case "-o" :: fn :: list => out_file   = fn; parse(list)
                case string     :: list => file_name  = string; parse(list)
                case Nil => return
            }
        }
        parse(args.toList)
        
        log = System.err
        out = if(out_file != null) new PrintStream(new FileOutputStream(out_file))
            else System.out
        log.println("Contextual Petri Net Unfolder")
        
        if (file_name == null && !stdin) {
            log.println("File name not specified")
            log.println(usage)
            sys.exit(1)
        }
        
        val input = if (stdin) new InputStreamReader(System.in)
        else try {
            log.println("Opening file %s".format(file_name))
            new FileReader(file_name)
        } catch {
            case _: FileNotFoundException =>
                log.println("File not found")
                sys.exit(1)
        }
        
        var t_start = System.currentTimeMillis
        val parser = new Parser(log)
        val parsed = parser.parseAll(parser.llnet, input)
        if (!parsed.successful) {
            log.println("Input parsing failed")
            log.println(parsed)
            sys.exit(1)
        }
        net = parsed.get
        var t_end = System.currentTimeMillis
        log.println("Read time: " + (t_end - t_start) + "ms")
        t_start = t_end
        if (unfold) {
            val result = new Unfolder(net).unfold()
            t_end = System.currentTimeMillis
            log.println("Unfold time: " + (t_end - t_start) + "ms")
            t_start = t_end
            print(result, output_fmt, histinf)
        } else {
            print(net, output_fmt)
        }
        t_end = System.currentTimeMillis
        log.println("Write time: " + (t_end - t_start) + "ms")
    }
    
    def print(result: Net[_ <: Node[_ <: Node[_]], _ <: Node[_ <: Node[_]]], output_fmt: Int, histinf: Boolean = false) = {
        if (output_fmt == OUTPUT_DOT)
            DotWriter.write(out, result)
        else if (output_fmt == OUTPUT_LLNET)
            LLWriter.write(out, result, histinf)
        else if (output_fmt == OUTPUT_ASP)
            AspWriter.write(out, result.asInstanceOf[Unfolding])
    }
}
