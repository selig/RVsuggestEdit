package main

import base.Base._
import machine.StateMachine
import machine.Machine._
import edit.WeightedTransducer
import edit.ThreeWayComposition
import edit.ShortestPath
import edit.Path
import scala.util.Random
import edit.SuggestedPaths

object Example {

  val OPEN = A
  val USE = B
  val CLOSE = C
  
  val prop = new StateMachine{
	    addState(0,A,1)
	    addState(1,B,1)
	    addState(1,C,0)
	    var accept = Set(0)
	    var alphabet = Set(A,B,C)
	  }  
  
  // The expanded version of the trace chunk given in the introduction
  def trace_chunk : Trace = {
    val Abit : Trace = (OPEN,At("A1"))::List.make(20,(USE,At("A2"))):::List((CLOSE,At("A3")),(OPEN,At("A1")))
    val Bbit : Trace = (OPEN,At("B1"))::List.make(25,(USE,At("B2"))):::List((CLOSE,At("B3")))
    val Cbit : Trace = (OPEN,At("B1"))::List.make(25,(USE,At("B2"))):::List((CLOSE,At("B3")))
    
    Abit:::Bbit:::Abit:::Cbit
  }
  
  def good_chunk(len:Int) : Trace = {
    if(len<3) List()
    else (OPEN,At("D1"))::List.make(len-2,(USE,At("D2"))):::List((CLOSE,At("D3")))
  }
  
  def trace_chunk_no_dif : Trace = {
    var c = 0
    trace_chunk.map{case (a,At(s)) => c+=1;(a,At("p"+c))}
  }  
  
   def check(trace:Trace) {
      
      val start = System.currentTimeMillis()
	  val wp = WeightedTransducer(prop)
	  val wt = WeightedTransducer(trace)
	  val we = WeightedTransducer(Set(OPEN,USE,CLOSE))
	  val mid = System.currentTimeMillis()
	  val result = ThreeWayComposition(wt,we,wp)
	  val midmid = System.currentTimeMillis()
	  val min_estimate = ShortestPath(result)
	  val midmidmid = System.currentTimeMillis()
	  val (edits,actual_min) = SuggestedPaths(result,100,true,min_estimate)
	  val end = System.currentTimeMillis()
	  //println("Produced "+edits.size+" edits, "+(end-midmid))
	  println(trace.length+","+min_estimate+","+actual_min+","+edits.size+","+
	      (end-start)+","+(mid-start)+","+(midmid-mid)+","+(midmidmid-midmid)+","+(end-midmidmid))
	}  
 
    def check_chunks(chunks:Int,chunk:Trace) = {
      val t : Trace = List.make(chunks,chunk).flatten
      check(t)
    }
    
    def check_scaling(chunks:Int,m:Int){
      val tc : Trace = trace_chunk:::good_chunk(m)
      check_chunks(chunks,tc)
    }
   
   def compare_for_points(chunks:Int) {
     val blank_trace = trace_chunk_no_dif
     val trace = trace_chunk
     check_chunks(chunks,blank_trace)
     check_chunks(chunks,trace)
   }
   
   def main(args:Array[String]){
     
     (1 to 10).foreach{_ =>       
     	//(5 to 5).foreach(i => compare_for_points(i))
       List(1,3,6).foreach(c => List(0,900,2400).foreach(m => check_scaling(c,m)))
     }
     
   }
   
}