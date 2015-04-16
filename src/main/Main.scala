package main

import base.Base._
import machine.StateMachine
import machine.Machine._
import edit.WeightedTransducer
import edit.ThreeWayComposition
import edit.ShortestPath
import edit.Path

object Main {

  def s(s:Int) : String = s match {
    case A => "a"
    case B => "b"
    case C => "c"
    case D => "d"
    case E => "e"
    case _ => s.toString
  }
  
  def main(args:Array[String]){
    
	  val prop = new StateMachine{
	    addState(0,A,1)
	    addState(1,B,1)
	   // addState(2,D,3)
	   // addState(3,B,2)
	    addState(1,C,0)
	    var accept = Set(0)
	    var alphabet = Set(A,B,C)
	  }
    
	  val trace : Trace = List.make(1,List(
			  (A,At("A1")),
			  (B,At("A2")),			  
			  (C,At("A3")),
			  (A,At("A1")),			  
			  (A,At("B1")),
			  (B,At("B2")),
			  (C,At("B3")),
			  (A,At("A1")),
			  (B,At("A2")),			  
			  (C,At("A3")),
			  (A,At("A1")),			  
			  (A,At("C1")),
			  (B,At("C2")),
			  (C,At("C3"))			  

	  )).flatten
	  
	  val trace2 : Trace = List.make(2,List(
			  (A,At("A1")),
			  (B,At("A2")),			  
			  (C,At("A3")),
			  (A,At("A1")),			  
			  (A,At("B1")),
			  (B,At("B2")),
			  (C,At("B3"))			  
	  )).flatten	  
	  
	  val wp = WeightedTransducer(prop)
	  val wt = WeightedTransducer(trace)
	  val we = WeightedTransducer(Set(A,B,C))
	  /*
	  val wt2 = WeightedTransducer(trace2)
	  val result2 = ThreeWayComposition(wt,we,wp)
	  val result1 = ThreeWayComposition(wt2,we,wp)
	  
	  result1.delta.foreach{ case (s,m) =>
	  
	    val rw = result2.delta(s).map{
	      case (((a,c1),(b,c2)),x) => (((a,c1.replace("C","B")),(b,c2)),x) // c2 will be null
	    }.toSet
	    
	    val d1 = m.toSet -- rw
	    val d2 = rw -- m.toSet
	    if(!d1.isEmpty || !d2.isEmpty){
	      println(d1+"\t\t"+d2)
	    }
	  
	  }
	  System.exit(0)*/
	  
	  val result = ThreeWayComposition(wt,we,wp)	  
	  println("Composition done")
	  val min_estimate = ShortestPath(result)
	  println("min_estimate: "+min_estimate)
	  
	  val (w,paths) = ShortestPath(0,min_estimate,result)
	  //val (w,paths) = ShortestPath(15,20,result)
	  
	  println("-----------------")
	  println(trace map (_._1) map s mkString("."))
	  println("-----------------")
	  println("weight: "+w)
	  println("-----------------")
	  var min_edit_size = Int.MaxValue
	  val edits = paths map { path => 
	    var m : Map[(Event,Event),Int] = Map()
	    path.filter(_._3==1).foreach{ case a =>
	      val b = (a._1,a._2)
	      m += (b -> (m.getOrElse(b,0)+1))
	    }
	    val edit = m.toSet
	    if(edit.size<min_edit_size) min_edit_size = edit.size
	    (edit,path)
	  }
	  val min_edits = edits.filter{_._1.size==min_edit_size}
	  min_edits foreach{ 
	    case (edit,path) => {
	      //edit foreach println
	      path foreach {
	        case (in,out,w) =>
	          if(w==0){
	            print(s(in._1)+"["+in._2+"]")
	          }else{
	            (in,out) match {
	              case (_,(-1,_)) => print("-"+s(in._1)+"["+in._2+"]")
	              case ((-1,_),_) => print("+"+s(out._1)+"["+in._2+"]")
	              case ((a,_),(b,_)) => print(s(a)+"/"+s(b)+"["+in._2+"]")
	            }
	          }
	          print(".")
	      }
	      println
	    }
	    println("-----------------")
	  }
	  
	  
	  //println("\n\n\n\n\n")
	  //println(result)
	  
  }
  
  
}