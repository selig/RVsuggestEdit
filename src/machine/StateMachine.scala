package machine

import Machine._

/*
 * Assume prefix closed
 * Assume state machine uses 2 symbols only
 * 
 * At some point replace state machine with a combined checker with output function on states
 * 
 */

object Machine {
  val A = 0
  val B = 1
  val C = 2  
  val D = 3
  val E = 4
  val F = 5
  val G = 6
  val H = 7
}

trait Machine {
  
  var name = ""
  override def toString = name    
  
  val init = 0
  val fail = -1
  
  // (state,symbol) -> state
  var delta : Map[(Int,Int),Int] 
  
  def next(symbol:Int,state:Int) : Int = {
    delta.getOrElse((state,symbol),fail)
  }
  
  def accepts(trace:List[Int]) : Boolean = {
    var state = init
    for(e <- trace){
      if(alphabet contains e)
    	  state = next(e,state)
    }
    accept.contains(state)
  }  
  
  def failed(state:Int) : Boolean
  def okay(state:Int) : Boolean
 
  var alphabet : Set[Int]
  var accept : Set[Int]
  
  private var reach : Set[Int] = null
    def make_reach(s:Int){
      val n = delta.filter{case ((t,_),r) => t==s && !(reach contains r)}.map(_._2)   
      reach ++= n
      n foreach make_reach
    }
  
  // at the moment just let it be empty if there are no transitions or accepting states
  def isTrivial : Boolean = {
    //val states = delta.map{case ((s1,_),s2) => Set(s1,s2)}.flatten.toSet
    //if(reach==null){
    //	reach = Set[Int]()
    //	make_reach(init)
    //}      
    	  
    // we now do sm reduction, so we don't need to use reach
    
    delta.isEmpty || accept.isEmpty //|| !reach.exists(s => accept.contains(s))
  }
    
  	override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode((delta,accept));
  	override def equals(a:Any) = this.hashCode == a.hashCode
  
}


trait StateMachine extends Machine {
  
  var delta : Map[(Int,Int),Int] = Map()
  def addState(p1:Int,s:Int,p2:Int){ delta += (((p1,s),p2)) }
  
  def used_alphabet = delta.map{case ((_,a),_) => a}.toSet
  
  // doesn't seem to be working
  implicit def lift_addState(p1:Int) = new {
    def -(s:Int) = new {
      def ->(p2:Int){
        addState(p1,s,p2)
      }
    }
  }
    
  def okay(state:Int) = (state!=fail)//accept.contains(state)
  def failed(state:Int) = (state==fail)
  
  var accept : Set[Int]
   
  import scala.collection.mutable.HashMap
  val lookup : HashMap[Map[Int,Int],StateMachine] = new HashMap[Map[Int,Int],StateMachine]()
  def apply(map:Map[Int,Int]) : StateMachine = {
    if(lookup contains map) return lookup(map)
    val save_accept = accept
    val save_alphabet = alphabet
    val save_delta = delta
    val outer = this
    val result = new StateMachine {     
    	var accept : Set[Int] = save_accept
    	var alphabet : Set[Int] = save_alphabet.map(map)
    
    	delta = save_delta.map{case ((s1,e),s2) => ((s1,map(e)) -> s2)}
    	name = outer.name+map.mkString("[",",","]")
    }
    lookup.put(map,result)
    result
  }
  
  def reduce : StateMachine = {
   
    val Q = delta.map{case ((p,_),q) => Set(p,q)}.flatten.toSet+fail
    
    var P : Set[Set[Int]] = Set(accept,Q)
    var W : Set[Set[Int]] = Set(accept)
       
    
    while(!W.isEmpty){
      
      val A = W.head
      W = W.tail
      alphabet.foreach{ a =>
        
        val X = delta.filter{ case ((p,b),q) => b==a && (A contains q)}.map{case ((p,_),_) => p}.toSet
        P.foreach{ Y =>
          	val intersect = (X & Y)
        	if(!intersect.isEmpty){
        	  val diff = Y--X
        	  P -= Y;P+=intersect;P+=diff
        	  if(W contains Y){
        	    W -= Y;W+=intersect;W+=diff
        	  }
        	  else{
        	    if(intersect.size <= diff.size) W+=intersect else W+=diff        	    
        	  }
        	}
        }
      }
    }
    // P is now a set of equivalence classes of states, rewrite delta and accept accordingly
    //println(P)
    
    // both are from old to new
    var c = 0
    var ns_set_lookup : Map[Set[Int],Int] = P.map{case ss =>
    	if(ss contains init) (ss -> 0)
    	else{
    	  c+=1
    	  (ss -> c)
    	}
    }.toMap
    
    val i_lookup = ns_set_lookup.map{case (ss,i) => ss.map(s => (s,i))}.flatten.toMap
    
    val new_accept = accept.map(i_lookup)    
    val new_delta : Map[(Int,Int),Int] = delta.map{case ((p,a),q) => ((i_lookup(p),a),i_lookup(q))}
    val old_alphabet = alphabet
    val old_name = name
    
    new StateMachine{
      delta = new_delta
      var accept : Set[Int]= new_accept
      var alphabet : Set[Int] = old_alphabet
      name = old_name
    }
  }  
  
  
}

object TestStateMachineReduce extends App {
  
  val sm = new StateMachine{
    
    val a = 0
    val b = 1
    delta += ((0,a) -> 1)
    delta += ((0,b) -> 2)
    delta += ((1,a) -> 3)
    delta += ((3,b) -> 1)  
    delta += ((2,a) -> 4)
    delta += ((4,b) -> 2)   
    var accept = Set[Int]()//Set(3,4)
    var alphabet = Set(a,b)
  }
  
  val red_sm = sm.reduce
  
  println(red_sm.accept)
  red_sm.delta foreach println
  
}