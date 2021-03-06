package edit

import base.Base._
import machine._
import Machine._

trait WeightedTransducer {

  val init = 0
  
  // we'll make it deterministic for input/output pair
  // delta : state -> (input,output) -> (weight,state)
  var delta : Map[Int,Map[(Event,Event),(Int,Int)]]  
  var accept : Set[Int]
  
  override def toString = {
    var ret = "delta:\n"
    delta.foreach{ case (s,m) => ret+=s+":\n"; m.foreach{ ret+="\t"+_.toString+"\n"}}
    ret += "accept: "+accept
    ret+"\n"
  }
  
  val epsilon : MethodCall = -1
  
  def toStateMachine(lookup_in: Map[(Event,Event,Int),Int]) : (StateMachine,Map[(Event,Event,Int),Int]) = {
    val outer = this
    var j = lookup_in.size
    var lookup : Map[(Event,Event,Int),Int]= lookup_in
    def symbol(i:Event,o:Event,w:Int) : Int = {
      if(lookup contains (i,o,w)) lookup((i,o,w))
      else{
        val r = j
        j+=1
        lookup += ((i,o,w) -> r)
        r
      }
    }
    (new StateMachine{
      var accept : Set[Int] = outer.accept
      var alphabet : Set[Int] = outer.delta.map{case (_,m) => m.map{case ((i,o),(w,_)) => symbol(i,o,w)}}.flatten.toSet
      
      outer.delta.foreach{case (s1,m) => m.map{case ((i,o),(w,s2)) => delta += ((s1,symbol(i,o,w))->s2)}}
      
    },lookup)
  }
  
  	override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode((delta,accept));
  	override def equals(a:Any) = {
  	  if(a.isInstanceOf[WeightedTransducer]){
  	    val other = a.asInstanceOf[WeightedTransducer]
  	    (delta equals other.delta) && (accept equals other.accept)
  	  }
  	  else false
  	}
  
  	def reduce : WeightedTransducer = {
  	  // trim unreachable states
  	  var reach : Map[Int,Set[Int]] = Map()
  	  
  	  def make_reach(s:Int,X:Set[Int]) : Set[Int] = {
  	    val next = delta(s).map(_._2._2).filterNot(X contains _).toSet
  	    val XX = X++next
  	    val R = (next map{t => make_reach(t,XX)} flatten)
  	    reach += (s -> R)
  	    next
  	  }
  	  make_reach(init,Set[Int]())
  	  
  	  var seen = reach.filter{case (_,ss) => !(ss & accept).isEmpty}.keySet
  	  
  	  var new_delta = delta.filterKeys(seen contains _)
  	  new_delta = new_delta.map{case (s,m) =>  	    
  	    val new_m = m.filter(seen contains _._2._2)  	    
  	    (s,new_m)
  	  }
  	  val new_accept = accept.filter(seen contains _)
  	  
  	  new WeightedTransducer{  	  
  	    var delta = new_delta
  	    var accept = new_accept
  	  }
  	}
  	
}

object WeightedTransducer{
  
  import Machine._
  
  def apply(trace:List[Event]) : WeightedTransducer = new WeightedTransducer{
    var delta : Map[Int,Map[(Event,Event),(Int,Int)]] = Map()
    
    var state = 0
    var last_cp : CodePoint = Beginning
    
    for(e : Event <- trace){
      val (_,cp) = e
      // transition
      var m = Map((e,e) -> (0,state+1))
      // self-looping
      val self = (epsilon,Between(last_cp,cp))
      m += ((self,self) -> (0,state))
      // add
      delta += (state -> m)
      // update states
      state+=1
      last_cp = cp
    }
    
    var accept : Set[Int] = Set(state)
  }
  
  def apply(machine:StateMachine) : WeightedTransducer = new WeightedTransducer{
    var delta : Map[Int,Map[(Event,Event),(Int,Int)]] = Map()
    
    var state = 0
    
    for(((s1,e),s2) <- machine.delta){
      
      val self = (epsilon,null)
      var s1_map : Map[(Event,Event),(Int,Int)] = Map((self,self) -> (0,s1))
      if(delta.contains(s1)) s1_map = delta(s1)
      
      val ee = (e,null)
      // transition
      s1_map += ((ee,ee) -> (0,s2))
      
      delta += (s1 -> s1_map)
    }
      
    var accept : Set[Int] = machine.accept
  }
  
  def apply(alphabet:Set[MethodCall]) = new WeightedTransducer{
    var m : Map[(Event,Event),(Int,Int)] = Map()
    
    //matching
    alphabet.foreach{a => m+= ( ((a,null),(a,null)) -> (0,init))}
    //replacement
    alphabet.foreach{a => (alphabet-a).foreach{b => 
      m+= ( ((a,null),(b,null)) -> (1,init))
      m+= ( ((b,null),(a,null)) -> (1,init))
    }}
    //deletion
    alphabet.foreach{a => m+= ( ((a,null),(epsilon,null)) -> (1,init))}
    //insertion
    alphabet.foreach{a => m+= ( ((epsilon,null),(a,null)) -> (1,init))}
    
    var accept = Set(init)
    var delta = Map(init -> m)
  }
  
}

object ThreeWayComposition {
  
  // doesn't look like we need this - it's probably an optimisation
  val x = 2
  val epsilon_filter : Map[Int,Set[(Int,Int,Int,Int)]] = Map(
      
      /*
      (0,0,1),
      (0,1,0)
      (0,1,1),
      (1,0,0),
      (1,0,1),
      (1,1,0),
      (1,1,1),
      (x,x,x),
      (x,x,1),
      (x,x,0),
      (x,1,x),
      (x,0,0),
      (x,0,1),
      (x,1,0),
      (x,1,1),
      (0,x,x),
      (0,x,0)
      (1,x,x),
      
      
      */
      
      
  0 -> Set(
          // 0
		  (x,x,1,0),
		  (x,x,x,0),
		  (1,x,x,0),
		  (1,1,1,0),
		  // 1
		  (0,0,1,1),
		  // 2
		  (0,x,x,2),
		  (0,1,1,2),
		  // 3
		  (0,1,0,3),
		  // 4
		  (x,x,0,4),
		  (1,1,0,4),
		  // 5
		  (1,0,0,5),
		  // 6
		  (1,0,1,6)
	),
  1 -> Set(
		  // 0
		  (x,x,x,0),
		  // 1
		  (0,0,1,1),
		  // 2
		  (0,x,x,2)
  ),
  2 -> Set(
		  // 0
		  (x,x,1,0),
		  (x,x,x,0),
		  // 1
		  (0,0,1,1),
		  // 2
		  (0,1,1,2),
		  (0,x,x,2),
		  // 3
		  (0,1,0,3),
		  // 4
		  (x,x,0,4)
  ),
  3 -> Set(
		  // 0
		  (x,x,x,0),
		  // 2
		  (0,x,x,2),
		  // 3
		  (0,1,0,3),
		  // 4
		  (x,x,0,4)
  ),
  4 -> Set(
		  // 0
		  (x,x,x,0),
		  (1,x,x,0),
		  // 2
		  (0,x,x,2),
		  // 3
		  (0,1,0,3),
		  // 4
		  (x,x,0,4),
		  (1,1,0,4),
		  // 5
		  (1,0,0,5)
  ),
  5 -> Set(
		  // 0
		  (x,x,x,0),
		  // 4
		  (x,x,0,4),
		  // 5
		  (1,0,0,5)
  ),
  6 -> Set(
		  // 0
		  (1,x,x,0),
		  (x,x,1,0),
		  (x,x,x,0),
		  // 1
		  (0,0,1,1),
		  // 2
		  (0,x,x,2),
		  // 4
		  (x,x,0,4),
		  // 5
		  (1,0,0,5),
		  // 6
		  (1,0,1,6)
  )
  
  )

  def apply(t1:WeightedTransducer,t2:WeightedTransducer,t3:WeightedTransducer) : WeightedTransducer = 
    new WeightedTransducer{
    
    var delta : Map[Int,Map[(Event,Event),(Int,Int)]] = Map()
    var accept : Set[Int] = Set()
    
	var state_map : Map[(Int,Int,Int),Int] = Map((0,0,0) -> 0)
	
	var state_count = 1
	def add_state(s:(Int,Int,Int)) : Int = add_state(s._1,s._2,s._3)
	def add_state(q1:Int,q2:Int,q3:Int) : Int = {
      if(state_map.contains((q1,q2,q3))) state_map((q1,q2,q3))
      else{
        val r = state_count
        state_count+=1
        state_map += ((q1,q2,q3) -> r)
        r
      }
    }
	
    // The combined states of the three machines
    val queue = new scala.collection.mutable.Queue[(Int,Int,Int)]()
	var seen : Set[(Int,Int,Int)] = Set((0,0,0))
    
	queue.enqueue((0,0,0))
	while(!queue.isEmpty){	  
	  val (q1,q2,q3) = queue.dequeue
	  val this_state = add_state(q1,q2,q3)
	  
	  if(t1.accept.contains(q1) && t2.accept.contains(q2) && t3.accept.contains(q3)) 
	    accept += this_state
	    
	  var small_delta = Map[(Event,Event),(Int,Int)]()  
	    
	  //println(t1.delta.getOrElse(q1,Map()))
	  //println(t3.delta.getOrElse(q3,Map()))
	  
	  for(e1 <- t1.delta.getOrElse(q1,Map()); e3 <- t3.delta.getOrElse(q3,Map())){
	    
	    val G = t2.delta.getOrElse(q2,Map()).filter{
	      case ((i,o),_) => i._1==e1._1._2._1 && o._1 == e3._1._1._1 }
	    
	    //println("G: "+G)
	    
	    for(e2 <- G){
	      val next_state = (e1._2._2, e2._2._2, e3._2._2)
	      if(!seen.contains(next_state)){
	        seen += next_state
	        queue.enqueue(next_state)
	      }
	      val weight = e1._2._1 + e2._2._1 + e3._2._1
	      small_delta += ((e1._1._1,e3._1._2) -> (weight,add_state(next_state)))
	    }
	  }
	  //println("sd: "+small_delta)
	  delta += (this_state -> small_delta)
	}
    
  }
  
  
}

object ShortestPath{
  
  var DEBUG = false
  
  // shortest distance to accepting state
  def apply(wt:WeightedTransducer) : Int = {
    
    var reach = wt.accept
    
    val states = wt.delta.map{case (s,sd) => sd.map(_._2._2).toSet+s}.flatten.toList
    
    // weights [state -> weight]
    var d : Map[Int,Int] = states.map(s => (s -> Int.MaxValue)).toMap
    d += (wt.init -> 0)
    
    // edits used in getting to the weight for this state
    var eds : Map[Int,Set[(Event,Event)]] = Map()
    eds += (wt.init -> Set())
    
    var previous : Map[Int,Set[(Int,Int)]] = states.map(s => (s -> Set((-1,0)))).toMap
    
    var queue = new scala.collection.mutable.PriorityQueue[Int]()(new Ordering[Int]{
      def compare(x:Int,y:Int) = d(y) - d(x)
    })
    queue.enqueue(wt.init)
    var seen : Set[Int] = Set(wt.init)
    
    
    while(!queue.isEmpty){
      
      val state = queue.dequeue
      
      for((a,(w,e)) <- wt.delta(state)){
        val next = d(state) + (if(eds.getOrElse(state,Set()) contains a) w else 10*w)
        if(next < d(e)){
          d += (e -> next)
          eds += (e -> (eds.getOrElse(state,Set())+a))
          var set = previous.getOrElse(e,Set())
          if(set equals Set((-1,0))) set = Set()
          set += ((state,next-d(state)))
          previous += (e -> set)
          if(!seen.contains(e)){
            seen+=e
            queue.enqueue(e)           
          }
        }
      }
    }
    
    if(false){
	    println("ddddddddddddddddddddddddddddd")
	    //d foreach println
	    //previous foreach println
	    var last = 28
	    while(last != -1){
	      var lasts = previous(last).toList
	      if(lasts.size!=1) println(lasts)
	      val n = lasts.last
	      println(n)
	      last = n._1
	    }
	    println("ddddddddddddddddddddddddddddd")
	    System.exit(0)
    }
   
    var min = Int.MaxValue
    reach.foreach{s => if(d(s)<min) min=d(s)}
    
    min
  }

    
  case class Store(weight:Int,spath:List[Int],epath:List[(Event,Event,Int)],eds:Set[(Event,Event)]){
    /*
    override def equals(x:Any) : Boolean = {
      if(x.isInstanceOf[Store]){
        val y = x.asInstanceOf[Store]
        (eds equals y.eds) && (spath.last equals y.spath.last)
      }
      false
    }
    override def hashCode : Int = ((spath.last,eds)).hashCode
    * */
  }  
  
  
def apply(minmin:Int,startmin:Int, wt : WeightedTransducer,stop:Int) : (Int,Set[List[(Event,Event,Int)]],Int) = {    
    
	 var min = startmin
	 val reach = wt.accept
    
    //println("min at " +min_at+", min is "+min)
    
    // now we do a search of the machine collecting paths of length min
    
    var paths : Set[List[(Event,Event,Int)]] = Set()
    var building = Set(Store(0,List(0),List[(Event,Event,Int)](),Set[(Event,Event)]()))
    
    var max_removes = 0
    
	    while(!building.isEmpty){
	      //println(building)
	      building = building.map{case Store(t,spath,epath,eds) =>
	        //println("From "+(t,spath,epath))
	        val last = spath.last
	        val ts = wt.delta(last).filter{case (_,(_,n)) => !(spath contains n)}.toSet
	        val next  = ts.map{case ((i,o),(w,n)) =>
	          //println(a+" -> "+n+" costing "+w)
	          val nw = t+(if(eds contains (i,o)) w else 10*w)
	          val np = Store(nw,spath:::List(n),epath:::List((i,o,w)),eds+((i,o)))
	          //println(np)
	          np
	        }
	        
	        // If there is a 0-weight transition take only that one
	        val zeros = next.filter(p => p.epath.last._3==0)
	        
	        if(!zeros.isEmpty) zeros
	        else next
	      }.flatten
	      //println(building)
	      building.filter(p => (reach contains p.spath.last)).foreach{p =>
	         if(p.weight < min && p.weight >= minmin){
	          //throw new RuntimeException("Min calculation was wrong i.e.\n "+p))
	           //println("Reseting min")
	           paths=Set()
	           min=p.weight
	        }
	      }
	      paths ++= building.filter(p => p.weight == min && (reach contains p.spath.last)).map(_.epath)
	      val bbefore = building.size
	      building = building.filter(p => p.weight <= min && !(reach contains p.spath.last))
	      max_removes += (bbefore - building.size)
	      //println("B "+building.size)
	      if(paths.size>stop) return (-1,Set(),max_removes)
	    }
    
    //println("paths :"+paths)
    (min,paths,max_removes)
  }

  val list_int_path = new Ordering[(Int,List[Int])]{
    def compare(l1:(Int,List[Int]),l2:(Int,List[Int])) = {
      
      l2._2.length - l1._2.length
    }
  }
}

object AlternatingMethod{
  def apply(wt:WeightedTransducer,local_max:Int) : Set[List[(Event,Event,Int)]] = {
    
    /*
     * Idea is to repeatedly find the longest 0-weighted path then use local_max
     * edits to reach another state with a 0-weighted outgoing transition and repeat
     */
    
    var s = wt.init
    while(true){
      println(s)
      val t = wt.delta(s).filter{case (_,(w,_)) => w==0}
      if(t.size==0){
        System.exit(0)
      }
      s = t.head._2._2
    }
    
    
    
    Set()
    
  }
}

object SuggestedPaths{
  def apply(wt:WeightedTransducer,n:Int,domin:Boolean,min_estimate:Int) : (Set[List[(Event,Event,Int)]],Int) = {
    
	  
	  var set : Set[List[(Event,Event,Int)]] = Set()
	  
	  var actual_min = -1;
	  
	  var min = 0
	  var max = min_estimate
	  var fails = 1
	  while(set.size<n){
	    //println("using "+min+","+max)
		  val (w,paths,max_rem) = ShortestPath(min,max,wt,(n-set.size))		  
		  
		  if(actual_min == -1) actual_min = w
		  if(w==0) return (paths,actual_min)
		  
		  //paths foreach println
		  //System.exit(0)
		  
		  if(paths.isEmpty){
		    if(max_rem==0) return (set,actual_min)
		    if(max>2*min) return (set,actual_min)
		    fails+=1
		    max+=(2*fails)
		    //println("Failed, increase max to "+max+" max rem was "+max_rem)
		  }
		  else{
		    
			  //println("Produced "+paths.size+" edits of weight "+w)
			  if(domin) return (paths,actual_min)
			/*  
			  var min_edit_size = Int.MaxValue
		      val edits = paths map { path => 
		        var m : Map[(Event,Event),Int] = Map()
		        path.filter(_._3!=0).foreach{ case a =>
		          val b = (a._1,a._2)
		          m += (b -> (m.getOrElse(b,0)+1))
		        }
		        val edit = m.toSet
		        if(edit.size<min_edit_size) min_edit_size = edit.size
		        (edit,path)
		      }		  
			  val min_edits = edits.filter{_._1.size==min_edit_size}
			  
			  println("Added "+min_edits.size+" edits with min "+min_edit_size)
			  set ++= min_edits.map(_._2)
			  */
			  set ++= paths
			  min = w+1
			  max= List(min+1,max).max
			  
			  //min_edits foreach { case (a,b) => println(a)}
			  //System.exit(0)
		  }
	  }
      (set,actual_min)
  }
}

object Path{
  
  def apply(wt:WeightedTransducer,path:List[Int]) : List[Set[(Event,Event,Int)]] = {

    //println(path)
    
    (0 until path.length-1).toList.map{case i => 
      
      val prev = path(i)
      val next = path(i+1)
      
      wt.delta(prev).filter{case (_,(_,s2)) => s2==next}.map{case ((i,o),(w,s2)) => (i,o,w)}.toSet
      
      //val (in,out) = t._1
      //val w = t._2._1      
      //println(prev+" -"+(in,out,w)+"-> "+next)      
      //(in,out,w)
    }

    
  }
  
}

object MakeInfo{
  
  def apply(wt:WeightedTransducer,path:List[Int]) : Info = {
		  //println("make "+path)
    
    (0 until path.length-1).toList.map{case i => 
      
      val prev = path(i)
      val next = path(i+1)
      
      val t = wt.delta(prev).filter{case (_,(_,s2)) => s2==next}.head
      
      val (in,out) = t._1
      val w = t._2._1
      
      (i,in,out,w)
      
    }.filter{case (i,in,out,w) => in!=out}
     .map{case (i,in,out,w) => 
     
       (in._1,out._1) match {
         case (-1,x) => (i,JUMP_WITH(x))
         case (x,-1) => (i,STAY)
         case (x,y)  => (i,REPLACE(x,y))
       }
     
     }

    
  }
  
}

