package machine

object Minimise {

  
  def apply(state_machine:StateMachine) : StateMachine = {

   //println("mmmmmmmmmmmmmmmmmmmm")
   //state_machine.delta foreach println

   var states = state_machine.delta.map{case ((s1,e),s2) => Set(s1,s2)}.flatten.toSet + -1
   
   val n = states.size

   // table(i)(j)=true inticates that we can distinguish states i and j
   val table = Array.fill[Boolean](n,n)(false)

   // start off distinguishing between accepting and non-accepting

   for(q1 <- states; q2 <- states){
     val q1_a = state_machine.accept.contains(q1)
     val q2_a = state_machine.accept.contains(q2)
     // can distinguish if one is accepting and one is not
     val d = (q1_a || q2_a) && (!q1_a || !q2_a)
     table(q1+1).update(q2+1,d)
   }

   // distinguish two states if the states they reach via a symbol
   // are distinguished
   var updated = true
   while(updated){
     updated = false
     for(q1 <- states; q2 <- states; a <- state_machine.alphabet){

        val next_q1 = state_machine.delta.getOrElse((q1,a),-1)
        val next_q2 = state_machine.delta.getOrElse((q2,a),-1)

        // if the next states are distinguished and the current states
        // are not we can distinguish q1 and q2 (and we should update table)
        val d = table(next_q1+1)(next_q2+1) && !table(q1+1)(q2+1)

        if(d){
         table(q1+1).update(q2+1,true)
         updated = true
        }
     }
   }

   // the block [p] is the set of states we can't distinguish from p

   var blocks : Map[Int,Set[Int]] =
      Map(state_machine.init -> states.filter{q => !table(state_machine.init+1)(q+1) })
   blocks += ((-1 -> states.filter{q => !table(-1+1)(q+1) }))
   states --= blocks(state_machine.init)
   states --= blocks(-1)
   while(!states.isEmpty){
     val state = states.head
     val un_d = states.filter{q => !table(state+1)(q+1) }
     states --=un_d
     blocks += ((state -> un_d))
   }

   val un_block : Map[Int,Int] = (for((q,ps) <- blocks; p <- ps) yield (p -> q)).toMap

   var new_delta =  state_machine.delta.map{case ((q1,a),q2) => ((un_block(q1),a),un_block(q2)) }
   var new_accept = state_machine.accept.map(q => un_block(q))

   //new_delta foreach println
   
   //println(output.print(new DFA(new_delta,new_accept,Set(),"",state_machine.ordering),true))

   // Renumbering stage
   // renumber states in a canonical way by using state_machine.ordering

   val ordered_alpha = state_machine.alphabet.toList.sorted
   var re_numbering : Map[Int,Int] = Map(-1 -> -1)
   var next_state = 0
   def afs(state:Int){
     if(!re_numbering.contains(state)){
       //println((state -> next_state))
       re_numbering += ((state -> next_state))
       next_state += 1
       var nexts = ordered_alpha.map(a => new_delta.getOrElse((state,a),-1))
       nexts = nexts.filterNot(q => re_numbering.contains(q))
       nexts.map(q => afs(q))
     }
   }
   afs(0)
   //println(re_numbering)

   new_delta =  new_delta.map{case ((q1,a),q2) => ((re_numbering(q1),a),re_numbering(q2)) }
   new_accept = new_accept.map(q => re_numbering(q))

   new StateMachine{
     var accept : Set[Int] = new_accept
     var alphabet : Set[Int] = state_machine.alphabet
     delta = new_delta
   }

 }
  
}