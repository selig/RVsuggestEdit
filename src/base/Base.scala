package base

import machine.StateMachine
import Base._

object Base {

  trait CodePoint{ def replace(from:String,to:String) : CodePoint}
  case class At(point:String) extends CodePoint{ 
    def replace(from:String,to:String) : CodePoint ={ At(point.replace(from,to)) }
    override def toString = point
  }
  case class Between(cp1:CodePoint, cp2:CodePoint) extends CodePoint{
    def replace(from:String,to:String) : CodePoint ={ Between(cp1.replace(from,to),cp2.replace(from,to))}
    override def toString = cp1+"-"+cp2
  }
  case object Beginning extends CodePoint{
    def replace(from:String,to:String) : CodePoint = Beginning
    override def toString = "B"
  }
  
  type MethodCall = Int
  type Event = (MethodCall,CodePoint)
  type Trace = List[Event]
  
  trait RECOVERY_EVENT{ def apply(m:Map[Int,Int]) : RECOVERY_EVENT}
  // the STAY recovery action relates to an incorrect symbol appearing in the trace
  case object STAY extends RECOVERY_EVENT
  		{ def apply(m:Map[Int,Int]) = this}
  // the JUMP_WITH recovery action relates to a symbol missing in the trace
  case class JUMP_WITH(symbol:Int) extends RECOVERY_EVENT
  		{ def apply(m:Map[Int,Int]) = JUMP_WITH(m(symbol))}
  // the REPLACE recovery action is a combination of the above two
  case class REPLACE(old_one:Int,new_one:Int) extends RECOVERY_EVENT 
  		{ def apply(m:Map[Int,Int]) = REPLACE(m(old_one),m(new_one))}
  
  case class OR(events:Set[RECOVERY_EVENT])  extends RECOVERY_EVENT
  		{ def apply(m:Map[Int,Int]) = OR(events.map(_(m)))}
  
  case object SAME extends RECOVERY_EVENT
  		{ def apply(m:Map[Int,Int]) = this}
  
  type Info = List[(Int,RECOVERY_EVENT)]  
  
  implicit def lift_info(info:Info) = new {
    def apply(m:Map[Int,Int]) = info.map{case (i,re) => (i,re(m))}
  } 
 
  def meta(a:Int) = Map(0 -> "a", 1 -> "b", 2 -> "c")(a)
  
}
