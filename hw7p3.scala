
//curried and non-curried version of same problem
object currying { 
  def main(args: Array[String]) {
  def ys= List(12, 6, 23, 1, 8, 4)
  def g(x:Int): Boolean= {
    x%2 == 0
  }
  
  //returns a list containing the elements of xs for which f(x) is true 
  def isTrue(xs:List[Int], f:Int=>Boolean): List[Int]= {
    if (xs.isEmpty) Nil
    else if (f(xs.head)){
      xs.head :: isTrue(xs.tail, f)
    }
    else{
      isTrue(xs.tail, f)
    }
  }
  println(isTrue(ys, g))

  
 //curried version of above function
  def isCurried(f:Int=>Boolean): List[Int]=> List[Int]= {
    xs=>
      if (xs.isEmpty) Nil
      else {
        def ret= isCurried(f)
        if (f(xs.head)){
          xs.head :: ret(xs.tail)
        }
        else{
          ret(xs.tail)
        }
      }
    }
    println(isCurried(g)(ys))
   
 
//anonymously curried version of above function
  def isCurriedAnon(f:Int=>Boolean): List[Int]=> List[Int]= {
    xs=>
      if (xs.isEmpty) Nil
      else {
        if (f(xs.head)){
          xs.head :: isCurriedAnon(f)(xs.tail)
        }
        else{
          isCurriedAnon(f)(xs.tail)
        }
      }
  }
  println(isCurriedAnon(g)(ys))
}
}