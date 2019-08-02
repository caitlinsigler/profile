//Created by: Caitlin Sigler, 2019

//Curried and non-curried versions of same problem
//Each problem returns a list containing the even elements of ys
object currying { 
  def main(args: Array[String]) {
    //parameters for the following functions: list ys and function isEven
    def ys= List(12, 6, 23, 1, 8, 4)
    def isEven(x:Int): Boolean= {
      x%2 == 0
    }
  
  //non-curried version
  def isTrue(xs:List[Int], f:Int=>Boolean): List[Int]= {
    if (xs.isEmpty) Nil //base case, append Nil at end of list
    else if (f(xs.head)){
      xs.head :: isTrue(xs.tail, f) //if even, append head to list using cons, recursively parse through tail
    }
    else{
      isTrue(xs.tail, f) //skip head if odd, recursively parse through tail
    }
  }
  
  println(isTrue(ys, isEven))

  
 //curried version of above function
  def isCurried(f:Int=>Boolean): List[Int]=> List[Int]= {
    xs=> //transformer, compiler interprets as list from return value
      if (xs.isEmpty) Nil //append Nil at end of list
      else {
        def ret= isCurried(f) //define return function
        if (f(xs.head)){
          xs.head :: ret(xs.tail) //if even, append head to list using cons, recursively parse through tail
        }
        else{
          ret(xs.tail) //skip head if odd, recursively parse through tail
        }
      }
    }
    
  println(isCurried(isEven)(ys))
   
 
//anonymously curried version of above function
  def isCurriedAnon(f:Int=>Boolean): List[Int]=> List[Int]= {
    xs=> //transformer, compiler interprets as list from return value
      if (xs.isEmpty) Nil
      else {
        if (f(xs.head)){
          xs.head :: isCurriedAnon(f)(xs.tail) //if even, append head to list using cons, recursively parse through tail
        }
        else{
          isCurriedAnon(f)(xs.tail) //skip head if odd, recursively parse through tail
        }
      }
  }
  
  println(isCurriedAnon(isEven)(ys))
}
}
