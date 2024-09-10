case class Rational(up:Int,down:Int){
  def numer:Int=up
  def denom:Int=down
  def sub(r:Rational):Rational={
    new Rational(numer*r.denom-denom*r.numer,denom*r.denom)
  }
  def neg:Rational= new Rational(-up,-down)
}

class Account(var money:Double=0.0){
  def Deposit(x:Double)={
    money=money+x
  }
  def Withdraw(x:Double)={
    money=money-x
  }
  def Transfer(x:Double,acc:Account)={
    acc.money=acc.money+x 
    money=money-x
  }
  def balance()={
    println(money)
  }
   def applyInterest(): Unit = {
    if (money > 0) {
      money += money * 0.05
    } else {
      money =money + money * 0.10 
    }
  }
}



object Practical10 {
 def main(args: Array[String]): Unit = {
   
   def Accwithneg(x:List[Account]):List[Account]={
  x.filter(acc=>acc.money<0)
  }
  def sumOfAll(accounts: List[Account]): Double = {
    accounts.map(_.money).sum
  }
  def countLetterOccurrences(x:List[String]):Int={
    x.map(word=>word.length).reduce((a,b)=>a+b)
  }
  
  println("Question 1 & 2")
  val num1= Rational(3,4)
  println(num1)
  //negation
  val num2=num1.neg
  println(num2)
  //substraction
  val num3=num1.sub(new Rational(5,8))
  println(num3)
  
  println("Question 3")
  val acc1=new Account
  //deposit
  acc1.Deposit(500.0)
  //withdraw
  acc1.Withdraw(200.0)
  val acc2=new Account
  //transfer
  acc1.Transfer(100,acc2)
  //check balance
  acc1.balance
  acc2.balance
    
  println("Question 4")
  val acc4=new Account()
  val acc5=new Account()
  val acc6=new Account()
  val acc7=new Account()
  
  acc4.Deposit(100.0)
  acc5.Deposit(250.0)
  acc6.Deposit(-40.0)
  acc7.Deposit(-200.0)
  val Bank:List[Account]= List(acc4,acc5,acc6,acc7)
  //getting neg acccss only
  val negBank=Accwithneg(Bank)
  negBank.foreach(x=>println(x.money))
  //getting sum
  var sum=sumOfAll(Bank)
  println(sum)
  //adding interest
  Bank.foreach(x=>x.applyInterest())
  //printing value after interest
  sum=sumOfAll(Bank)
  println(sum)
  
  println("Question 5")
  val fruits =List("apple","orange","grapes","kiwi","mango")
  val count=countLetterOccurrences(fruits)
  println(count)
}
}
