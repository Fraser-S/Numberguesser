/**
  * Created by Administrator on 13/06/2017.
  */

class numberGuess(lowerLimit: Int, upperLimit: Int) {

  val lower: Boolean = false
  val higher: Boolean = true

  def higherOrLower(userNumber: Int) : Int ={
    def iter(smallest: Int, highest: Int, noOfGuesses: Int, userNumber: Int): Int = {
      val guess = highest - ((highest - smallest) / 2)
      val input = readLine("Is this your number " + guess + ": ")

      input match {
        case "correct" => noOfGuesses
        case "higher" => iter(guess, highest, noOfGuesses + 1, userNumber)
        case "lower" => iter(smallest, guess, noOfGuesses + 1, userNumber)
        case a if userNumber == guess => noOfGuesses
        case _ => println("\nNot a recognised command!!"); iter(smallest, highest, noOfGuesses, userNumber)
      }
    }
    println("Use higher or lower or correct")
    iter(lowerLimit, upperLimit, 1, userNumber)
  }

  def guessHigher(lastNumber: Int, userInput: String): Int ={
    userInput match{
      case "better" => lastNumber+1
      case "worse" => lastNumber+5
    }
  }

  def guessLower(lastNumber: Int, userInput: String): Int ={
    userInput match{
      case "better" => lastNumber-1
      case "worse" => lastNumber-5
    }
  }

def guesses(userNumber: Int) : Int = {
  def iter(lastNumber: Int, lastOutcome: String, improvement: Boolean, direction: Boolean, noOfGuesses: Int, userNumber: Int):Int = {
    var guess: Int = 0
    var rnd = new scala.util.Random()
    noOfGuesses match{
      case 1 => guess = rnd.nextInt(upperLimit)
      case 2 => guess = rnd.nextInt(upperLimit)
      case a if improvement && direction == higher => guess = guessHigher(lastNumber, lastOutcome)
      case a if improvement && direction == lower => guess = guessLower(lastNumber, lastOutcome)
      case a if !improvement && direction == higher => guess = guessLower(lastNumber, lastOutcome)
      case a if !improvement && direction == lower => guess = guessHigher(lastNumber, lastOutcome)
      case _ => guess = rnd.nextInt(upperLimit)
    }

    var retry = false
    do {
      val input = readLine("Is this your number " + guess + ": ")

      input match {
        case a if input == "boiling" && (lastOutcome == "boiling" || lastOutcome == "warmer" || lastOutcome == "warm" || lastOutcome == "cold" || lastOutcome == "colder" || lastOutcome == "freezing") =>
        case a if input == "warmer" && ( lastOutcome == "warmer" || lastOutcome == "warm" || lastOutcome == "cold" || lastOutcome == "colder" || lastOutcome == "freezing") =>
        case a if input == "warm" && (lastOutcome == "warm" || lastOutcome == "cold" || lastOutcome == "colder" || lastOutcome == "freezing") =>
        case a if input == "cold" && (lastOutcome == "colder" || lastOutcome == "freezing") =>
        case a if input == "lastOutcome" && (lastOutcome == "freezing") =>
        case a if input == "better" && lastOutcome == "better" => iter(guess, "better", true, direction, noOfGuesses + 1, userNumber)
        case a if input == "better" && lastOutcome == "worse" => iter(lastNumber, "worse", false, !direction, noOfGuesses + 1, userNumber)
        case a if input == "worse" && lastOutcome == "better" => iter(guess, "better", true, direction, noOfGuesses + 1, userNumber)
        case a if input == "worse" && lastOutcome == "worse" => iter(lastNumber, "worse", false, !direction, noOfGuesses + 1, userNumber)
        case a if input == "better" && lastOutcome == "" => iter(guess, "better", true, direction, noOfGuesses + 1, userNumber)
        case a if input == "worse" && lastOutcome == "" => iter(guess, "better", true, direction, noOfGuesses + 1, userNumber)
        case "correct" => noOfGuesses
        case a if guess == userNumber => noOfGuesses
        case _ => retry = true; println("\nNot a recognised command!!")
      }
    }while(retry == true)
    noOfGuesses
  }
  println("Use better or worse or correct")
  iter(0, "", false, higher, 1, userNumber)
}


  def Num(): Int = {
    val input = readLine("Enter a number")
    var number :Int = input.toInt
    input match{
      case a if number <= upperLimit && number >= lowerLimit => //do nothing
      case _ => println("not a valid number"); number = Num()
    }
    number
  }
}

object Main {
  def main(args: Array[String]): Unit ={
    val guessNumber = new numberGuess(0,100)
    val number = guessNumber.Num()
    //println("I took "+guessNumber.higherOrLower(number)+" guesses")
    println("I took "+guessNumber.guesses(number)+" guesses")
  }

}
