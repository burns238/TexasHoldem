

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class Game {
  var rolls: Array[Int] = Array()

  def roll(pin: Int) = {

    if (pin > 10) {
      throw new Exception("Too many pins")
    }

    if (rolls.length > 20) {
      throw new Exception("Too many")
    }

    rolls = rolls :+ pin
  }

  def score: Int = {
    var rollscount:Int=0
    for(i <- 0 until rolls.length) 
    {
      rollscount = rollscount + rolls(i)
      
      if (i==2 && rolls(i-1) + rolls(i-2) == 10){
         rollscount = rollscount + rolls(i)
      }
    }
    rollscount
  }

}

class BowlingTest extends FlatSpec with Matchers {

  "Starting a game" should "return a score of 0" in {
    val game: Game = new Game

    game.score shouldBe 0
  }
  "First Roll" should "return a score of 6" in {
    val game: Game = new Game

    game.roll(6)

    game.score shouldBe 6
  }
  "Full game of gutter balls" should "return a score of 0" in {
    val game: Game = new Game

    for (i <- 1 to 20) game.roll(0)

    game.score shouldBe 0
  }
  "22nd roll" should "thrown an error" in {
    val game: Game = new Game
    val thrown = intercept[Exception] {
      for (i <- 1 to 22) game.roll(0)
    }

    thrown.getMessage shouldBe "Too many"
  }

  "11 pins" should "is not allowed" in {
    val game: Game = new Game

    val thrown = intercept[Exception] {
      game.roll(11)

    }

    thrown.getMessage shouldBe "Too many pins"

  }
  
  "spare" should "be calculated" in {
    val game: Game = new Game

    game.roll(9)
    game.roll(1)
    game.roll(2)
    
    game.score shouldBe 14

  }

}


//
//class GameTest extends FlatSpec with Matchers {
//  
//  
//  
//  
//  "score" should "initially be 0" in {
//    val game = new Game()
//    
//    game.score shouldBe 0
//  }
//  
//  "score" should "be correct after 2 rolls" in {
//    val game = new Game()
//    
//    game.roll(1)
//    game.roll(2)
//    
//    game.score shouldBe 3
//  }
//  
//  "score" should "be correct after 20 rolls" in {
//    val game = new Game()
//    
//    for(i <- 1 to 20) {
//      game.roll(1)
//    }
//    
//    game.score shouldBe 20
//  }
//  
//  "score" should "be correct after a gutter game" in {
//    val game = new Game()
//    
//    for(i <- 1 to 20) {
//      game.roll(0)
//    }
//    
//    game.score shouldBe 0
//  }
//  
//  "score" should "be correct after a spare" in {
//    val game = new Game()
//    
//    game.roll(5)
//    game.roll(5)
//    game.roll(5)
//    
//    game.score shouldBe 20
//  }
//}