

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class FromNumeral {
   
  def returnNumber (numeralString: String) = {
    constructNumber(numeralString.toList)
  }
    
  def constructNumber (numerals: List[Char]): Int = numerals match {
    case a :: Nil => convertNumeral(a)   
    case a :: b :: c if (convertNumeral(a) < convertNumeral(b)) => constructNumber(b :: c) - convertNumeral(a)
    case a :: b :: c => constructNumber(b :: c) + convertNumeral(a) 
    case _ => 0
  } 
    
  def convertNumeral (numeral: Char): Int = numeral match {
    case 'I' => 1
    case 'V' => 5
    case 'X' => 10
    case 'L' => 50
    case 'C' => 100
    case 'D' => 500
    case 'M' => 1000
  }
}


class NumeralTest extends FlatSpec with Matchers {

  "FromNumeral" should "pass back '1' for 'I'" in {
    
    val fromNumeral: FromNumeral = new FromNumeral
    
    fromNumeral.returnNumber("I") shouldBe 1   
  }

    it should "pass back '3' for 'III'" in {
    
    val fromNumeral: FromNumeral = new FromNumeral
    
    fromNumeral.returnNumber("III") shouldBe 3   
  }
    
    it should "pass back '9' for 'IX'" in {
    
    val fromNumeral: FromNumeral = new FromNumeral
    
    fromNumeral.returnNumber("IX") shouldBe 9   
  }    
    
    it should "pass back '1066' for 'MLXVI'" in {
    
    val fromNumeral: FromNumeral = new FromNumeral
    
    fromNumeral.returnNumber("MLXVI") shouldBe 1066   
  }    
    
    it should "pass back '1989' for 'MCMLXXXIX'" in {
    
    val fromNumeral: FromNumeral = new FromNumeral
    
    fromNumeral.returnNumber("MCMLXXXIX") shouldBe 1989   
  }            
      
    it should "pass back '578' for 'DLXXVIII'" in {
    
    val fromNumeral: FromNumeral = new FromNumeral
    
    fromNumeral.returnNumber("DLXXVIII") shouldBe 578   
  }        
}

