package bazhanau.iis.microexpert.parsers

import bazhanau.iis.microexpert.entities._

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by a.bazhanau on 27.03.16.
  */
object RulesParser extends RegexParsers {
  def number: Parser[Int] = """\d+""" ^^ { _.toInt  }

  def value : Parser[Value] = """\w""" ^^ {Value.apply}

  def attribute : Parser[Attribute] = """\w""" ^^ {Attribute.apply}

  def predicate : Parser[Predicate] = "IS" ^^ {_ => Predicate.Is}

  def sentence : Parser[Sentence] = attribute ~ predicate ~ value ^^ {
    case attribute ~ predicate ~ value => Sentence(attribute, value, predicate)
  }

  def statement : Parser[Statement] = sentence ~ rep("AND" ~> sentence) ^^ {
    case first ~ others => Statement((first :: others).toSet)
  }
  
  def rule : Parser[Rule] = number ~ "IF" ~ statement ~ "THEN" ~ statement <~ elem('.') ^^ {
    case num ~ _ ~ conditions ~ _ ~ conclusions => Rule(num, conditions, conclusions)
  }

}
