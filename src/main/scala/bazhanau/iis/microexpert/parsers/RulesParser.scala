package bazhanau.iis.microexpert.parsers

import bazhanau.iis.microexpert.entities._

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by a.bazhanau on 27.03.16.
  */
class RulesParser extends RegexParsers {
  def number: Parser[Int] = """\d+""".r ^^ { _.toInt  }

  def value : Parser[Value] = """\w+""".r ^^ {Value.apply}

  def attribute : Parser[Attribute] = """\w+""".r ^^ {Attribute.apply}

  def predicate : Parser[Predicate] = "IS" ^^ {_ => Predicate.Is}

  def sentence : Parser[Statement] = attribute ~ predicate ~ value ^^ {
    case attribute ~ predicate ~ value => Statement(attribute, value, predicate)
  }

  def statement : Parser[Sentence] = sentence ~ rep("AND" ~> sentence) ^^ {
    case first ~ others => Sentence((first :: others).toSet)
  }
  
  def rule : Parser[Rule] = number ~ "IF" ~ statement ~ "THEN" ~ statement <~ elem('.') ^^ {
    case num ~ _ ~ conditions ~ _ ~ conclusions => Rule(num, conditions, conclusions)
  }

  def apply(input : String) = parseAll(rule+, input)
}
