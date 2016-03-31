package bazhanau.iis.microexpert.parsers

import bazhanau.iis.microexpert.entities._

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by a.bazhanau on 27.03.16.
  */
object RulesParser extends RegexParsers {

  override def skipWhitespace: Boolean = true

  def wordRegex = """[а-яa-z0-9 ]*[а-яa-z0-9]""".r

  def number: Parser[Int] = """\d+""".r ^^ { _.toInt  }

  def value : Parser[Value] = wordRegex ^^ {Value.apply}

  def attribute : Parser[Attribute] = wordRegex ^^ {Attribute.apply}

  def predicate : Parser[Predicate] = Predicate.Is.value ^^ {_ => Predicate.Is}

  def sentence : Parser[Statement] = attribute ~ predicate ~ value ^^ {
    case attribute ~ predicate ~ value => Statement(attribute, value, predicate)
  }

  def statement : Parser[Sentence] = sentence ~ rep("AND" ~> sentence) ^^ {
    case first ~ others => Sentence((first :: others).toSet)
  }
  
  def rule : Parser[Rule] = number ~ "IF" ~ statement ~ "THEN" ~ statement <~ elem('.') ^^ {
    case num ~ _ ~ conditions ~ _ ~ conclusions => Rule(num, conditions, conclusions)
  }

  def apply(input : String): ParseResult[List[Rule]] = parseAll(rule.+, input)
}
