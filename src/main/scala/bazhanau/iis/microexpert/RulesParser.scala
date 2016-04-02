package bazhanau.iis.microexpert

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by a.bazhanau on 27.03.16.
  */
object RulesParser extends RegexParsers {

  override def skipWhitespace: Boolean = true

  def valueRegex = """[а-яa-z0-9 ]*[а-яa-z0-9]""".r

  def number: Parser[Int] = """\d+""".r ^^ { _.toInt  }

  def attribute : Parser[Attribute] = valueRegex ^^ {Attribute.apply}

  def predicate : Parser[Predicate] = Predicate.Is.value ^^ {_ => Predicate.Is}

  def sentence : Parser[Statement] = attribute ~ predicate ~ valueRegex ^^ {
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
