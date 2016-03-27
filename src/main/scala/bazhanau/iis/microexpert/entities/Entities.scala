package bazhanau.iis.microexpert.entities

/**
  * Created by a.bazhanau on 27.03.16.
  */
case class Value (value: String)
case class Attribute (value: String)
case class Predicate(value : String)
case class Statement(attribute: Attribute, value: Value, predicate: Predicate = Predicate.Is)
case class Sentence(statements : Set[Statement])
case class Rule(number : Int, condition : Sentence, conclusion: Sentence)

object Predicate{
  val Is = Predicate("IS")
}
