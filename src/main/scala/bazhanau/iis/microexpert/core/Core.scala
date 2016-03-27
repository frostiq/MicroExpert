package bazhanau.iis.microexpert.core

import bazhanau.iis.microexpert.entities.{Value, Attribute, Sentence, Rule}

/**
  * Created by a.bazhanau on 27.03.16.
  */
class Core(rules : Set[Rule]) {
  def getAnswer(target: Attribute) : Sentence = {
    Sentence(Attribute(""), Value(""))
  }
}
