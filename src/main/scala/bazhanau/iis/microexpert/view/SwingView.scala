package bazhanau.iis.microexpert.view

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{GridLayout, BorderLayout}
import javax.swing._

import bazhanau.iis.microexpert.core.Core
import bazhanau.iis.microexpert.entities.TypeDef.{TargetsStack, Context}
import bazhanau.iis.microexpert.entities._
import bazhanau.iis.microexpert.parsers.RulesParser

/**
  * Created by a.bazhanau on 27.03.16.
  */
object SwingView extends JFrame with App {
  val MAX_ANSWERS_SIZE: Int = 16

  val totalRulesLabel: JLabel = new JLabel("Всего правил: 0")
  val attributeLabel: JLabel = new JLabel()

  val selectOptionButton: JButton = new JButton("Выбрать вариант")
  val optionButtons: Seq[JRadioButton] = Seq.fill(MAX_ANSWERS_SIZE)(new JRadioButton())
  val optionGroup: ButtonGroup = new ButtonGroup
  val startButton: JButton = new JButton("Начать консультацию")
  val chooseFileButton: JButton = new JButton("Выбрать файл")

  val fileChooser: JFileChooser = new JFileChooser

  val targetStackPanel: JPanel = new JPanel()
  val contextPanel: JPanel = new JPanel()
  val optionsPanel: JPanel = new JPanel()
  val controlPanel: JPanel = new JPanel()
  val statusPanel: JPanel = new JPanel()

  val targetListModel: DefaultListModel[String] = new DefaultListModel[String]()
  val contextListModel: DefaultListModel[String] = new DefaultListModel[String]()


  var core: Option[Core] = None
  var result: Option[ConsultationResult] = None
  var path: String = "rules.txt"

  linkComponents()
  attachListeners()
  finishInit()

  def linkComponents(): Unit = {
    setLayout(new BorderLayout())
    add(targetStackPanel, BorderLayout.WEST)
    add(contextPanel, BorderLayout.CENTER)
    add(optionsPanel, BorderLayout.EAST)
    add(controlPanel, BorderLayout.NORTH)
    add(statusPanel, BorderLayout.SOUTH)

    controlPanel.setLayout(new BoxLayout(controlPanel, BoxLayout.LINE_AXIS))
    contextPanel.add(chooseFileButton)
    controlPanel.add(startButton)

    optionsPanel.setLayout(new GridLayout(MAX_ANSWERS_SIZE + 1, 1))
    optionsPanel.add(attributeLabel)
    for (button <- optionButtons) {
      optionGroup.add(button)
      optionsPanel.add(button)
    }
    optionsPanel.add(selectOptionButton)

    cleanOptions()


    statusPanel.add(totalRulesLabel)

    targetStackPanel.add(new JList(targetListModel))
    contextPanel.add(new JList(contextListModel))
  }

  def attachListeners(): Unit = {

    startButton.addActionListener (() => {
      val rules = parseRules(path)
      totalRulesLabel.setText(s"Всего правил: ${rules.size}")
      core = Some(new Core(rules))
      updateUI(None)
    })

    selectOptionButton.addActionListener (() => {
      for (button: JRadioButton <- optionButtons.find(_.isSelected); c: Core <- core) {
        result = result match {
          case None => Some(c.consult(Attribute(button.getText)))
          case Some(q: Question) => Some(c.consult(q, Value(button.getText)))
          case _ => result
        }
        updateUI(result)
      }
    })
  }

  def finishInit(): Unit = {
    setTitle("Microexpert")
    setSize(400, 400)
    setLocation(200, 200)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setVisible(true)
  }

  def parseRules(path: String) = {
    val content = scala.io.Source.fromFile(path).mkString
    val parseResult = RulesParser(content).getOrElse(List()).toSet
    totalRulesLabel.setText(s"Всего правил: ${parseResult.size}")
    parseResult
  }


  def updateUI(result: Option[ConsultationResult]) =
  for(c: Core <- core)
  result match {
    case None =>
      fillOptionsGroup(core.get.getTargets.map(_.value), "Выберите цель:")
      targetListModel.clear()
      contextListModel.clear()
    case Some(q : Question) =>
      val attr = q.currentTarget.value
      val caption = s"""Укажите значение атрибута "$attr":"""
      fillOptionsGroup(c.getOptions(q).map(_.value), caption)
      fillTargetStack(q.targets)
      fillContext(q.context)
    case Some(Answer(st)) =>
      cleanOptions()
      JOptionPane.showMessageDialog(this, st.toString, "Ответ", JOptionPane.INFORMATION_MESSAGE)
    case Some(NoAnswer()) =>
      cleanOptions()
      JOptionPane.showMessageDialog(this, "Ответ не может быть получен", "Ответа нет", JOptionPane.WARNING_MESSAGE)
  }

  def fillOptionsGroup(options: Set[String], caption: String): Unit = {
    cleanOptions()
    options.zip(optionButtons).foreach(x => {
      val (str, button) = x
      button.setText(str)
      button.setVisible(true)
    })
    selectOptionButton.setVisible(true)
    attributeLabel.setText(caption)
  }

  def cleanOptions(): Unit = {
    for (button <- optionButtons) button.setVisible(false)
    selectOptionButton.setVisible(false)
    attributeLabel.setText("")
  }

  def fillTargetStack(targets: TargetsStack) : Unit = {
    targetListModel.clear()
    for(target <- targets){
      targetListModel.addElement(target.toString)
    }
  }

  def fillContext(context: Context) : Unit = {
    contextListModel.clear()
    for(entry <- context){
      contextListModel.addElement(entry.toString())
    }
  }

  implicit class ActionListenerProxy(f: () => Unit) extends ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = f()
  }
}
