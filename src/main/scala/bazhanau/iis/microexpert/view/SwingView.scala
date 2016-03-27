package bazhanau.iis.microexpert.view

import javax.swing._

/**
  * Created by a.bazhanau on 27.03.16.
  */
object SwingView extends JFrame with App{
  val MAX_ANSW_SIZE: Int = 16
  val statusBar: JLabel = new JLabel("--")
  val totalRulesBar: JLabel = new JLabel("Всего правил: 0")
  val centralPanel: JPanel = new JPanel
  val northPanel: JPanel = new JPanel
  val select: JButton = new JButton("Выбрать")
  val selButtons: Array[JRadioButton] = new Array[JRadioButton](MAX_ANSW_SIZE)
  val selGroup: ButtonGroup = new ButtonGroup
  val fileChooser: JFileChooser = new JFileChooser
}
