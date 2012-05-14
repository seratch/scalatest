package org.scalatest.integ

import javax.swing.JPanel
import java.awt.GridLayout
import javax.swing.JLabel
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.Dimension
import java.awt.Insets
import java.util.Observer
import java.util.Observable
import javax.swing.ImageIcon
import java.awt.Color
import java.awt.Image
import java.awt.Graphics
import java.awt.BorderLayout
import javax.swing.JTree
import javax.swing.JScrollPane
import javax.swing.tree.TreeNode
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreeSelectionModel
import javax.swing.SwingUtilities
import javax.swing.tree.DefaultTreeCellRenderer

class ResultView extends JPanel with Observer {
  
  private val counterPanel = new CounterPanel()
  private val colorBar = new ColorBar()
  private val resultTree = new ResultTree()
  
  private val resultModel = new ResultModel()
  resultModel.addObserver(counterPanel)
  resultModel.addObserver(colorBar)
  resultModel.addObserver(resultTree)
  
  private val controlPanel = new JPanel() {
    setLayout(new BorderLayout())
    add(counterPanel, BorderLayout.CENTER)
    add(colorBar, BorderLayout.SOUTH)
  }
  
  setLayout(new BorderLayout())
  add(controlPanel, BorderLayout.NORTH)
  add(resultTree, BorderLayout.CENTER)
  
  def update(o: Observable, event: AnyRef) {
    event match {
      case e: Event => resultModel.update(e)
      case _ => // Ignore others
    }
  }
}

private object Icons {
  private val loader = getClass.getClassLoader
  
  val TESTS = new ImageIcon(loader.getResource("images/tests.gif"))
  val SUCCEEDED = new ImageIcon(loader.getResource("images/test_succeeded.gif"))
  val FAILED = new ImageIcon(loader.getResource("images/test_failed.gif"))
  val IGNORED = new ImageIcon(loader.getResource("images/test_ignored.gif"))
  val PENDING = new ImageIcon(loader.getResource("images/test_pending.gif"))
  val CANCELED = new ImageIcon(loader.getResource("images/test_canceled.gif"))
  val SUITE = new ImageIcon(loader.getResource("images/suite.gif"))
  val SUITE_ABORTED = new ImageIcon(loader.getResource("images/suite_aborted.gif"))
  val INFO = new ImageIcon(loader.getResource("images/info.gif"))
}

private class ResultModel extends Observable {
  
  @volatile var startedCount = 0
  @volatile var succeedCount = 0
  @volatile var failureCount = 0
  @volatile var ignoredCount = 0
  @volatile var pendingCount = 0
  @volatile var canceledCount = 0
  @volatile var totalCount = 0
  @volatile var suiteCount = 0
  @volatile var suiteAbortedCount = 0
  
  private var suiteMap: Map[String, SuiteModel] = null
  private var run: RunModel = null
  
  private def notifyChanges(changedData: AnyRef) {
    setChanged()
    notifyObservers(changedData)
  }
  
  def update(event: Event) {
    event match {
      case testStarting: TestStarting => 
        startedCount += 1
        val test = 
          TestModel(
            testStarting.suiteId, 
            testStarting.testName,
            testStarting.testText,
            testStarting.decodedTestName,
            None,
            None, 
            None, 
            None, 
            testStarting.location,
            testStarting.rerunner,
            testStarting.threadName,
            testStarting.timeStamp, 
            TestStatus.STARTED
          )
        suiteMap.get(testStarting.suiteId) match {
          case Some(suite) => 
            suite.addChild(test)
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestStarting, suiteId: " + testStarting.suiteId + ", test name: " + testStarting.testName)
        }
      case testSucceeded: TestSucceeded => 
        succeedCount += 1
        suiteMap.get(testSucceeded.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testSucceeded.testName, TestStatus.SUCCEEDED, testSucceeded.duration, testSucceeded.location, None, None, None)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestSucceeded, suiteId: " + testSucceeded.suiteId + ", test name: " + testSucceeded.testName)
        }
      case testFailed: TestFailed => 
        failureCount += 1
        suiteMap.get(testFailed.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testFailed.testName, TestStatus.FAILED, testFailed.duration, testFailed.location, testFailed.errorMessage, testFailed.errorDepth, testFailed.errorStackTraces)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestFailed, suiteId: " + testFailed.suiteId + ", test name: " + testFailed.testName)
        }
      case testIgnored: TestIgnored => 
        ignoredCount += 1
        val test = 
          TestModel(
            testIgnored.suiteId, 
            testIgnored.testName,
            testIgnored.testText,
            testIgnored.decodedTestName,
            None,
            None, 
            None, 
            None, 
            testIgnored.location,
            None,
            testIgnored.threadName,
            testIgnored.timeStamp, 
            TestStatus.IGNORED
          )
        suiteMap.get(testIgnored.suiteId) match {
          case Some(suite) => 
            suite.addChild(test)
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestIgnored, suiteId: " + testIgnored.suiteId + ", test name: " + testIgnored.testName)
        }
      case testPending: TestPending => 
        pendingCount += 1
        suiteMap.get(testPending.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testPending.testName, TestStatus.PENDING, testPending.duration, testPending.location, None, None, None)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestPending, suiteId: " + testPending.suiteId + ", test name: " + testPending.testName)
        }
      case testCanceled: TestCanceled => 
        canceledCount += 1
        suiteMap.get(testCanceled.suiteId) match {
          case Some(suite) => 
            val test = suite.updateTest(testCanceled.testName, TestStatus.CANCELED, testCanceled.duration, testCanceled.location, testCanceled.errorMessage, testCanceled.errorDepth, testCanceled.errorStackTraces)
            suite.closeScope()
            notifyChanges(test)
          case None => 
            // Should not happen
            throw new IllegalStateException("Unable to find suite model for TestCanceled, suiteId: " + testCanceled.suiteId + ", test name: " + testCanceled.testName)
        }
      case suiteStarting: SuiteStarting => 
        if (suiteStarting.suiteId != "org.scalatest.tools.DiscoverySuite") {
          suiteCount += 1
          val suite = SuiteModel(
                        suiteStarting.suiteName,
                        suiteStarting.suiteId,
                        suiteStarting.suiteClassName,
                        suiteStarting.decodedSuiteName,
                        suiteStarting.location,
                        suiteStarting.rerunner,
                        None,
                        None,
                        None, 
                        None, 
                        suiteStarting.threadName,
                        suiteStarting.timeStamp, 
                        SuiteStatus.STARTED
                      )
          run.addChild(suite)
          //suiteList += suite
          suiteMap += (suite.suiteId -> suite)
          //fTestRunSession.rootNode.addChild(suite)
          notifyChanges(suite)
        }
      case suiteCompleted: SuiteCompleted => 
        if (suiteCompleted.suiteId != "org.scalatest.tools.DiscoverySuite") {
          suiteMap.get(suiteCompleted.suiteId) match {
            case Some(suite) => 
              suite.duration = suiteCompleted.duration
              suite.location = suiteCompleted.location
              suite.status = 
                if (suite.suiteSucceeded)
                  SuiteStatus.SUCCEED
                else
                  SuiteStatus.FAILED
              notifyChanges(suite)
            case None => 
              // Should not happen
              throw new IllegalStateException("Unable to find suite model for SuiteCompleted, suiteId: " + suiteCompleted.suiteId)
          }
        }
      case suiteAborted: SuiteAborted => 
        if (suiteAborted.suiteId != "org.scalatest.tools.DiscoverySuite") {
          suiteAbortedCount += 1
          suiteMap.get(suiteAborted.suiteId) match {
            case Some(suite) => 
              suite.duration = suiteAborted.duration
              suite.location = suiteAborted.location
              suite.errorMessage = suiteAborted.errorMessage
              suite.errorDepth = suiteAborted.errorDepth
              suite.errorStackTrace = suiteAborted.errorStackTraces
              suite.status = SuiteStatus.ABORTED
              notifyChanges(suite)
            case None => 
              // Should not happend
              throw new IllegalStateException("Unable to find suite model for SuiteAborted, suiteId: " + suiteAborted.suiteId)
          }
        }
      case runStarting: RunStarting => 
        //enableToolbarControls(false)
        //suiteList.clear()
        startedCount = 0
        succeedCount = 0
        failureCount = 0
        ignoredCount = 0
        pendingCount = 0
        canceledCount = 0
        totalCount = runStarting.testCount
        suiteCount = 0
        suiteAbortedCount = 0
        suiteMap = Map.empty[String, SuiteModel]
        totalCount = runStarting.testCount
        run = 
          RunModel(
            runStarting.testCount, 
            None, 
            None,
            None,
            None, 
            None, 
            runStarting.threadName,
            runStarting.timeStamp, 
            RunStatus.STARTED
          )
        notifyChanges(run)
      case runCompleted: RunCompleted =>
        run.duration = runCompleted.duration
        run.summary = runCompleted.summary
        run.status = RunStatus.COMPLETED
        //nodeList = getFlattenNode(suiteList.toList)
        notifyChanges(run)
        //enableToolbarControls(true)
      case runStopped: RunStopped => 
        run.duration = runStopped.duration
        run.summary = runStopped.summary
        run.status = RunStatus.STOPPED
        //nodeList = getFlattenNode(suiteList.toList)
        notifyChanges(run)
        //enableToolbarControls(true)
      case runAborted: RunAborted => 
        if (run != null) {
          run.duration = runAborted.duration
          run.summary = runAborted.summary
          run.errorMessage = runAborted.errorMessage
          run.errorDepth = runAborted.errorDepth
          run.errorStackTrace = runAborted.errorStackTraces
          run.status = RunStatus.ABORTED
          notifyChanges(run)
        }
        //nodeList = getFlattenNode(suiteList.toList)
        //enableToolbarControls(true)
      case infoProvided: InfoProvided => 
        val info = 
          InfoModel(
            infoProvided.message,
            infoProvided.nameInfo,
            infoProvided.aboutAPendingTest,
            infoProvided.aboutACanceledTest,
            infoProvided.errorMessage, 
            infoProvided.errorDepth, 
            infoProvided.errorStackTraces, 
            infoProvided.location, 
            infoProvided.threadName,
            infoProvided.timeStamp
          )
        infoProvided.nameInfo match {
          case Some(nameInfo) => 
            suiteMap.get(nameInfo.suiteId) match {
              case Some(suite) => 
                suite.addChild(info)
                notifyChanges(info)
              case None => 
                // Should not happen
               throw new IllegalStateException("Unable to find suite model for InfoProvided, suiteId: " + nameInfo.suiteId)
            }
          case None => 
            run.addChild(info)
        }
      case markupProvided: MarkupProvided => 
        // Do nothing for MarkupProvided, markup info should be shown in HtmlReporter only.
      case scopeOpened: ScopeOpened => 
        suiteMap.get(scopeOpened.nameInfo.suiteId) match {
          case Some(suite) => 
            val scope = 
              ScopeModel(
                scopeOpened.message,
                scopeOpened.nameInfo,
                scopeOpened.location,
                scopeOpened.threadName,
                scopeOpened.timeStamp, 
                ScopeStatus.OPENED
              )
            suite.addChild(scope)
            notifyChanges(scope)
          case None => 
            // Should not happend
            throw new IllegalStateException("Unable to find suite model for ScopeOpened, suiteId: " + scopeOpened.nameInfo.suiteId)
        }
      case scopeClosed: ScopeClosed => 
        suiteMap.get(scopeClosed.nameInfo.suiteId) match {
          case Some(suite) => 
            suite.closeScope()
          case None => 
            throw new IllegalStateException("Unable to find suite model for ScopeClosed, suiteId: " + scopeClosed.nameInfo.suiteId)
        }
      case _ => // Ignore others
    }
  }
}

private class CounterPanel extends JPanel with Observer {
  
  setLayout(new GridBagLayout())
  
  private def createLabel(title: String, gridx: Int, gridy: Int, size: Dimension, icon: ImageIcon) = {
    val label = new JLabel(title)
    label.setPreferredSize(size)
    label.setIcon(icon)
    val constraints = new GridBagConstraints()
    constraints.insets = new Insets(2, 5, 2, 5)
    constraints.gridx = gridx
    constraints.gridy = gridy
    constraints.gridwidth = 1;
    constraints.gridheight = 1;
    constraints.anchor = GridBagConstraints.WEST;
    constraints.fill = GridBagConstraints.BOTH
    add(label, constraints)
    label
  }
    
  private val labelSize = new Dimension(120, 20)
  private val countSize = new Dimension(40, 20)
  
  private val runLabel = createLabel("Tests", 0, 0, labelSize, Icons.TESTS)
  private val runCountLabel = createLabel("0/0", 1, 0, countSize, null)
  private val succeededLabel = createLabel("Succeeded", 2, 0, labelSize, Icons.SUCCEEDED)
  private val succeededCountLabel = createLabel("0", 3, 0, countSize, null)
  private val failedLabel = createLabel("Failed", 4, 0, labelSize, Icons.FAILED)
  private val failedCountLabel = createLabel("0", 5, 0, countSize, null)
    
  private val ignoredLabel = createLabel("Ignored", 0, 1, labelSize, Icons.IGNORED)
  private val ignoredCountLabel = createLabel("0", 1, 1, countSize, null)
  private val pendingLabel = createLabel("Pending", 2, 1, labelSize, Icons.PENDING)
  private val pendingCountLabel = createLabel("0", 3, 1, countSize, null)
  private val canceledLabel = createLabel("Canceled", 4, 1, labelSize, Icons.CANCELED)
  private val canceledCountLabel = createLabel("0", 5, 1, countSize, null)
    
  private val suitesLabel = createLabel("Suites", 0, 2, labelSize, Icons.SUITE)
  private val suitesCountLabel = createLabel("0", 1, 2, countSize, null)
  private val suitesAbortedLabel = createLabel("Aborted", 2, 2, labelSize, Icons.SUITE_ABORTED)
  private val suitesAbortedCountLabel = createLabel("0", 3, 2, countSize, null)
  
  def update(o: Observable, changedModel: AnyRef) {
    o match {
      case resultModel: ResultModel => 
        changedModel match {
          case test: TestModel => 
            test.status match {
              case TestStatus.STARTED => 
                runCountLabel.setText(resultModel.startedCount + "/" + resultModel.totalCount)
              case TestStatus.SUCCEEDED =>
                succeededCountLabel.setText(resultModel.succeedCount.toString)
              case TestStatus.FAILED =>
                failedCountLabel.setText(resultModel.failureCount.toString)
              case TestStatus.IGNORED => 
                ignoredCountLabel.setText(resultModel.ignoredCount.toString)
              case TestStatus.PENDING =>
                pendingCountLabel.setText(resultModel.pendingCount.toString)
              case TestStatus.CANCELED => 
                canceledCountLabel.setText(resultModel.canceledCount.toString)
            }
          case scope: ScopeModel =>
            // Do nothing for scope
          case suite: SuiteModel =>
            suite.status match {
              case SuiteStatus.STARTED => 
                suitesCountLabel.setText(resultModel.suiteCount.toString)
              case SuiteStatus.ABORTED => 
                suitesAbortedCountLabel.setText(resultModel.suiteAbortedCount.toString)
              case SuiteStatus.SUCCEED =>
                // Do nothing for suite succeed
              case SuiteStatus.FAILED =>
                // Do nothing for suite failed
            }
          case run: RunModel => 
            run.status match {
              case RunStatus.STARTED => 
                runCountLabel.setText(resultModel.startedCount + "/" + resultModel.totalCount)
                succeededCountLabel.setText(resultModel.succeedCount.toString)
                failedCountLabel.setText(resultModel.failureCount.toString)
                ignoredCountLabel.setText(resultModel.ignoredCount.toString)
                pendingCountLabel.setText(resultModel.pendingCount.toString)
                canceledCountLabel.setText(resultModel.canceledCount.toString)
                suitesCountLabel.setText(resultModel.suiteCount.toString)
                suitesAbortedCountLabel.setText(resultModel.suiteAbortedCount.toString)
              case RunStatus.COMPLETED =>
                // Do nothing for completed
              case RunStatus.STOPPED =>
                // Do nothing for stopped
              case RunStatus.ABORTED => 
                // Do nothing for aborted
            }
          case _ =>
            // Ignore others
        }
      case _ => 
        // Do nothing if the observable is not ResultModel
    }
  }
}

private class ColorBar extends JPanel with Observer {

  private val HANDSOME_GREEN: Color = new Color(0x55, 0xEE, 0x66)
  private val DEEP_RED: Color = new Color(0xEE, 0x55, 0x66)
  private val SENSIBLE_GRAY: Color = new Color(0xEE, 0xEE, 0xEE)
  // These vars should be set only by the event handler thread, so no need for synchronization
  private var max: Int = 0
  private var value: Int = 0
  private var barColor: Color = HANDSOME_GREEN

  // A cache, for performance, so I can reuse the Image object if the dimensions are the same
  private var offscreenImage: Image = _

  setBackground(SENSIBLE_GRAY)

  def setGreen() {
    barColor = HANDSOME_GREEN
    repaint()
  }

  def setRed() {
    barColor = DEEP_RED
    repaint()
  }

  def setGray() {
    barColor = SENSIBLE_GRAY
    repaint()
  }
  
  def increaseValue() {
    setValue(value + 1)
  }

  def setValue(value: Int) {

    if (value < 0) 
      throw new IllegalArgumentException()

    this.value = value

    if (value > max)
      max = value

    repaint()
  }

  def setMax(max: Int) {

    if (max < 0)
      throw new IllegalArgumentException()

    this.max = max

    if (value > max)
      value = max

    repaint()
  }

  override def update(g: Graphics) {
    paint(g)
  }

  override def paint(g: Graphics) {

    val dim: Dimension = getSize()

    if (offscreenImage == null) {
      offscreenImage = createImage(dim.width, dim.height)
    }
    else {
      val offWidth: Int = offscreenImage.getWidth(null)
      val offHeight: Int = offscreenImage.getHeight(null)
      if (offWidth != dim.width || offHeight != dim.height)
        offscreenImage = createImage(dim.width, dim.height)
    }

    val og: Graphics = offscreenImage.getGraphics()

    og.setColor(SENSIBLE_GRAY)
    og.fillRect(0, 0, dim.width, dim.height)

    val localVal: Int = value
    val localMax: Int = max

    val extent: Int =
      if (localVal >= localMax) {
        dim.width + 1
      }
      else if (localVal != 0) {

        val floatExtent: Float = (dim.width.toFloat * localVal) / localMax
        floatExtent.toInt 
      }
      else 0

    if (max != 0) {
      og.setColor(barColor)
      og.fillRect(0, 0, extent, dim.height + 1)
    }
    g.drawImage(offscreenImage, 0, 0, this)
  }
  
  def update(o: Observable, changedModel: AnyRef) {
    o match {
      case resultModel: ResultModel => 
        changedModel match {
          case test: TestModel => 
            test.status match {
              case TestStatus.STARTED => 
                increaseValue()
              case TestStatus.SUCCEEDED =>
                // Do nothing for test succeeded.
              case TestStatus.FAILED =>
                setRed()
              case TestStatus.IGNORED => 
                // Do nothing for test ignored.
              case TestStatus.PENDING =>
                // Do nothing for test pending.
              case TestStatus.CANCELED => 
                // Do nothing for test canceled
            }
          case scope: ScopeModel =>
            // Do nothing for scope
          case suite: SuiteModel =>
            // Do nothing for suite
          case run: RunModel => 
            run.status match {
              case RunStatus.STARTED => 
                setValue(0)
                setMax(resultModel.totalCount)
                setGreen()
              case RunStatus.COMPLETED =>
                // Do nothing for completed
              case RunStatus.STOPPED =>
                setGray()
              case RunStatus.ABORTED => 
                setGray()
            }
          case _ =>
            // Ignore others
        }
      case _ => 
        // Do nothing if the observable is not ResultModel
    }
  }
}

private object SwingHelper {
  def invokeLater(f: => Unit) {
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        f
      }
    })
  }
} 

private class ResultTree extends JPanel with Observer {
  
  import SwingHelper._
  
  private val tree = new JTree()
  private var root: RunModel = null
  private var model: DefaultTreeModel = null
  
  setLayout(new GridLayout(1, 1))
  initTree()
  
  private def initTree() {
    tree.getSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    tree.setCellRenderer(new NodeRenderer())
    add(new JScrollPane(tree))
    resetRoot(null)
  }
  
  private class NodeRenderer extends DefaultTreeCellRenderer {
    override def getTreeCellRendererComponent(tree: JTree, value: AnyRef, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean) = {
      super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
      value match {
        case test: TestModel =>
          setText(test.testText)
          test.status match {
            case TestStatus.STARTED => 
              setIcon(Icons.TESTS)
            case TestStatus.SUCCEEDED =>
              setIcon(Icons.SUCCEEDED)
            case TestStatus.FAILED =>
              setIcon(Icons.FAILED)
            case TestStatus.IGNORED =>
              setIcon(Icons.IGNORED)
            case TestStatus.PENDING =>
              setIcon(Icons.PENDING)
            case TestStatus.CANCELED =>
              setIcon(Icons.CANCELED)
          }
        case scope: ScopeModel => 
          setText(scope.message)
          setIcon(Icons.SUITE)
        case suite: SuiteModel => 
          setText(suite.suiteName)
          suite.status match {
            case SuiteStatus.STARTED => 
              setIcon(Icons.SUITE)
            case SuiteStatus.SUCCEED => 
              setIcon(Icons.SUITE)
            case SuiteStatus.FAILED => 
              setIcon(Icons.SUITE_ABORTED)
            case SuiteStatus.ABORTED => 
              setIcon(Icons.SUITE_ABORTED)
          }
        case info: InfoModel => 
          setText(info.message)
          setIcon(Icons.INFO)
        case run: RunModel => 
          setText("Run")
          setIcon(Icons.TESTS)
        case str: String => 
          setText(str)
        case _ =>
      }
      this
    }
  }
  
  private def resetRoot(run: RunModel) {
    root = run
    if (root == null) 
      root = RunModel(0, None, None, None, None, None, null, 0, RunStatus.COMPLETED)
    model = new DefaultTreeModel(root)
    tree.setModel(model)
    invokeLater { model.nodeStructureChanged(root) }
  }
  
  def update(o: Observable, changedModel: AnyRef) {
    o match {
      case resultModel: ResultModel => 
        changedModel match {
          case test: TestModel => 
            test.status match {
              case TestStatus.STARTED => 
                invokeLater { model.nodeStructureChanged(test.parent) }
              case TestStatus.SUCCEEDED =>
                invokeLater { model.nodeChanged(test) }
              case TestStatus.FAILED =>
                invokeLater { model.nodeChanged(test) }
              case TestStatus.IGNORED => 
                invokeLater { model.nodeChanged(test) }
              case TestStatus.PENDING =>
                invokeLater { model.nodeChanged(test) }
              case TestStatus.CANCELED => 
                invokeLater { model.nodeChanged(test) }
            }
          case scope: ScopeModel => 
            scope.status match {
              case ScopeStatus.OPENED => 
                invokeLater { model.nodeStructureChanged(scope.parent) }
            }
          case suite: SuiteModel =>
            suite.status match {
              case SuiteStatus.STARTED => 
                invokeLater { model.nodeStructureChanged(suite.parent) }
              case SuiteStatus.ABORTED => 
                invokeLater { model.nodeChanged(suite) }
              case SuiteStatus.SUCCEED =>
                invokeLater { model.nodeChanged(suite) }
              case SuiteStatus.FAILED =>
                invokeLater { model.nodeChanged(suite) }
            }
          case run: RunModel => 
            run.status match {
              case RunStatus.STARTED => 
                resetRoot(run)
              case RunStatus.COMPLETED =>
                model.reload()
              case RunStatus.STOPPED =>
                model.reload()
              case RunStatus.ABORTED => 
                model.reload()
            }
          case info: InfoModel =>
            invokeLater { model.nodeStructureChanged(info.parent) }
          case _ =>
            // Ignore others
        }
      case _ => 
        // Do nothing if the observable is not ResultModel
    }
  }
}