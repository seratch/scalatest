package org.scalatest.integ.ui

import javax.swing.JPanel
import java.util.Observer
import java.awt.GridBagLayout
import java.awt.Dimension
import javax.swing.ImageIcon
import javax.swing.JLabel
import java.awt.GridBagConstraints
import java.awt.Insets
import java.util.Observable
import org.scalatest.integ.TestModel
import org.scalatest.integ.TestStatus
import org.scalatest.integ.ScopeModel
import org.scalatest.integ.SuiteModel
import org.scalatest.integ.SuiteStatus
import org.scalatest.integ.RunModel
import org.scalatest.integ.RunStatus

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
      case resultController: ResultController => 
        changedModel match {
          case test: TestModel => 
            test.status match {
              case TestStatus.STARTED => 
                runCountLabel.setText(resultController.startedCount + "/" + resultController.totalCount)
              case TestStatus.SUCCEEDED =>
                succeededCountLabel.setText(resultController.succeedCount.toString)
              case TestStatus.FAILED =>
                failedCountLabel.setText(resultController.failureCount.toString)
              case TestStatus.IGNORED => 
                ignoredCountLabel.setText(resultController.ignoredCount.toString)
              case TestStatus.PENDING =>
                pendingCountLabel.setText(resultController.pendingCount.toString)
              case TestStatus.CANCELED => 
                canceledCountLabel.setText(resultController.canceledCount.toString)
            }
          case scope: ScopeModel =>
            // Do nothing for scope
          case suite: SuiteModel =>
            suite.status match {
              case SuiteStatus.STARTED => 
                suitesCountLabel.setText(resultController.suiteCount.toString)
              case SuiteStatus.ABORTED => 
                suitesAbortedCountLabel.setText(resultController.suiteAbortedCount.toString)
              case SuiteStatus.SUCCEED =>
                // Do nothing for suite succeed
              case SuiteStatus.FAILED =>
                // Do nothing for suite failed
            }
          case run: RunModel => 
            run.status match {
              case RunStatus.STARTED => 
                runCountLabel.setText(resultController.startedCount + "/" + resultController.totalCount)
                succeededCountLabel.setText(resultController.succeedCount.toString)
                failedCountLabel.setText(resultController.failureCount.toString)
                ignoredCountLabel.setText(resultController.ignoredCount.toString)
                pendingCountLabel.setText(resultController.pendingCount.toString)
                canceledCountLabel.setText(resultController.canceledCount.toString)
                suitesCountLabel.setText(resultController.suiteCount.toString)
                suitesAbortedCountLabel.setText(resultController.suiteAbortedCount.toString)
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