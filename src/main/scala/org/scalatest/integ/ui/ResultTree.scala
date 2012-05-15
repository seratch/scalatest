package org.scalatest.integ.ui

import org.scalatest.integ.spi.TreeActionProvider
import javax.swing.JPanel
import java.util.Observer
import javax.swing.JTree
import org.scalatest.integ.RunModel
import javax.swing.tree.DefaultTreeModel
import java.awt.GridLayout
import javax.swing.tree.TreeSelectionModel
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt.event.MouseAdapter
import java.awt.event.KeyAdapter
import java.awt.event.MouseEvent
import java.awt.event.KeyEvent
import org.scalatest.integ.Node
import javax.swing.JScrollPane
import org.scalatest.integ.TestModel
import org.scalatest.integ.TestStatus
import org.scalatest.integ.ScopeModel
import org.scalatest.integ.SuiteModel
import org.scalatest.integ.SuiteStatus
import org.scalatest.integ.InfoModel
import org.scalatest.integ.RunStatus
import java.util.Observable
import org.scalatest.integ.ScopeStatus
import javax.swing.tree.TreePath
import javax.swing.tree.TreeNode
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.collection.JavaConversions._

private class ResultTree(treeActionProvider: TreeActionProvider) extends JPanel with Observer {
  
  import SwingHelper._
  
  private val tree = new JTree()
  private var root: RunModel = null
  private var model: DefaultTreeModel = null
  
  setLayout(new GridLayout(1, 1))
  initTree()
  
  private def initTree() {
    tree.getSelectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
    tree.setCellRenderer(new NodeRenderer())
    tree.addMouseListener(new MouseAdapter() {
      override def mousePressed(e: MouseEvent) {
        treeActionProvider.mousePressed(e, getSelectedNode)
      }
    })
    tree.addKeyListener(new KeyAdapter() {
      override def keyPressed(e: KeyEvent) {
        treeActionProvider.keyPressed(e, getSelectedNode)
      }
    })
    add(new JScrollPane(tree))
    resetRoot(null)
  }
  
  private def getSelectedNode = {
    try {
      tree.getSelectionPath.getLastPathComponent match {
        case node: Node => 
          node
        case _ =>
          null
      }
    }
    catch {
      case e: Throwable => 
        null
    }
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
      case resultController: ResultController => 
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
                invokeLater {
                  model.reload()
                  selectNode(resultController.findNextFailure(null))
                }
              case RunStatus.STOPPED =>
                invokeLater {
                  model.reload()
                  selectNode(resultController.findNextFailure(null))
                }
              case RunStatus.ABORTED => 
                invokeLater {
                  model.reload()
                  selectNode(resultController.findNextFailure(null))
                }
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
  
  // Returns a TreePath containing the specified node.
  private def getPath(node: TreeNode) = {
     @tailrec
    def getPathAcc(acc: List[TreeNode], node: TreeNode): List[TreeNode] = 
      if (node.getParent == null)
        (node :: acc).reverse
      else
        getPathAcc(node :: acc, node.getParent)
    val nodeArray: Array[AnyRef] = getPathAcc(List.empty, node).reverse.toArray
    new TreePath(nodeArray)
  }
  
  private def selectNode(node: Node)  {
    if (node != null) {
      val treePath = getPath(node)
      tree.setSelectionPath(treePath)
      tree.scrollPathToVisible(treePath)
    }
  }
}