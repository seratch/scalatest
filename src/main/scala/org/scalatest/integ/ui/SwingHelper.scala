package org.scalatest.integ.ui

import javax.swing.SwingUtilities
import javax.swing.Icon
import javax.swing.JButton

private object SwingHelper {
  def invokeLater(f: => Unit) {
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        f
      }
    })
  }
  def createIconButton(enabledIcon: Icon, disabledIcon: Icon) = {
    val button = new JButton(enabledIcon)
    button.setDisabledIcon(disabledIcon)
    button
  }
}