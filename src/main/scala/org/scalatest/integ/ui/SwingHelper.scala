package org.scalatest.integ.ui

import javax.swing.SwingUtilities

private object SwingHelper {
  def invokeLater(f: => Unit) {
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        f
      }
    })
  }
}