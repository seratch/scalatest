package org.scalatest.integ.ui

import javax.swing.ImageIcon

private[ui] object Icons {
  private val loader = getClass.getClassLoader
  
  val ENABLED_NEXT_FAILURE = new ImageIcon(loader.getResource("images/e_next_failed.gif"))
  val DISABLED_NEXT_FAILURE = new ImageIcon(loader.getResource("images/d_next_failed.gif"))
  val ENABLED_PREVIOUS_FAILURE = new ImageIcon(loader.getResource("images/e_prev_failed.gif"))
  val DISABLED_PREVIOUS_FAILURE = new ImageIcon(loader.getResource("images/d_prev_failed.gif"))
  val SHOW_FAILURE_ONLY = new ImageIcon(loader.getResource("images/show_failed_only.gif"))
  
  val TESTS = new ImageIcon(loader.getResource("images/tests.gif"))
  val SUCCEEDED = new ImageIcon(loader.getResource("images/test_succeeded.gif"))
  val FAILED = new ImageIcon(loader.getResource("images/test_failed.gif"))
  val IGNORED = new ImageIcon(loader.getResource("images/test_ignored.gif"))
  val PENDING = new ImageIcon(loader.getResource("images/test_pending.gif"))
  val CANCELED = new ImageIcon(loader.getResource("images/test_canceled.gif"))
  val SUITE = new ImageIcon(loader.getResource("images/suite.gif"))
  val SUITE_ABORTED = new ImageIcon(loader.getResource("images/suite_aborted.gif"))
  val INFO = new ImageIcon(loader.getResource("images/info.gif"))
  
  val STACKTRACE = new ImageIcon(loader.getResource("images/stacktrace.gif"))
  val STACKFOLD = new ImageIcon(loader.getResource("images/stackfold.gif"))
}