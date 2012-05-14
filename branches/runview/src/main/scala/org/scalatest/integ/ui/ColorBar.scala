package org.scalatest.integ.ui

import javax.swing.JPanel
import java.util.Observer
import java.awt.Color
import java.awt.Image
import java.awt.Graphics
import java.awt.Dimension
import java.util.Observable
import org.scalatest.integ.TestModel
import org.scalatest.integ.TestStatus
import org.scalatest.integ.ScopeModel
import org.scalatest.integ.SuiteModel
import org.scalatest.integ.RunModel
import org.scalatest.integ.RunStatus

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
      case resultController: ResultController => 
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
                setMax(resultController.totalCount)
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