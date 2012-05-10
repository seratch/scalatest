/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalatest.finders

class StringLiteral(pClassName: String, pParent: AstNode, pValue: String) extends AstNode {
  def className = pClassName
  lazy val parent = getParent
  protected def getParent() = {
    if (pParent != null)
      pParent.addChild(this)
    pParent
  }
  def children = Array.empty
  def name = "StringLiteral"
  def addChild(node: AstNode) = throw new UnsupportedOperationException("StringLiteral does not support addChild method.")
  val value = pValue
  override def toString() = {
    value.toString
  }
}

object StringLiteral {
  def apply(className: String, parent: AstNode, value: String) = 
    new StringLiteral(className, parent, value)
  def unapply(lit: StringLiteral): Option[(String, AstNode, String)] = 
    if (lit != null)
      Some((lit.className, lit.parent, lit.value))
    else
      None
}