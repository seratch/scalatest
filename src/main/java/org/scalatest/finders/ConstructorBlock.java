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

package org.scalatest.finders;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ConstructorBlock implements AstNode {
  
  private String className;
  private AstNode parent;
  private List<AstNode> children;
    
  public ConstructorBlock(String className, AstNode parent, AstNode[] childrenArr) {
    this.className = className;
    this.parent = parent;
    children = new ArrayList<AstNode>();
    children.addAll(Arrays.asList(childrenArr));
  }
    
  public String className() {
    return className;
  }
  
  public AstNode parent() {
    return parent;
  }
  
  public AstNode[] children() {
    return children.toArray(new AstNode[children.size()]);
  }
  
  public String name() {
    return "constructor";
  }
  
  public void addChild(AstNode node) {
    if (!children.contains(node)) 
        children.add(node);
  }
}

/*class ConstructorBlock(
    pClassName: String, 
    pChildren: Array[AstNode]) 
extends AstNode {
  import scala.collection.mutable.ListBuffer
  private val childrenBuffer = new ListBuffer[AstNode]()
  childrenBuffer ++= pChildren
  // Because parent of constructor block is always null now, should enable this when we add ClassDef later.
  def className = pClassName
  def parent = null
  def children = childrenBuffer.toArray
  def name = "constructor"
  def addChild(node: AstNode) = if (!childrenBuffer.contains(node)) childrenBuffer += node
}

object ConstructorBlock {
  def apply(className: String, children: Array[AstNode]) = new ConstructorBlock(className, children)
  def unapply(value: ConstructorBlock): Option[(String, Array[AstNode])] = if (value != null) Some((value.className, value.children)) else None
}*/