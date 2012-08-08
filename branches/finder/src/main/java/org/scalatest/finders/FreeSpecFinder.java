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

public class FreeSpecFinder implements Finder {
  
  private String getTestNameBottomUp(MethodInvocation invocation) {
    String result = "";
    while (invocation != null) {
      result = invocation.target().toString() + " " + result;
      if (invocation.parent() != null && invocation.parent() instanceof MethodInvocation)
        invocation = (MethodInvocation) invocation.parent();
      else
        invocation = null;
    }
    return result.trim();      
  }
   
  private List<String> getTestNamesTopDown(MethodInvocation invocation) {
    List<String> results = new ArrayList<String>();
    List<AstNode> nodes = new ArrayList<AstNode>();
    nodes.add(invocation);
      
    while (nodes.size() > 0) {
      AstNode head = nodes.remove(0);
      if (head instanceof MethodInvocation) {
        MethodInvocation headInvocation = (MethodInvocation) head;
        if (headInvocation.name().equals("in") || headInvocation.name().equals("is")) {
          String testName = getTestNameBottomUp(headInvocation);
          results.add(testName);
        }
        else
          nodes.addAll(0, Arrays.asList(headInvocation.children()));
      }
    }
      
    return results;
  }
  
  public Selection find(AstNode node) {
    Selection result = null;
    while (result == null) {
      if (node instanceof MethodInvocation) {
        MethodInvocation invocation = (MethodInvocation) node;
        String name = invocation.name();
        AstNode parent = invocation.parent();
        if ((name.equals("in") || name.equals("is")) && parent.name().equals("-")) {
          String testName = getTestNameBottomUp(invocation);
          result = new Selection(invocation.className(), testName, new String[] { testName });
        }
        else if (name.equals("-")) {
          String displayName = getTestNameBottomUp(invocation);
          List<String> testNames = getTestNamesTopDown(invocation);
          result = new Selection(invocation.className(), displayName, testNames.toArray(new String[testNames.size()]));
        }
      }
        
      if (result == null) {
        if (node.parent() != null) 
          node = node.parent();
        else
          break;
      }
    }
    return result;
  }
}

/*package org.scalatest.finders

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

class FreeSpecFinder extends Finder {
  
  private def getTestNameBottomUp(invocation: MethodInvocation): String = {
    if(invocation.parent == null || !invocation.parent.isInstanceOf[MethodInvocation])
      invocation.target.toString
    else
      getTestNameBottomUp(invocation.parent.asInstanceOf[MethodInvocation]) + " " + invocation.target.toString  
  }
  
  private def getTestNamesTopDown(invocation: MethodInvocation): List[String] = {
    @tailrec
    def getTestNamesTopDownAcc(invocations: List[AstNode], acc: List[String]): List[String] = invocations match {
      case Nil => acc
      case node :: rs => 
        node match {
          case invocation: MethodInvocation => 
            if (invocation.name == "in" || invocation.name == "is") {
              val testName = getTestNameBottomUp(invocation)
              getTestNamesTopDownAcc(rs, testName :: acc)
            }
            else {
              getTestNamesTopDownAcc(invocation.children.toList ::: rs, acc)
            }
          case _ => getTestNamesTopDownAcc(rs, acc)
        }
    }
    getTestNamesTopDownAcc(List(invocation), List.empty).reverse
  }
  
  def find(node: AstNode): Option[Selection] = {
    node match {
      case invocation @ MethodInvocation(className, target, parent, children, name, args) =>
        name match {
          case "in" | "is" if parent.name == "-" =>
            val testName = getTestNameBottomUp(invocation)
            Some(new Selection(className, testName, Array(testName)))
          case "-" =>
            val displayName = getTestNameBottomUp(invocation)
            val testNames = getTestNamesTopDown(invocation)
            Some(new Selection(className, displayName, testNames.toArray))
          case _ => 
            if (node.parent != null)
              find(node.parent)
            else
              None
        }
      case _ => 
        if (node.parent != null)
          find(node.parent)
        else
          None
    }
  }
}*/