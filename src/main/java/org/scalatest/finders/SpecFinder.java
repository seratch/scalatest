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

public class SpecFinder implements Finder {
    
  private String getTestNameBottomUp(AstNode node) {
    String result = "";
    if (node instanceof MethodDefinition || node instanceof ModuleDefinition) {
      result = node.name();
      node = node.parent();
      while (node != null) {
        if (node instanceof ModuleDefinition) {
          result = ((ModuleDefinition) node).name() + " " + result;
          node = node.parent();
        }
        else if (node instanceof ConstructorBlock) {
          node = node.parent();
        }
        else if (node instanceof ClassDefinition) {
          if (node.parent() == null) // Require be top level class
            node = null;
          else {
            // Nested class will not be recognized.
            result = "";
            node = null;
          }
        }
        else {
          result = "";
          node = null;
        }
      }
    }
    return result;
  }
  
  private List<String> getTestNamesTopDown(AstNode node) {
    List<String> results = new ArrayList<String>();
    List<AstNode> nodes = new ArrayList<AstNode>();
    nodes.add(node);
    
    while (nodes.size() > 0) {
      AstNode head = nodes.remove(0);
      if (head instanceof MethodDefinition) {
        MethodDefinition methodDef = (MethodDefinition) head;
        if (methodDef.name().indexOf(" ") >= 0) {
          String testName = getTestNameBottomUp(methodDef);
          if (testName.length() > 0)
            results.add(testName);
        }
      }
      else if (head instanceof ModuleDefinition || head instanceof ConstructorBlock) 
        nodes.addAll(0, Arrays.asList(head.children()));
    }
    
    return results;
  }

  public Selection find(AstNode node) {
    Selection result = null;
    while (result == null) {
      if (node instanceof MethodDefinition && node.name().indexOf(" ") >= 0) {
        String testName = getTestNameBottomUp(node);
        result = new Selection(node.className(), testName, new String[] { testName });
      }
      else if (node instanceof ModuleDefinition && node.name().indexOf(" ") >= 0) {
        String displayName = getTestNameBottomUp(node);
        List<String> testNames = getTestNamesTopDown(node);
        result = new Selection(node.className(), displayName, testNames.toArray(new String[testNames.size()]));
      }
      else {
        if (node.parent() != null) 
          node = node.parent();
        else
          break;
      }
    }
    return result;
  }
}
