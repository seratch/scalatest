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
import scala.annotation.tailrec

class FeatureSpecFinder extends Finder {
  
  @tailrec
  final def find(node: AstNode): Option[Selection] = {
    node match {
      case MethodInvocation(className, target, parent, children, "scenario", args)
        if args.length > 0 && args(0).isInstanceOf[StringLiteral] => 
        parent match {
          case ConstructorBlock(_, _) =>
            Some(new Selection(className, "Scenario: " + args(0).toString.toString, Array("Scenario: " + args(0).toString)))
          case MethodInvocation(_, _, _, _, "feature", parentArgs)
            if parentArgs.length > 0 && parentArgs(0).isInstanceOf[StringLiteral] => 
              val testName = "Feature: " + parentArgs(0) + " Scenario: " + args(0).toString
              Some(new Selection(className, testName, Array(testName)))
          case _ =>
            if (node.parent != null)
              find(node.parent)
            else
              None
        }
        
      case MethodInvocation(className, target, parent, children, "feature", args) 
        if args.length > 0 && args(0).isInstanceOf[StringLiteral] => 
        parent match {
          case ConstructorBlock(_, _) => 
            val featureText = "Feature: " + args(0).toString
            val testNameList = children.filter( childNode => 
                               childNode.isInstanceOf[MethodInvocation] 
                               && childNode.name == "scenario" 
                               && childNode.asInstanceOf[MethodInvocation].args.length > 0
                               && childNode.asInstanceOf[MethodInvocation].args(0).isInstanceOf[StringLiteral]
                               ).map { childNode => 
                                 val child = childNode.asInstanceOf[MethodInvocation]
                                 featureText + " Scenario: " + child.args(0)
                               }
            Some(new Selection(className, featureText, testNameList.toArray))
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
}