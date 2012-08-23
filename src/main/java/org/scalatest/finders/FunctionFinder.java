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

abstract class FunctionFinder implements Finder {
  protected abstract String getName();
  
  public Selection find(AstNode node) {
    Selection result = null;
    while (result == null) {
      if (node instanceof MethodInvocation) {
        MethodInvocation methodInv = (MethodInvocation) node;
        if (getName().equals(methodInv.name()) && methodInv.parent() != null && methodInv.parent() instanceof ConstructorBlock && methodInv.args()[0] instanceof StringLiteral)
          result = new Selection(methodInv.className(), methodInv.className() + ": \"" + methodInv.args()[0].toString() + "\"", new String[] { methodInv.args()[0].toString() });
        else {
          if (node.parent() != null) 
            node = node.parent();
          else
            break;
        }
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

/*package org.scalatest.finders

import scala.annotation.tailrec

trait FunctionFinder extends Finder {
  
  val name: String 

  @tailrec
  final def find(node: AstNode): Option[Selection] = {
    node match {
      case MethodInvocation(className, target, parent, children, name, args)
        if parent != null && parent.isInstanceOf[ConstructorBlock] && args.length > 0 && args(0).isInstanceOf[StringLiteral] =>
          Some(new Selection(className, className + ": \"" + args(0).toString + "\"", Array(args(0).toString)))
      case _ => 
        if (node.parent != null)
          find(node.parent)
        else
          None
    }
  }
  
}*/