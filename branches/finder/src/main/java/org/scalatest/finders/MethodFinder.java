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

public class MethodFinder implements Finder {

    public Selection find(AstNode node) {
      Selection result = null;
      while (result == null) {
        if (node instanceof MethodDefinition) {
          MethodDefinition methodDef = (MethodDefinition) node;
          if (methodDef.parent() != null && methodDef.parent() instanceof ConstructorBlock && methodDef.paramTypes().length == 0) {
            String displayName = NameTransformer.decode(methodDef.className()) + "." + methodDef.name();
            String testName = NameTransformer.encode(methodDef.name());
            result = new Selection(methodDef.className(), displayName, new String[] { testName });
          }
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
import scala.reflect.NameTransformer

class MethodFinder extends Finder {

  @tailrec
  final def find(node: AstNode): Option[Selection] = {
    node match {
      case MethodDefinition(className, parent, children, name, paramTypes) 
        if parent != null && parent.isInstanceOf[ConstructorBlock] && paramTypes.length == 0 =>
          Some(new Selection(className, NameTransformer.decode(className) + "." + name, Array(NameTransformer.encode(name))))
      case _ => 
        if (node.parent != null)
          find(node.parent)
        else
          None
    }
  }
  
}*/