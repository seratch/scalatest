package org.scalatest.finders;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ClassDefinition implements AstNode {

  private String className;
  private AstNode parent;
  private List<AstNode> children;
  private String name;
  private String[] paramTypes;
  
  public ClassDefinition(String className, AstNode parent, AstNode[] childrenArr, String name, String... paramTypes) {
    this.className = className;
    this.parent = parent;
    if (parent != null)
      parent.addChild(this);
    children = new ArrayList<AstNode>();
    children.addAll(Arrays.asList(childrenArr));
    this.name = name;
    this.paramTypes = paramTypes;
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
    return name;
  }
    
  public void addChild(AstNode node) {
    if (!children.contains(node)) 
      children.add(node);
  }
  
  public String[] paramTypes() {
    return paramTypes;
  }
}
