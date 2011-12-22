package org.scalatest.spi.location;

public class ToStringOwner extends AstNode {
    
    private Object obj;
    
    public ToStringOwner(AstNode parent, AstNode[] children, Object obj) {
        this.parent = parent;
        this.children = children;
        this.obj = obj;
    }
    @Override
    public String getClassName() {
        return obj.getClass().getName();
    }
    @Override
    public String getName() {
        return obj.toString();
    }
    @Override
    public String toString() {
        return obj.toString();
    }
}
