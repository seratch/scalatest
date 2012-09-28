package org.scalatest;

public class JFilterResult {

  private boolean filtered;
  private boolean ignored;
  
  public JFilterResult(boolean filtered, boolean ignored) {
    this.filtered = filtered;
    this.ignored = ignored;
  }
    
  public boolean filtered() {
    return filtered;
  }
  
  public boolean ignored() {
    return ignored;
  }
}
