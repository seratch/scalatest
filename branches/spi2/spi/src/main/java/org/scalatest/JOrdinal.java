package org.scalatest;

public interface JOrdinal {

  JOrdinal next();
    
  JOrdinal[] nextNewOldPairArray();
  
  int[] asArray();
}
