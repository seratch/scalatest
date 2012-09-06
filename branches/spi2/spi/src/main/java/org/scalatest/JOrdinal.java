package org.scalatest;

import java.util.List;

public interface JOrdinal {

  JOrdinal next();
    
  JOrdinal[] nextNewOldPairArray();
  
  int[] asArray();
}
