package org.scalatest;

import org.scalatest.fingerprints.Fingerprint;
import org.scalatest.JSuite;

public interface ServiceProvider {

  Fingerprint[] fingerprints();
  
  JSuite suite(Fingerprint fingerprint, String className);
}
