package org.scalatest.fingerprints;

/**
* Indicates that classes (and possibly modules) that extend a particular superclass,
* or mix in a particular supertrait, should be discovered as test classes.
*/
public interface SubclassFingerprint {

  /**
   * Indicates whether modules (singleton objects) that extend the superclass or
   * supertrait should be considered during discovery, or just classes.
   *
   * <p>
   * If a test framework allows both classes and modules, they should return two different
   * fingerprints from <code>ServiceProvider.fingerprints</code>, one that returns <code>false</code> for
   * <code>isModule</code> and another that returns <code>true</code>.
   * </p>
   */ 
  public boolean isModule();

  /**
   * The name of the superclass or supertriat that identifies classes (and possibly modules) as test
   * classes to be discovered.
   */
  public String superclassName();
    
}
