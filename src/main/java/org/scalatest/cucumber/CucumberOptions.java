package org.scalatest.cucumber;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.ElementType;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface CucumberOptions {
    /**
     * @return true if this is a dry run
     */
    boolean dryRun() default false;

    /**
     * @return the paths to the feature(s)
     */
    String[] features() default {};

    /**
     * @return where to look for glue code (stepdefs and hooks)
     */
    String[] glue() default {};

    /**
     * @return what tags in the feature should be executed
     */
    String[] tags() default {};

    /**
     * @return what formatter(s) to use
     */
    String[] format() default {};
}
