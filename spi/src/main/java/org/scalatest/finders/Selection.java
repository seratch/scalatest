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

public class Selection {
    
    private String className;
    private String displayName;
    private String[] testNames;
  
    public Selection(String className, String displayName, String[] testNames) {
        this.className = className;
        this.displayName = displayName;
        this.testNames = testNames;
    }

    public String className() {
        return className;
    }

    public String displayName() {
        return displayName;
    }

    public String[] testNames() {
        return testNames;
    }
}
