/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.selenium

import org.openqa.selenium.WebDriver
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.firefox.FirefoxProfile
import org.openqa.selenium.safari.SafariDriver
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.ie.InternetExplorerDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.openqa.selenium.By
import org.openqa.selenium.WebElement
import java.util.concurrent.TimeUnit
import org.openqa.selenium.support.ui.WebDriverWait
import org.openqa.selenium.support.ui.Clock
import org.openqa.selenium.support.ui.Sleeper
import org.openqa.selenium.support.ui.ExpectedCondition
import scala.collection.mutable.Buffer
import scala.collection.JavaConverters._
import org.openqa.selenium.Cookie
import java.util.Date
import org.scalatest.time.Span
import org.scalatest.time.Milliseconds
import org.openqa.selenium.TakesScreenshot
import org.openqa.selenium.OutputType
import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import org.openqa.selenium.Alert
import org.openqa.selenium.support.ui.Select
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.StackDepthException
import org.openqa.selenium.JavascriptExecutor
import org.scalatest.ScreenshotCapturer

/**
 * Trait that provides a domain specific language (DSL) for writing browser-based tests using <a href="http://seleniumhq.org">Selenium</a>.  
 *
 * To use ScalaTest's Selenium DSL, mix trait <code>WebBrowser</code> into your test class. This trait provides the DSL in its
 * entirety except for one missing piece: an implicit <code>org.openqa.selenium.WebDriver</code>. One way to provide the missing
 * implicit driver is to declare one as a member of your test class, like this:
 * 
 * <pre class="stHighlight">
 * class BlogSpec extends FlatSpec with ShouldMatchers with WebBrowser {
 *
 *   implicit val webDriver: WebDriver = new HtmlUnitDriver
 *
 *   "The blog app home page" should "have the correct title" in {
 *     go to (host + "index.html")
 *     title should be ("Awesome Blog")
 *   }
 * }
 * </pre>
 * 
 * <p>
 * For convenience, however, ScalaTest provides a <code>WebBrowser</code> subtrait containing an implicit <code>WebDriver</code> for each
 * driver provided by Selenium. 
 * Thus a simpler way to use the <code>HtmlUnit</code> driver, for example, is to extend
 * ScalaTest's <a href="HtmlUnit.html"><code>HtmlUnit</code></a> trait, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * class BlogSpec extends FlatSpec with ShouldMatchers with HtmlUnit {
 *
 *   "The blog app home page" should "have the correct title" in {
 *     go to (host + "index.html")
 *     title should be ("Awesome Blog")
 *   }
 * }
 * </pre>
 * 
 * <p>
 * The web driver traits provided by ScalaTest are:
 * </p>
 * 
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Driver</strong></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong><code>WebBrowser</code> subtrait</strong></th></tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * Google Chrome
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="Chrome.html"><code>Chrome</code></a>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * Mozilla Firefox
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="Firefox.html"><code>Firefox</code></a>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * HtmlUnit
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="HtmlUnit.html"><code>HtmlUnit</code></a>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * Microsoft Internet Explorer
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="InternetExplorer.html"><code>InternetExplorer</code></a>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * Apple Safari
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">
 * <a href="Safari.html"><code>Safari</code></a>
 * </td>
 * </tr>
 * </table>
 *
 * <h2>Navigation</h2>
 *
 * <p>
 * You can ask the browser to retrieve a page (go to a URL) like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * go to "http://www.artima.com"
 * </pre>
 * 
 * <p>
 * Note: If you are using the <em>page object pattern</em>, you can also go to a page using the <code>Page</code> instance, as
 * illustrated in the section on <a href="#pageObjects">page objects</a> below.
 * </p>
 *
 * <p>
 * Once you have retrieved a page, you can fill in and submit forms, query for the values of page elements, and make assertions.  
 * In the following example, selenium will go to <code>http://www.google.com</code>, fill in the text box with
 * <code>Cheese!</code>, press the submit button, and wait for result returned from an AJAX call:
 * </p>
 * 
 * <pre class="stHighlight">
 * go to "http://www.google.com"
 * click on "q"
 * enter("Cheese!")
 * submit()
 * // Google's search is rendered dynamically with JavaScript.
 * eventually { title should be ("Cheese! - Google Search") }
 * </pre>
 * 
 * <p>
 * In the above example, the <code>"q"</code> used in &ldquo;<code>click on "q"</code>&rdquo; 
 * can be either the id or name of an element. ScalaTest's Selenium DSL will try to lookup by id first. If it cannot find 
 * any element with an id equal to <code>&quot;q&quot;</code>, it will then try lookup by name <code>&quot;q&quot;</code>.
 * </p>
 * 
 * <p>
 * Alternatively, you can be more specific:
 * </p>
 * 
 * <pre class="stHighlight">
 * click on id("q")   // to lookup by id "q" 
 * click on name("q") // to lookup by name "q" 
 * </pre>
 * 
 * <p>
 * In addition to <code>id</code> and <code>name</code>, you can use the following approaches to lookup elements, just as you can do with
 * Selenium's <code>org.openqa.selenium.By</code> class:
 * </p>
 * 
 * <ul>
 *   <li><code>xpath</code></li>
 *   <li><code>className</code></li>
 *   <li><code>cssSelector</code></li>
 *   <li><code>linkText</code></li>
 *   <li><code>partialLinkText</code></li>
 *   <li><code>tagName</code></li>
 * </ul>
 * 
 * <p>
 * For example, you can select by link text with:
 * </p>
 *
 * <pre class="stHighlight">
 * click on linkText("click here!")
 * </pre>
 * 
 * <p>
 * If an element is not found via any form of lookup, evaluation will complete abruptly with a <code>TestFailedException</code>.
 * <p>
 *
 * <h2>Getting and setting input element values</h2>
 * 
 * <p>
 * ScalaTest's Selenium DSL provides a clear, simple syntax for accessing and updating the values of input elements such as
 * text fields, radio buttons, checkboxes, and selection lists. If a requested element is not found, or if it is found but is
 * not of the requested type, an exception will immediately result causing the test to fail.
 * <p>
 *
 * <h3>Text fields and text areas</h3>
 * 
 * <p>
 * You can change a text field's value by assigning it via the <code>=</code> operator, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * textField("q").value = "Cheese!"
 * </pre>
 * 
 * <p>
 * And you can access a text field's value by simply invoking <code>value</code> on it:
 * </p>
 * 
 * <pre class="stHighlight">
 * textField("q").value should be ("Cheese!")
 * </pre>
 * 
 * <p>
 * If the text field is empty, <code>value</code> will return an empty string (<code>""</code>).
 * </p>
 * 
 * <p>
 * You can use the same syntax with text areas by replacing <code>textField</code> with <code>textArea</code>, as in:
 * </p>
 * 
 * <pre class="stHighlight">
 * textArea("body").value = "I saw something cool today!"
 * textArea("body").value should be ("I saw something cool today!")
 * </pre>
 * 
 * <p>
 * An alternate way to enter data into a text field or text area is to use <code>enter</code> or <code>pressKeys</code>.
 * Although both of these send characters to the active element, <code>pressKeys</code> can be used on any kind of
 * element, whereas <code>enter</code> can only be used on text fields and text areas. Another difference is that <code>enter</code>
 * will clear the text field or area before sending the characters, effectively replacing any currently existing text with the
 * new text passed to <code>enter</code>. By contrast, <code>pressKeys</code> does not do any clearing&#8212;it just appends
 * more characters to any existing text. You can backup with <code>pressKeys</code>, however, by sending explicit backspace
 * characters, <code>"&#92;u0008"</code>.
 * </p>
 * 
 * <p>
 * To use these commands, you must first click on the text field or area you are interested in
 * to give it the focus. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * click on "q"
 * enter("Cheese!")
 * </pre>
 * 
 * <p>
 * Here's a (contrived) example of using <code>pressKeys</code> with backspace to fix a typo:
 * </p>
 *
 * <pre class="stHighlight">
 * click on "q"              // q is the name or id of a text field or text area
 * enter("Cheesey!")         // Oops, meant to say Cheese!
 * pressKeys("&#92;u0008&#92;u0008") // Send two backspaces; now the value is Cheese
 * pressKeys("!")            // Send the missing exclamation point; now the value is Cheese!
 * </pre>
 * 
 * <h3>Radio buttons</h3>
 * 
 * <p>
 * Radio buttons work together in groups. For example, you could have a group of radio buttons, like this:
 * </p>
 * 
 * <pre>
 * &lt;input type="radio" id="opt1" name="group1" value="Option 1"&gt; Option 1&lt;/input&gt;
 * &lt;input type="radio" id="opt2" name="group1" value="Option 2"&gt; Option 2&lt;/input&gt;
 * &lt;input type="radio" id="opt3" name="group1" value="Option 3"&gt; Option 3&lt;/input&gt;
 * </pre>
 * 
 * <p>
 * You can select an option in either of two ways:
 * </p>
 *
 * <pre class="stHighlight">
 * radioButtonGroup("group1").value = "Option 2"
 * radioButtonGroup("group1").selection = Some("Option 2")
 * </pre>
 *
 * <p>
 * Likewise, you can read the currently selected value of a group of radio buttons in two ways:
 * </p>
 *
 * <pre class="stHighlight">
 * radioButtonGroup("group1").value should be ("Option 2")
 * radioButtonGroup("group1").selection should be (Some("Option 2"))
 * </pre>
 * 
 * <p>
 * If the radio button has no selection at all, <code>selection</code> will return <code>None</code> whereas <code>value</code>
 * will throw a <code>TestFailedException</code>. By using <code>value</code>, you are indicating you expect a selection, and if there
 * isn't a selection that should result in a failed test.
 * </p>
 * 
 * <p>
 * If you would like to work with <code>RadioButton</code> element directly, you can select it by calling <code>radioButton</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * click on radioButton("opt1")
 * </pre>
 * 
 * <p>
 * you can check if an option is selected by calling <code>isSelected</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * radioButton("opt1").isSelected should be (true)
 * </pre>
 * 
 * <p>
 * to get the value of radio button, you can call <code>value</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * radioButton("opt1").value should be ("Option 1")
 * </pre>
 * 
 * <h3>Checkboxes</h3>
 * 
 * <p>
 * A checkbox in one of two states: selected or cleared. Here's how you select a checkbox:
 * </p>
 * 
 * <pre class="stHighlight">
 * checkbox("cbx1").select()
 * </pre>
 * 
 * <p>
 * And here's how you'd clear one:
 * </p>
 * 
 * <pre class="stHighlight">
 * checkbox("cbx1").clear()
 * </pre>
 * 
 * <p>
 * You can access the current state of a checkbox with <code>isSelected</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * checkbox("cbx1").isSelected should be (true)
 * </pre>
 * 
 * <h3>Single-selection dropdown lists</h3>
 * 
 * <p>
 * Given the following single-selection dropdown list:
 * </p>
 * 
 * <pre>
 * &lt;select id="select1"&gt;
 *  &lt;option value="option1"&gt;Option 1&lt;/option&gt;
 *  &lt;option value="option2"&gt;Option 2&lt;/option&gt;
 *  &lt;option value="option3"&gt;Option 3&lt;/option&gt;
 * &lt;/select&gt;
 * </pre>
 * 
 * <p>
 * You could select <code>Option 2</code> in either of two ways:
 * </p>
 * 
 * <pre class="stHighlight">
 * singleSel("select1").value = "option2"
 * singleSel("select1").selection = Some("option2")
 * </pre>
 * 
 * <p>
 * To clear the selection, either invoke <code>clear</code> or set <code>selection</code> to <code>None</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * singleSel.clear()
 * singleSel("select1").selection = None
 * </pre>
 * 
 * <p>
 * You can read the currently selected value of a single-selection list in the same manner as radio buttons:
 * </p>
 *
 * <pre class="stHighlight">
 * singleSel("select1").value should be ("option2")
 * singleSel("select1").selection should be (Some("option2"))
 * </pre>
 * 
 * <p>
 * If the single-selection list has no selection at all, <code>selection</code> will return <code>None</code> whereas <code>value</code>
 * will throw a <code>TestFailedException</code>. By using <code>value</code>, you are indicating you expect a selection, and if there
 * isn't a selection that should result in a failed test.
 * </p>
 * 
 * <h3>Multiple-selection lists</h3>
 * 
 * <p>
 * Given the following multiple-selection list:
 * </p>
 * 
 * <pre>
 * &lt;select name="select2" multiple="multiple"&gt;
 *  &lt;option value="option4"&gt;Option 4&lt;/option&gt;
 *  &lt;option value="option5"&gt;Option 5&lt;/option&gt;
 *  &lt;option value="option6"&gt;Option 6&lt;/option&gt;
 * &lt;/select&gt;
 * </pre>
 * 
 * <p>
 * You could select <code>Option 5</code> and <code>Option 6</code> like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * multiSel("select2").values = Seq("option5", "option6")
 * </pre>
 * 
 * <p>
 * The previous command would essentially clear all selections first, then select <code>Option 5</code> and <code>Option 6</code>.
 * If instead you want to <em>not</em> clear any existing selection, just additionally select <code>Option 5</code> and <code>Option 6</code>,
 * you can use the <code>+=</code> operator, like this.
 * </p>
 * 
 * <pre class="stHighlight">
 * multiSel("select2").values += "option5"
 * multiSel("select2").values += "option6"
 * </pre>
 * 
 * <p>
 * To clear a specific option, pass its name to <code>clear</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * multiSel("select2").clear("option5")
 * </pre>
 * 
 * <p>
 * To clear all selections, call <code>clearAll</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * multiSel("select2").clearAll()
 * </pre>
 * 
 * <p>
 * You can access the current selections with <code>values</code>, which returns an <code>IndexedSeq[String]</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * multiSel("select2").values should have size 2
 * multiSel("select2").values(0) should be ("option5")
 * multiSel("select2").values(1) should be ("option6")
 * </pre>
 * 
 * <h3>Clicking and submitting</h3>
 * 
 * <p>
 * You can click on any element with &ldquo;<code>click on</code>&rdquo; as shown previously:
 * </p>
 * 
 * <pre class="stHighlight">
 * click on "aButton"
 * click on name("aTextField")
 * </pre>
 * 
 * <p>
 * If the requested element is not found, <code>click on</code> will throw an exception, failing the test.
 * </p>
 * 
 * <p>
 * Clicking on a input element will give it the focus. If current focus is in on an input element within a form, you can submit the form by 
 * calling <code>submit</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * submit()
 * </pre>
 * 
 * <h2>Switching</h2>
 * 
 * <p>
 * You can switch to a popup alert using the following code:
 * </p>
 * 
 * <pre class="stHighlight">
 * switch to alert
 * </pre>
 * 
 * <p>
 * to switch to a frame, you could:
 * </p>
 * 
 * <pre class="stHighlight">
 * switch to frame(0) // switch by index
 * switch to frame("name") // switch by name
 * </pre>
 * 
 * <p>
 * If you have reference to a window handle (can be obtained from calling windowHandle/windowHandles), you can switch to a particular 
 * window by:
 * </p>
 * 
 * <pre class="stHighlight">
 * switch to window(windowHandle)
 * </pre>
 * 
 * <p>
 * You can also switch to active element and default content:
 * </p>
 * 
 * <pre class="stHighlight">
 * switch to activeElement
 * switch to defaultContent
 * </pre>
 * 
 * <h2>Navigation history</h2>
 * 
 * <p>
 * In real web browser, you can press the 'Back' button to go back to previous page.  To emulate that action in your test, you can call <code>goBack</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * goBack()
 * </pre>
 * 
 * <p>
 * To emulate the 'Forward' button, you can call:
 * </p>
 * 
 * <pre class="stHighlight">
 * goForward()
 * </pre>
 * 
 * And to refresh or reload the current page, you can call:
 * 
 * <pre class="stHighlight">
 * reloadPage()
 * </pre>
 * 
 * <h2>Cookies!</h2>
 * 
 * <p>To create a new cookie, you'll say:</p>
 * 
 * <pre class="stHighlight">
 * add cookie ("cookie_name", "cookie_value")
 * </pre>
 * 
 * <p>
 * to read a cookie value, you do:
 * </p>
 * 
 * <pre class="stHighlight">
 * cookie("cookie_name").value should be ("cookie_value") // If value is undefined, throws TFE right then and there. Never returns null.
 * </pre>
 * 
 * <p>
 * In addition to the common use of name-value cookie, you can pass these extra fields when creating the cookie, available ways are:
 * </p>
 * 
 * <pre class="stHighlight">
 * cookie(name: String, value: String)
 * cookie(name: String, value: String, path: String)
 * cookie(name: String, value: String, path: String, expiry: Date)
 * cookie(name: String, value: String, path: String, expiry: Date, domain: String)
 * cookie(name: String, value: String, path: String, expiry: Date, domain: String, secure: Boolean)
 * </pre>
 * 
 * and to read those extra fields:
 * 
 * <pre class="stHighlight">
 * cookie("cookie_name").value   // Read cookie's value
 * cookie("cookie_name").path    // Read cookie's path
 * cookie("cookie_name").expiry  // Read cookie's expiry
 * cookie("cookie_name").domain  // Read cookie's domain
 * cookie("cookie_name").isSecure  // Read cookie's isSecure flag
 * </pre>
 * 
 * <p>
 * In order to delete a cookie, you could use the following code: 
 * </p>
 * 
 * <pre class="stHighlight">
 * delete cookie "cookie_name"
 * </pre>
 * 
 * <p>
 * or to delete all cookies in the same domain:-
 * </p>
 * 
 * <pre class="stHighlight">
 * delete all cookies
 * </pre>
 * 
 * To get the underlying Selenium cookie, you can use <code>underlying</code>:
 * 
 * <pre class="stHighlight">
 * cookie("cookie_name").underlying.validate()  // call the validate() method on underlying Selenium cookie
 * </pre>
 * 
 * <h2>Other useful element properties</h2>
 * 
 * <p>
 * All element types (<code>textField</code>, <code>textArea</code>, <code>radioButton</code>, <code>checkbox</code>, <code>singleSel</code>, <code>multiSel</code>) 
 * support the following useful properties:
 * </p>
 * 
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Method</strong></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>Description</strong></th></tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>location</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * The XY location of the top-left corner of this <code>Element</code>.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>size</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * The width/height size of this <code>Element</code>.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>isDisplayed</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Indicates whether this <code>Element</code> is displayed.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>isEnabled</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Indicates whether this <code>Element</code> is enabled.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>isSelected</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Indicates whether this <code>Element</code> is selected.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>tagName</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * The tag name of this element.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>underlying</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * The underlying <code>WebElement</code> wrapped by this <code>Element</code>.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>attribute(name: String)</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * The attribute value of the given attribute name of this element, wrapped in a <code>Some</code>, or <code>None</code> if no
 * such attribute exists on this <code>Element</code>.
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>text</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * Returns the visible (<em>i.e.</em>, not hidden by CSS) text of this element, including sub-elements, without any leading or trailing whitespace.
 * </td>
 * </tr>
 * </table>
 * 
 * <h2>Implicit wait</h2>
 * 
 * <p>
 * To set Selenium's implicit wait timeout, you can call the <code>implicitlyWait</code> method:
 * </p>
 * 
 * <pre class="stHighlight">
 * implicitlyWait(Span(10, Seconds))
 * </pre>
 * 
 * <p>
 * Invoking this method sets the amount of time the driver will wait when searching for an element that is not immediately present. For
 * more information, see the documentation for method <code>implicitlyWait</code>.
 * </p>
 *
 * <h2>Page source and current URL</h2>
 * 
 * <p>
 * It is possible to get the html source of currently loaded page, using:
 * </p>
 * 
 * <pre class="stHighlight">
 * pageSource
 * </pre>
 * 
 * <p>
 * and if needed, get the current URL of currently loaded page:
 * </p>
 * 
 * <pre class="stHighlight">
 * currentUrl
 * </pre>
 * 
 * <h2>Screen capture</h2>
 * 
 * <p>
 * You can capture screen using the following code:
 * </p>
 * 
 * <pre class="stHighlight">
 * val file = capture
 * </pre>
 * 
 * <p>
 * By default, the captured image file will be saved in temporary folder (returned by java.io.tmpdir property), with random file name 
 * ends with .png extension.  You can specify a fixed file name:
 * </p>
 * 
 * <pre class="stHighlight">
 * capture to "MyScreenShot.png"
 * </pre>
 * 
 * <p>
 * or
 * </p>
 * 
 * <pre class="stHighlight">
 * capture to "MyScreenShot"
 * </pre>
 * 
 * <p>
 * Both will result in a same file name <code>MyScreenShot.png</code>.
 * </p>
 * 
 * <p>
 * You can also change the target folder screenshot file is written to, by saying:
 * </p>
 * 
 * <pre class="stHighlight">
 * setCaptureDir("/home/your_name/screenshots")
 * </pre>
 * 
 * <p>
 * If you want to capture a screenshot when something goes wrong (e.g. test failed), you can use <code>withScreenshot</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * withScreenshot {
 *   assert("Gold" == "Silver", "Expected gold, but got silver")
 * }
 * </pre>
 * 
 * <p>
 * In case the test code fails, you'll see the screenshot location appended to the error message, for example:
 * </p>
 * 
 * <pre class="stHighlight">
 * Expected gold but got silver; screenshot capture in /tmp/AbCdEfGhIj.png
 * </pre>
 * 
 * <a name="pageObjects"></a>
 * <h2>Using the page object pattern</h2>
 *
 * <p>
 * If you use the page object pattern, mixing trait <code>Page</code> into your page classes will allow you to use the <code>go to</code> 
 * syntax with your page objects. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * class HomePage extends Page {
 *   val url = "http://localhost:9000/index.html"
 * }
 *
 * val homePage = new HomePage
 * go to homePage
 * </pre>
 *
 * <h2>Executing JavaScript</h2>
 *
 * <p>
 * To execute arbitrary JavaScript, for example, to test some JavaScript functions on your page, pass it to <code>executeScript</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * go to (host + "index.html")
 * val result1 = executeScript("return document.title;")
 * result1 should be ("Test Title")
 * val result2 = executeScript("return 'Hello ' + arguments[0]", "ScalaTest")
 * result2 should be ("Hello ScalaTest")
 * </pre>
 *
 * <p>
 * To execute an asynchronous bit of JavaScript, pass it to <code>executeAsyncScript</code>. You can set the script timeout with <code>setScriptTimeout</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val script = """
 *   var callback = arguments[arguments.length - 1];
 *   window.setTimeout(function() {callback('Hello ScalaTest')}, 500);
 * """
 * setScriptTimeout(1 second)
 * val result = executeAsyncScript(script)
 * result should be ("Hello ScalaTest")
 * </pre>
 *
 * <h2>Querying for elements</h2>
 *
 * <p>
 * You can query for arbitrary elements via <code>find</code> and <code>findAll</code>. The <code>find</code> method returns the first matching element, wrapped in a <code>Some</code>,
 * or <code>None</code> if no element is found. The <code>findAll</code> method returns an <code>IndexedSeq</code> of all matching elements. If no elements match the query, <code>findAll</code>
 * returns an empty <code>IndexedSeq</code>. These methods allow you to perform rich queries using <code>for</code> expressions. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * val ele: Option[Element] = find("q")
 *
 * val eles: IndexedSeq[Element] = findAll(className("small"))
 * for (e <- eles; if e.tagName != "input")
 *   e should be ('displayed)
 * val textFields = eles filter { tf.isInstanceOf[TextField] }
 * </pre>
 *
 * <h2>Cleaning up</h2>
 * 
 * <p>
 * To close the current browser window, and exit the driver if the current window was the only one remaining, use <code>close</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * close()
 * </pre>
 * 
 * <p>
 * To close all windows, and exit the driver, use <code>quit</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * quit()
 * </pre>
 * 
 * <a name="alternateForms"/>
 * <h2>Alternate forms</h2>
 * 
 * <p>
 * Although statements like &ldquo;<code>delete all cookies</code>&rdquo; fit well with matcher statements
 * like &ldquo;<code>title should be ("Cheese!")</code>&rdquo;, they do not fit as well
 * with the simple method call form of assertions. If you prefer, you can avoid operator notation
 * and instead use alternatives that take the form of plain-old method calls. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * goTo("http://www.google.com")
 * clickOn("q")
 * textField("q").value = "Cheese!"
 * submit()
 * // Google's search is rendered dynamically with JavaScript.
 * eventually(assert(title === "Cheese! - Google Search"))
 * </pre>
 * 
 * <p>
 * Here's a table showing the complete list of alternatives:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>operator notation</strong></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><strong>method call</strong></th></tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>go to (host + "index.html")</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>goTo(host + "index.html")</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>click on "aButton"</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>clickOn("aButton")</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>switch to activeElement</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>switchTo(activeElement)</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>add cookie ("cookie_name", "cookie_value")</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>addCookie("cookie_name", "cookie_value")</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>delete cookie "cookie_name"</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>deleteCookie("cookie_name")</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>delete all cookies</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>deleteAllCookies()</code>
 * </td>
 * </tr>
 * <tr>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>capture to "MyScreenShot"</code>
 * </td>
 * <td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 * <code>captureTo("MyScreenShot")</code>
 * </td>
 * </tr>
 * </table>
 * 
 * @author Chua Chee Seng
 * @author Bill Venners
 */
trait WebBrowser { 

  /**
   * A point containing an XY screen location.
   */
  case class Point(x: Int, y: Int)

  /**
   * A dimension containing the width and height of a screen element.
   */
  case class Dimension(width: Int, height: Int)

  /**
   * Wrapper class for a Selenium <code>WebElement</code>.
   *
   * <p>
   * This class provides idiomatic Scala access to the services of an underlying <code>WebElement</code>.
   * You can access the wrapped <code>WebElement</code> via the <code>underlying</code> method.
   * </p>
   */
  sealed trait Element {

    /**
     * The XY location of the top-left corner of this <code>Element</code>.
     *
     * <p>
     * This invokes <code>getLocation</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return the location of the top-left corner of this element on the page
     */
    def location: Point = Point(underlying.getLocation.getX, underlying.getLocation.getY)

    /**
     * The width/height size of this <code>Element</code>.
     *
     * <p>
     * This invokes <code>getSize</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return the size of the element on the page
     */
    def size: Dimension = Dimension(underlying.getSize.getWidth, underlying.getSize.getHeight)

    /**
     * Indicates whether this <code>Element</code> is displayed.
     *
     * <p>
     * This invokes <code>isDisplayed</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return <code>true</code> if the element is currently displayed
     */
    def isDisplayed: Boolean = underlying.isDisplayed

    /**
     * Indicates whether this <code>Element</code> is enabled.
     *
     * <p>
     * This invokes <code>isEnabled</code> on the underlying <code>WebElement</code>, which
     * will generally return <code>true</code> for everything but disabled input elements.
     * </p>
     *
     * @return <code>true</code> if the element is currently enabled
     */
    def isEnabled: Boolean = underlying.isEnabled

    /**
     * Indicates whether this <code>Element</code> is selected.
     *
     * <p>
     * This method, which invokes <code>isSelected</code> on the underlying <code>WebElement</code>,
     * is relevant only for input elements such as checkboxes, options in a single- or multiple-selection
     * list box, and radio buttons. For any other element it will simply return <code>false</code>.
     * </p>
     *
     * @return <code>true</code> if the element is currently selected or checked
     */
    def isSelected: Boolean = underlying.isSelected

    /**
     * The tag name of this element.
     *
     * <p>
     * This method invokes <code>getTagName</code> on the underlying <code>WebElement</code>.
     * Note it returns the name of the tag, not the value of the of the <code>name</code> attribute.
     * For example, it will return will return <code>"input"</code> for the element
     * <code>&lt;input name="city" /&gt;</code>, not <code>"city"</code>.
     * </p>
     *
     * @return the tag name of this element
     */
    def tagName: String = underlying.getTagName

    /**
     * The underlying <code>WebElement</code> wrapped by this <code>Element</code>
     */
    val underlying: WebElement
    
    /**
     * The attribute value of the given attribute name of this element, wrapped in a <code>Some</code>, or <code>None</code> if no
     * such attribute exists on this <code>Element</code>.
     *
     * <p>
     * This method invokes <code>getAttribute</code> on the underlying <code>WebElement</code>, passing in the
     * specified <code>name</code>.
     * </p>
     *
     * @return the attribute with the given name, wrapped in a <code>Some</code>, else <code>None</code>
     */
    def attribute(name: String): Option[String] = Option(underlying.getAttribute(name))

    /**
     * Returns the visible (<em>i.e.</em>, not hidden by CSS) text of this element, including sub-elements, without any leading or trailing whitespace.
     *
     * @return the visible text enclosed by this element, or an empty string, if the element encloses no visible text
     */
    def text: String = {
      val txt = underlying.getText
      if (txt != null) txt else "" // Just in case, I'm not sure if Selenium would ever return null here
    }

    /**
     * Returns the result of invoking <code>equals</code> on the underlying <code>Element</code>, passing
     * in the specified <code>other</code> object.
     *
     * @param other the object with which to compare for equality
     *
     * @return true if the passed object is equal to this one
     */
    override def equals(other: Any): Boolean = underlying.equals(other)

    /**
     * Returns the result of invoking <code>hashCode</code> on the underlying <code>Element</code>.
     *
     * @return a hash code for this object
     */
    override def hashCode: Int = underlying.hashCode

    /**
     * Returns the result of invoking <code>toString</code> on the underlying <code>Element</code>.
     *
     * @return a string representation of this object
     */
    override def toString: String = underlying.toString 
  }

  /**
   * Trait that facilitates using the <em>page object pattern</em> with the ScalaTest Selenium DSL.
   *
   * <p>
   * If you use the page object pattern, mixing trait <code>Page</code> into your page classes will allow you to use the <code>go to</code>
   * syntax with your page objects. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * class HomePage extends Page {
   *   val url = "localhost:9000/index.html"
   * }
   *
   * val homePage = new HomePage
   * go to homePage
   * </pre>
   */
  trait Page {
    /**
     * The URL of the page represented by this page object.
     */
    val url: String
  }

  // fluentLinium has a doubleClick. Wonder how they are doing that?

  /**
   * Wrapper class for a Selenium <code>Cookie</code>.
   *
   * <p>
   * This class provides idiomatic Scala access to the services of an underlying <code>Cookie</code>.
   * You can access the wrapped <code>Cookie</code> via the <code>underlying</code> method.
   * </p>
   */
  final class WrappedCookie(val underlying: Cookie) {

    /**
     * The domain to which this cookie is visible.
     *
     * <p>
     * This invokes <code>getDomain</code> on the underlying <code>Cookie</code>.
     * </p>
     *
     * @return the domain of this cookie
     */
    def domain: String = underlying.getDomain 

    /**
     * The expire date of this cookie.
     *
     * <p>
     * This invokes <code>getExpiry</code> on the underlying <code>Cookie</code>.
     * </p>
     *
     * @return the expire date of this cookie
     */
    def expiry: Option[Date] = Option(underlying.getExpiry)

    /**
     * The name of this cookie.
     *
     * <p>
     * This invokes <code>getName</code> on the underlying <code>Cookie</code>.
     * </p>
     *
     * @return the name of this cookie
     */
    def name: String = underlying.getName

    /**
     * The path of this cookie.
     *
     * <p>
     * This invokes <code>getPath</code> on the underlying <code>Cookie</code>.
     * </p>
     *
     * @return the path of this cookie
     */
    def path: String = underlying.getPath 

    /**
     * The value of this cookie.
     *
     * <p>
     * This invokes <code>getValue</code> on the underlying <code>Cookie</code>.
     * </p>
     *
     * @return the value of this cookie
     */
    def value: String = underlying.getValue

    /**
     * Indicates whether the cookie requires a secure connection.
     *
     * <p>
     * This invokes <code>isSecure</code> on the underlying <code>Cookie</code>.
     * </p>
     *
     * @return true if this cookie requires a secure connection.
     */
    def secure: Boolean = underlying.isSecure

    /**
     * Returns the result of invoking <code>equals</code> on the underlying <code>Cookie</code>, passing
     * in the specified <code>other</code> object.
     *
     * <p>
     * Two Selenium <code>Cookie</code>s are considered equal if their name and values are equal.
     * </p>
     *
     * @param other the object with which to compare for equality
     *
     * @return true if the passed object is equal to this one
     */
    override def equals(other: Any): Boolean = underlying.equals(other)

    /**
     * Returns the result of invoking <code>hashCode</code> on the underlying <code>Cookie</code>.
     *
     * @return a hash code for this object
     */
    override def hashCode: Int = underlying.hashCode

    /**
     * Returns the result of invoking <code>toString</code> on the underlying <code>Cookie</code>.
     *
     * @return a string representation of this object
     */
    override def toString: String = underlying.toString 
  }

  /**
   * This class is part of the ScalaTest's Selenium DSL. Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a>
   * for an overview of the Selenium DSL.
   */
  class CookiesNoun

  /**
   * This field supports cookie deletion in ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This field enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * delete all cookies
   *            ^
   * </pre>
   */
  val cookies = new CookiesNoun
  
  /**
   * This sealed abstract class supports switching in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * One subclass of <code>SwitchTarget</code> exists for each kind of target that
   * can be switched to: active element, alert box, default content, frame (indentified by index,
   * name or id, or enclosed element), and window.
   * </p>
   */
  sealed abstract class SwitchTarget[T] {

    /**
     * Abstract method implemented by subclasses that represent "targets" to which the user can switch.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): T
  }

  /**
   * This class supports switching to the currently active element in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class is enables the following syntax:
   * </p>
   *
   * <pre>
   * switch to activeElement
   *           ^
   * </pre>
   */
  final class ActiveElementTarget extends SwitchTarget[Element] {

    /**
     * Switches the driver to the currently active element.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): Element = {
      createTypedElement(driver.switchTo.activeElement)
    }
  }

  /**
   * This class supports switching to the alert box in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class is enables the following syntax:
   * </p>
   *
   * <pre>
   * switch to alert
   *           ^
   * </pre>
   */
  final class AlertTarget extends SwitchTarget[Alert] {

    /**
     * Switches the driver to the currently active element.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): Alert = { 
      driver.switchTo.alert
    }
  }

  /**
   * This class supports switching to the default content in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class is enables the following syntax:
   * </p>
   *
   * <pre>
   * switch to defaultContent
   *           ^
   * </pre>
   */
  final class DefaultContentTarget extends SwitchTarget[WebDriver] {

    /**
     * Switches the driver to the default content
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): WebDriver = {
      driver.switchTo.defaultContent
    }
  }
 
  /**
   * This class supports switching to a frame by index in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class is enables the following syntax:
   * </p>
   *
   * <pre>
   * switch to frame(0)
   *           ^
   * </pre>
   */
  final class FrameIndexTarget(index: Int) extends SwitchTarget[WebDriver] {

    /**
     * Switches the driver to the frame at the index that was passed to the constructor.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): WebDriver = 
      try {
        driver.switchTo.frame(index)
      }
      catch {
        case e: org.openqa.selenium.NoSuchFrameException => 
          throw new TestFailedException(
                     sde => Some("Frame at index '" + index + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "switch", 1)
                   )
      }
  }

  /**
   * This class supports switching to a frame by name or ID in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class is enables the following syntax:
   * </p>
   *
   * <pre>
   * switch to frame("name")
   *           ^
   * </pre>
   */
  final class FrameNameOrIdTarget(nameOrId: String) extends SwitchTarget[WebDriver] {

    /**
     * Switches the driver to the frame with the name or ID that was passed to the constructor.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): WebDriver = 
      try {
        driver.switchTo.frame(nameOrId)
      }
      catch {
        case e: org.openqa.selenium.NoSuchFrameException => 
          throw new TestFailedException(
                     sde => Some("Frame with name or ID '" + nameOrId + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "switch", 1)
                   )
      }
  }

  /**
   * This class supports switching to a frame by web element in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   */
  final class FrameWebElementTarget(webElement: WebElement) extends SwitchTarget[WebDriver] {

    /**
     * Switches the driver to the frame containing the <code>WebElement</code> that was passed to the constructor.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): WebDriver = 
      try {
        driver.switchTo.frame(webElement)
      }
      catch {
        case e: org.openqa.selenium.NoSuchFrameException => 
          throw new TestFailedException(
                     sde => Some("Frame element '" + webElement + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "switch", 1)
                   )
      }
  }
  
  /**
   * This class supports switching to a frame by element in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   */
  final class FrameElementTarget(element: Element) extends SwitchTarget[WebDriver] {

    /**
     * Switches the driver to the frame containing the <code>Element</code> that was passed to the constructor.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): WebDriver = 
      try {
        driver.switchTo.frame(element.underlying)
      }
      catch {
        case e: org.openqa.selenium.NoSuchFrameException => 
          throw new TestFailedException(
                     sde => Some("Frame element '" + element + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "switch", 1)
                   )
      }
  }

  /**
   * This class supports switching to a window by name or handle in ScalaTest's Selenium DSL.
   * Please see the documentation for <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class is enables the following syntax:
   * </p>
   *
   * <pre>
   * switch to window(windowHandle)
   *           ^
   * </pre>
   */
  final class WindowTarget(nameOrHandle: String) extends SwitchTarget[WebDriver] {

    /**
     * Switches the driver to the window with the name or ID that was passed to the constructor.
     *
     * @param driver the <code>WebDriver</code> with which to perform the switch
     */
    def switch(driver: WebDriver): WebDriver =
      try {
        driver.switchTo.window(nameOrHandle)
      }
      catch {
        case e: org.openqa.selenium.NoSuchWindowException => 
          throw new TestFailedException(
                     sde => Some("Window with nameOrHandle '" + nameOrHandle + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "switch", 1)
                   )
      }
  }
  
  private def isTextField(webElement: WebElement): Boolean = 
    webElement.getTagName.toLowerCase == "input" && webElement.getAttribute("type").toLowerCase == "text"

  private def isTextArea(webElement: WebElement): Boolean = 
    webElement.getTagName.toLowerCase == "textarea"
  
  private def isCheckBox(webElement: WebElement): Boolean = 
    webElement.getTagName.toLowerCase == "input" && webElement.getAttribute("type").toLowerCase == "checkbox"
      
  private def isRadioButton(webElement: WebElement): Boolean = 
    webElement.getTagName == "input" && webElement.getAttribute("type") == "radio"
      
  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * textField("q").value should be ("Cheese!")
   * </pre>
   *
   * @param underlying the <code>WebElement</code> representing a text field
   * @throws TestFailedExeption if the passed <code>WebElement</code> does not represent a text field
   */
  final class TextField(val underlying: WebElement) extends Element {
    
    if(!isTextField(underlying))
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not text field."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    
    /**
     * Gets this text field's value.
     *
     * <p>
     * This method invokes <code>getAttribute("value")</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return the text field's value
     */
    def value: String = underlying.getAttribute("value")  
    
    /**
     * Sets this text field's value.
     *
     * @param value the new value
     */
    def value_=(value: String) {
      underlying.clear()
      underlying.sendKeys(value)
    }

    /**
     * Clears this text field.
     */
    def clear() { underlying.clear() }
  }

  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * textArea("q").value should be ("Cheese!")
   * </pre>
   *
   * @param underlying the <code>WebElement</code> representing a text area
   * @throws TestFailedExeption if the passed <code>WebElement</code> does not represent a text area
   */
  final class TextArea(val underlying: WebElement) extends Element {
    if(!isTextArea(underlying))
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not text area."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    
    /**
     * Gets this text area's value.
     *
     * <p>
     * This method invokes <code>getAttribute("value")</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return the text area's value
     */
    def value: String = underlying.getAttribute("value")

    /**
     * Sets this text area's value.
     *
     * @param value the new value
     */
    def value_=(value: String) {
      underlying.clear()
      underlying.sendKeys(value)
    }

    /**
     * Clears this text area.
     */
    def clear() { underlying.clear() }
  }
  
  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * radioButton(id("opt1")).value should be ("Option 1!")
   * </pre>
   *
   * @param underlying the <code>WebElement</code> representing a text area
   * @throws TestFailedExeption if the passed <code>WebElement</code> does not represent a text area
   */
  final class RadioButton(val underlying: WebElement) extends Element {
    if(!isRadioButton(underlying))
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not radio button."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    /**
     * Gets this radio button's value.
     *
     * <p>
     * Invokes <code>getAttribute("value")</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return the radio button's value
     */
    def value: String = underlying.getAttribute("value")
  }

  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * radioButtonGroup("group1").value should be ("Option 2")
   * </pre>
   *
   * @throws TestFailedExeption if no radio button with the passed <code>groupName</code> are found
   */
  final class RadioButtonGroup(groupName: String, driver: WebDriver) {

    private def groupElements = driver.findElements(By.name(groupName)).asScala.toList.filter(isRadioButton(_))

    if (groupElements.length == 0)
      throw new TestFailedException(
                     sde => Some("No radio buttons with group name '" + groupName + "' was found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )

    /**
     * Returns the value of this group's selected radio button, or throws <code>TestFailedException</code> if no
     * radio button in this group is selected.
     *
     * @return the value of this group's selected radio button
     * @throws TestFailedExeption if no radio button in this group is selected
     */
    def value: String = selection match {
      case Some(v) => v
      case None => 
        throw new TestFailedException(
                     sde => Some("The radio button group on which value was invoked contained no selected radio button."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "value", 1)
                   )
    }

    /**
     * Returns the value of this group's selected radio button, wrapped in a <code>Some</code>, or <code>None</code>, if no
     * radio button in this group is selected.
     *
     * @return the value of this group's selected radio button, wrapped in a <code>Some</code>, else <code>None</code>
     */
    def selection: Option[String] = {
      groupElements.find(_.isSelected) match {
        case Some(radio) => 
          Some(radio.getAttribute("value"))
        case None =>
          None
      }
    }

    /**
     * Selects the radio button with the passed value.
     *
     * @param the value of the radio button to select
     * @throws TestFailedExeption if the passed string is not the value of any radio button in this group
     */
    def value_=(value: String) {
      groupElements.find(_.getAttribute("value") == value) match {
        case Some(radio) => 
          radio.click()
        case None => 
          throw new TestFailedException(
                     sde => Some("Radio button value '" + value + "' not found for group '" + groupName + "'."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "value_=", 1)
                   )
      }
    }
  }

  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * checkbox("cbx1").select()
   * </pre>
   *
   * @param underlying the <code>WebElement</code> representing a checkbox
   * @throws TestFailedExeption if the passed <code>WebElement</code> does not represent a checkbox
   */
  final class Checkbox(val underlying: WebElement) extends Element {
    if(!isCheckBox(underlying))
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not check box."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    
    /**
     * Selects this checkbox.
     */
    def select() {
      if (!underlying.isSelected)
        underlying.click()
    }

    /**
     * Clears this checkbox
     */
    def clear() {
      if (underlying.isSelected())
        underlying.click()
    }

    /**
     * Gets this checkbox's value.
     *
     * <p>
     * This method invokes <code>getAttribute("value")</code> on the underlying <code>WebElement</code>.
     * </p>
     *
     * @return the checkbox's value
     */
    def value: String = underlying.getAttribute("value")
  }

  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * multiSel("select2").values += "option5"
   *                            ^
   * </pre>
   *
   * <p>
   * Instances of this class are returned from the <code>values</code> method of <code>MultiSel</code>.
   * <code>MultiSelOptionSeq</code> is an <code>IndexedSeq[String]</code> that wraps an underlying <code>IndexedSeq[String]</code> and adds two
   * methods, <code>+</code> and <code>-</code>, to facilitate the <code>+=</code> syntax for setting additional options
   * of the <code>MultiSel</code>. The Scala compiler will rewrite:
   * </p>
   *
   * <pre class="stHighlight">
   * multiSel("select2").values += "option5"
   * </pre>
   *
   * <p>
   * To:
   * </p>
   *
   * <pre class="stHighlight">
   * multiSel("select2").values = multiSel("select2").values + "option5"
   * </pre>
   *
   * <p>
   * Thus, first a new <code>MultiSelOptionSeq</code> is created by invoking the <code>+</code> method on the <code>MultiSelOptionSeq</code>
   * returned by <code>values</code>, and that result is passed to the <code>values_=</code> method.
   * </p>
   *
   * <p>
   * For symmetry, this class also offers a <code>-</code> method, which can be used to deselect an option, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * multiSel("select2").values -= "option5"
   *                            ^
   * </pre>
   *
   */
  class MultiSelOptionSeq(underlying: IndexedSeq[String]) extends IndexedSeq[String] {

    /**
     * Selects an element by its index in the sequence.
     *
     * <p>
     * This method invokes <code>apply</code> on the underlying <code>IndexedSeq[String]</code>, passing in <code>idx</code>, and returns the result.
     * </p>
     *
     * @param idx the index to select
     * @return the element of this sequence at index <code>idx</code>, where 0 indicates the first element
     */
    def apply(idx: Int): String = underlying.apply(idx)

    /**
     * The length of this sequence.
     *
     * <p>
     * This method invokes <code>length</code> on the underlying <code>IndexedSeq[String]</code> and returns the result.
     * </p>
     *
     * @return the number of elements in this sequence
     */
    def length: Int = underlying.length

    /**
     * Appends a string element to this sequence, if it doesn't already exist in the sequence.
     *
     * <p>
     * If the string element already exists in this sequence, this method returns itself. If not,
     * this method returns a new <code>MultiSelOptionSeq</code> with the passed value appended to the
     * end of the original <code>MultiSelOptionSeq</code>.
     * </p>
     *
     * @param the string element to append to this sequence
     * @return a <code>MultiSelOptionSeq</code> that contains the passed string value
     */
    def +(value: String): MultiSelOptionSeq = {
      if (!underlying.contains(value))
        new MultiSelOptionSeq(underlying :+ value)
      else
        this
    }

    /**
     * Removes a string element to this sequence, if it already exists in the sequence.
     *
     * <p>
     * If the string element does not already exist in this sequence, this method returns itself. If the element
     * is contained in this sequence, this method returns a new <code>MultiSelOptionSeq</code> with the passed value
     * removed from the the original <code>MultiSelOptionSeq</code>, leaving any other elements in the same order.
     * </p>
     *
     * @param the string element to append to this sequence
     * @return a <code>MultiSelOptionSeq</code> that contains the passed string value
     */
    def -(value: String): MultiSelOptionSeq = {
      if (underlying.contains(value))
        new MultiSelOptionSeq(underlying.filter(_ != value))
      else
        this
    }
  }

  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * singleSel.clear()
   * </pre>
   *
   * @param underlying a <code>WebElement</code> representing a single selection list
   * @throws TestFailedExeption if the passed <code>WebElement</code> does not represent a single selection list
   */
  class SingleSel(val underlying: WebElement) extends Element {
    if(underlying.getTagName.toLowerCase != "select")
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not select."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    private val select = new Select(underlying)
    if (select.isMultiple)
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not a single-selection list."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    
    /**
     * Returns the value of this single selection list, wrapped in a <code>Some</code>, or <code>None</code>, if this single
     * selection list has no currently selected value.
     *
     * @return the value of this single selection list, wrapped in a <code>Some</code>, else <code>None</code>
     */
    def selection = {
      val first = select.getFirstSelectedOption
      if (first == null)
        None
      else
        Some(first.getAttribute("value"))
    }
    
    /**
     * Gets this single selection list's selected value, or throws <code>TestFailedException</code> if no value is currently selected.
     *
     * @return the single selection list's value
     * @throws TestFailedException if the single selection list has no selected value
     */
    def value: String = selection match {
      case Some(v) => v
      case None => 
        throw new TestFailedException(
                     sde => Some("The single selection list on which value was invoked had no selection."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "value", 1)
                   )
    }
    
    /**
     * Sets this single selection list's value to the passed value.
     *
     * @param value the new value
     * @throws TestFailedException if the passed value does not match not one of the single selection list's values
     */
    def value_=(value : String) {
      try {
        select.selectByValue(value)
      }
      catch {
        case e: org.openqa.selenium.NoSuchElementException => 
          throw new TestFailedException(
                     sde => Some(e.getMessage),
                     Some(e),
                     getStackDepthFun("WebBrowser.scala", "value_=", 1)
                   )
      }
    }
  }

  

  /**
   * This class is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * multiSel("select2").clear("option5")
   * </pre>
   *
   * @param underlying a <code>WebElement</code> representing a multiple selection list
   * @throws TestFailedExeption if the passed <code>WebElement</code> does not represent a multiple selection list
   */
  class MultiSel(val underlying: WebElement) extends Element {
    if(underlying.getTagName.toLowerCase != "select")
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not select."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )
    private val select = new Select(underlying)
    if (!select.isMultiple)
      throw new TestFailedException(
                     sde => Some("Element " + underlying + " is not a multi-selection list."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "this", 1)
                   )

    /**
     * Clears the passed value in this multiple selection list.
     *
     * @param value the value to clear
     */
    def clear(value: String) {
      select.deselectByValue(value)
    }
  
    /**
     * Gets all selected values of this multiple selection list.
     *
     * <p>
     * If the multiple selection list has no selections, ths method will
     * return an empty <code>IndexedSeq</code>.
     * </p>
     *
     * @return An <code>IndexedSeq</code> containing the currently selected values
     */
    def values: MultiSelOptionSeq = {
      val elementSeq = Vector.empty ++ select.getAllSelectedOptions.asScala
      new MultiSelOptionSeq(elementSeq.map(_.getAttribute("value")))
    }

    /**
     * Clears any existing selections then sets all values contained in the passed <code>Seq[String]</code>.
     *
     * <p>
     * In other words, the <code>values_=</code> method <em>replaces</em> the current selections, if any, with
     * new selections defined by the passed <code>Seq[String]</code>.
     * </p>
     *
     * @param values a <code>Seq</code> of string values to select
     * @throws TestFailedException if a value contained in the passed <code>Seq[String]</code> is not
     *         among this multiple selection list's values.
     */
    def values_=(values: Seq[String]) {
      try {
        clearAll()
        values.foreach(select.selectByValue(_))
      }
      catch {
        case e: org.openqa.selenium.NoSuchElementException => 
          throw new TestFailedException(
                     sde => Some(e.getMessage),
                     Some(e),
                     getStackDepthFun("WebBrowser.scala", "value_=", 1)
                   )
      }
    }
    
    /**
     * Clears all selected values in this multiple selection list.
     *
     * @param value the value to clear
     */
    def clearAll() {
      select.deselectAll()
    }
  }
  
  /**
   * This object is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * This object enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * go to "http://www.artima.com"
   * ^
   * </pre>
   */
  object go {

    /**
     * Sends the browser to the passed URL.
     *
     * <p>
     * This method enables syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * go to "http://www.artima.com"
     *    ^
     * </pre>
     *
     * @param url the URL to which to send the browser
     * @param driver the <code>WebDriver</code> with which to drive the browser
     */
    def to(url: String)(implicit driver: WebDriver) {
      driver.get(url)
    }

    /**
     * Sends the browser to the URL contained in the passed <code>Page</code> object.
     *
     * <p>
     * This method enables syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * go to homePage
     *    ^
     * </pre>
     *
     * @param page the <code>Page</code> object containing the URL to which to send the browser
     * @param driver the <code>WebDriver</code> with which to drive the browser
     */
    def to(page: Page)(implicit driver: WebDriver) {
      driver.get(page.url)
    }
  }
  
  /**
   * Sends the browser to the passed URL.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * goTo("http://www.artima.com")
   * </pre>
   *
   * @param url the URL to which to send the browser
   * @param driver the <code>WebDriver</code> with which to drive the browser
   */
  def goTo(url: String)(implicit driver: WebDriver) {
    go to url
  }
  
  /**
   * Sends the browser to the URL contained in the passed <code>Page</code> object.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * goTo(homePage)
   * </pre>
   *
   * @param page the <code>Page</code> object containing the URL to which to send the browser
   * @param driver the <code>WebDriver</code> with which to drive the browser
   */
  def goTo(page: Page)(implicit driver: WebDriver) {
    go to page
  }
  
  /**
   * Closes the current browser window, and exits the driver if the current window was the only one remaining.
   *
   * @param driver the <code>WebDriver</code> with which to drive the browser
   */
  def close()(implicit driver: WebDriver) {
    driver.close()
  }
  
  /**
   * Returns the title of the current page, or the empty string if the current page has no title.
   *
   * @param driver the <code>WebDriver</code> with which to drive the browser
   * @returns the current page's title, or the empty string if the current page has no title
   */
  def title(implicit driver: WebDriver): String = {
    val t = driver.getTitle
    if (t != null) t else ""
  }
  
  /**
   * Returns the source of the current page.
   *
   * <p>
   * This method invokes <code>getPageSource</code> on the passed <code>WebDriver</code> and returns the result.
   * </p>
   *
   * @param driver the <code>WebDriver</code> with which to drive the browser
   * @returns the source of the current page
   */
  def pageSource(implicit driver: WebDriver): String = driver.getPageSource
  
  /**
   * Returns the URL of the current page.
   *
   * <p>
   * This method invokes <code>getCurrentUrl</code> on the passed <code>WebDriver</code> and returns the result.
   * </p>
   *
   * @param driver the <code>WebDriver</code> with which to drive the browser
   * @returns the URL of the current page
   */
  def currentUrl(implicit driver: WebDriver): String = driver.getCurrentUrl
  
  /**
   * This trait is part of ScalaTest's Selenium DSL. Please see the documentation for
   * <a href="WebBrowser.html"><code>WebBrowser</code></a> for an overview of the Selenium DSL.
   *
   * <p>
   * Subclasses of this trait define different ways of querying for elements, enabling
   * syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on id("q")
   *          ^
   * </pre>
   */
  sealed trait Query {

    /**
     * The Selenium <code>By</code> for this query.
     */
    val by: By

    /**
     * The query string for this query.
     *
     * <p>
     * For example, the query string for <code>id("q")</code> is <code>"q"</code>.
     * </p>
     */
    val queryString: String

    /**
     * Returns the first <code>Element</code> selected by this query, or throws <code>TestFailedException</code>
     * if no <code>Element</code> is selected.
     *
     * <p>
     * The class of the <code>Element</code> returned will be a subtype of <code>Element</code> if appropriate.
     * For example, if this query selects a text field, the class of the returned <code>Element</code> will
     * be <code>TextField</code>.
     * </p>
     *
     * @param driver the <code>WebDriver</code> with which to drive the browser
     * @returns the <code>Element</code> selected by this query
     * @throws TestFailedException if nothing is selected by this query
     */
    def element(implicit driver: WebDriver): Element = {
      try {
        createTypedElement(driver.findElement(by))
      }
      catch {
        case e: org.openqa.selenium.NoSuchElementException => 
          throw new TestFailedException(
                     sde => Some("Element '" + queryString + "' not found."),
                     Some(e),
                     getStackDepthFun("WebBrowser.scala", "name", 1)
                   )
      }
    }
    
    /**
     * Returns the first <code>Element</code> selected by this query, wrapped in a <code>Some</code>, or <code>None</code>
     * if no <code>Element</code> is selected.
     *
     * <p>
     * The class of the <code>Element</code> returned will be a subtype of <code>Element</code> if appropriate.
     * For example, if this query selects a text field, the class of the returned <code>Element</code> will
     * be <code>TextField</code>.
     * </p>
     *
     * @param driver the <code>WebDriver</code> with which to drive the browser
     * @returns the <code>Element</code> selected by this query, wrapped in a <code>Some</code>, or <code>None</code> if
     *   no <code>Element</code> is selected
     */
    def findElement(implicit driver: WebDriver): Option[Element] = 
      try {
        Some(createTypedElement(driver.findElement(by)))
      }
      catch {
        case e: org.openqa.selenium.NoSuchElementException => None
      }

    /**
     * Returns an <code>Iterator</code> over all <code>Element</code>s selected by this query.
     *
     * <p>
     * The class of the <code>Element</code>s produced by the returned <code>Iterator</code> will be a
     * subtypes of <code>Element</code> if appropriate.  For example, if an <code>Element</code>representing
     * a text field is returned by the <code>Iterator</code>, the class of the returned <code>Element</code> will
     * be <code>TextField</code>.
     * </p>
     *
     * <p>
     * If no <code>Elements</code> are selected by this query, this method will return an empty <code>Iterator</code> will be returned.
     * <p>
     *
     * @param driver the <code>WebDriver</code> with which to drive the browser
     * @returns the <code>Element</code> selected by this query, wrapped in a <code>Some</code>, or <code>None</code> if
     *   no <code>Element</code> is selected
     */
    def findAllElements(implicit driver: WebDriver): Iterator[Element] = driver.findElements(by).asScala.toIterator.map { e => createTypedElement(e) }
    
    /**
     * Returns the first <code>WebElement</code> selected by this query, or throws <code>TestFailedException</code>
     * if no <code>WebElement</code> is selected.
     *
     * @param driver the <code>WebDriver</code> with which to drive the browser
     * @returns the <code>WebElement</code> selected by this query
     * @throws TestFailedException if nothing is selected by this query
     */
    def webElement(implicit driver: WebDriver): WebElement = {
      try {
        driver.findElement(by)
      }
      catch {
        case e: org.openqa.selenium.NoSuchElementException => 
          throw new TestFailedException(
                     sde => Some("WebElement '" + queryString + "' not found."),
                     Some(e),
                     getStackDepthFun("WebBrowser.scala", "name", 1)
                   )
      }
    }
  }

  /**
   * An ID query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on id("q")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class IdQuery(queryString: String) extends Query { val by = By.id(queryString)}

  /**
   * A name query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on name("q")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class NameQuery(queryString: String) extends Query { val by = By.name(queryString) }

  /**
   * An XPath query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on xpath("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class XPathQuery(queryString: String) extends Query { val by = By.xpath(queryString) }

// TODO: Are these case classes just to get at the val?
  /**
   * A class name query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on className("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class ClassNameQuery(queryString: String) extends Query { val by = By.className(queryString) }

  /**
   * A CSS selector query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on cssSelector("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class CssSelectorQuery(queryString: String) extends Query { val by = By.cssSelector(queryString) }

  /**
   * A link text query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on linkText("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class LinkTextQuery(queryString: String) extends Query { val by = By.linkText(queryString) }

  /**
   * A partial link text query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on partialLinkText("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class PartialLinkTextQuery(queryString: String) extends Query { val by = By.partialLinkText(queryString) }

  /**
   * A tag name query.
   *
   * <p>
   * This class enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on tagName("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  case class TagNameQuery(queryString: String) extends Query { val by = By.tagName(queryString) }
  
  /**
   * Returns an ID query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on id("q")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def id(elementId: String): IdQuery = new IdQuery(elementId)

  /**
   * Returns a name query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on name("q")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def name(elementName: String): NameQuery = new NameQuery(elementName)

  /**
   * Returns an XPath query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on xpath("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def xpath(xpath: String): XPathQuery = new XPathQuery(xpath)

  /**
   * Returns a class name query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on className("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def className(className: String): ClassNameQuery = new ClassNameQuery(className)

  /**
   * Returns a CSS selector query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on cssSelector("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def cssSelector(cssSelector: String): CssSelectorQuery = new CssSelectorQuery(cssSelector)

  /**
   * Returns a link text query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on linkText("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def linkText(linkText: String): LinkTextQuery = new LinkTextQuery(linkText)

  /**
   * Returns a partial link text query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on partialLinkText("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def partialLinkText(partialLinkText: String): PartialLinkTextQuery = new PartialLinkTextQuery(partialLinkText)

  /**
   * Returns a tag name query.
   *
   * <p>
   * This method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * click on tagName("???")
   *          ^
   * </pre>
   *
   * @param queryString the query string for this query.
   */
  def tagName(tagName: String): TagNameQuery = new TagNameQuery(tagName)

  private def createTypedElement(element: WebElement): Element = {
    if (isTextField(element))
      new TextField(element)
    else if (isTextArea(element))
      new TextArea(element)
    else if (isCheckBox(element))
      new Checkbox(element)
    else if (isRadioButton(element))
      new RadioButton(element)
    else if (element.getTagName.toLowerCase == "select") {
      val select = new Select(element)
      if (select.isMultiple)
        new MultiSel(element)
      else
        new SingleSel(element)
    }
    else
      new Element() { val underlying = element }
  }
  
// XXX
  /**
   * Finds and returns the first element selected by the specified <code>Query</code>, wrapped
   * in a <code>Some</code>, or <code>None</code> if no element is selected.
   *
   * <p>
   * The class of the <code>Element</code> returned will be a subtype of <code>Element</code> if appropriate.
   * For example, if the query selects a text field, the class of the returned <code>Element</code> will
   * be <code>TextField</code>.
   * </p>
   *
   * @param query the <code>Query</code> with which to search
   * @param driver the <code>WebDriver</code> with which to drive the browser
   * @returns the <code>Element</code> selected by this query, wrapped in a <code>Some</code>, or <code>None</code> if
   *   no <code>Element</code> is selected
   */
  def find(query: Query)(implicit driver: WebDriver): Option[Element] = query.findElement

  /**
   * Finds and returns the first element selected by the specified string ID or name, wrapped
   * in a <code>Some</code>, or <code>None</code> if no element is selected. YYY
   *
   * <p>
   * This method will try to lookup by id first. If it cannot find 
   * any element with an id equal to the specified <code>queryString</code>, it will then try lookup by name.
   * </p>
   *
   * <p>
   * The class of the <code>Element</code> returned will be a subtype of <code>Element</code> if appropriate.
   * For example, if the query selects a text field, the class of the returned <code>Element</code> will
   * be <code>TextField</code>.
   * </p>
   *
   * @param queryString the string with which to search, first by ID then by name
   * @param driver the <code>WebDriver</code> with which to drive the browser
   * @returns the <code>Element</code> selected by this query, wrapped in a <code>Some</code>, or <code>None</code> if
   *   no <code>Element</code> is selected
   */
  def find(queryString: String)(implicit driver: WebDriver): Option[Element] = 
    new IdQuery(queryString).findElement match {
      case Some(element) => Some(element)
      case None => new NameQuery(queryString).findElement match {
        case Some(element) => Some(element)
        case None => None
      }
    }

  def findAll(query: Query)(implicit driver: WebDriver): Iterator[Element] = query.findAllElements

  def findAll(queryString: String)(implicit driver: WebDriver): Iterator[Element] = {
    val byIdItr = new IdQuery(queryString).findAllElements
    if (byIdItr.hasNext)
      byIdItr
    else 
      new NameQuery(queryString).findAllElements
  }
  
  private def tryQueries[T](queryString: String)(f: Query => T)(implicit driver: WebDriver): T = {
    try {
      f(IdQuery(queryString))
    }
    catch {
      case _: Throwable => f(NameQuery(queryString))
    }
  }
  
  def textField(query: Query)(implicit driver: WebDriver): TextField = new TextField(query.webElement)
  
  def textField(queryString: String)(implicit driver: WebDriver): TextField = 
    tryQueries(queryString)(q => new TextField(q.webElement))
  
  def textArea(query: Query)(implicit driver: WebDriver) = new TextArea(query.webElement)
  
  def textArea(queryString: String)(implicit driver: WebDriver): TextArea = 
    tryQueries(queryString)(q => new TextArea(q.webElement))
  
  def radioButtonGroup(groupName: String)(implicit driver: WebDriver) = new RadioButtonGroup(groupName, driver)
  
  def radioButton(query: Query)(implicit driver: WebDriver) = new RadioButton(query.webElement)
  
  def radioButton(queryString: String)(implicit driver: WebDriver): RadioButton = 
    tryQueries(queryString)(q => new RadioButton(q.webElement))
  
  def checkbox(query: Query)(implicit driver: WebDriver) = new Checkbox(query.webElement)
  
  def checkbox(queryString: String)(implicit driver: WebDriver): Checkbox = 
    tryQueries(queryString)(q => new Checkbox(q.webElement))
  
  def singleSel(query: Query)(implicit driver: WebDriver) = new SingleSel(query.webElement)
  
  def singleSel(queryString: String)(implicit driver: WebDriver): SingleSel = 
    tryQueries(queryString)(q => new SingleSel(q.webElement))
  
  def multiSel(query: Query)(implicit driver: WebDriver) = new MultiSel(query.webElement)
  
  def multiSel(queryString: String)(implicit driver: WebDriver): MultiSel = 
    tryQueries(queryString)(q => new MultiSel(q.webElement))
    
  @deprecated("The button methods will be removed in the next 2.0 milestone release.")
  def button(webElement: WebElement): WebElement = webElement  // enable syntax 'click on aButton', where aButton is a WebElement.
  
  @deprecated("The button methods will be removed in the next 2.0 milestone release.")
  def button(queryString: String)(implicit driver: WebDriver): WebElement = 
    tryQueries(queryString)(q => q.webElement)
  
  object click {
    def on(element: WebElement) {
      element.click()
    }
    
    def on(query: Query)(implicit driver: WebDriver) {
      query.webElement.click()
    }
  
    def on(queryString: String)(implicit driver: WebDriver) {
      // stack depth is not correct if just call the button("...") directly.
      val target = tryQueries(queryString)(q => q.webElement)
      on(target)
    }
    
    def on(element: Element) {
      element.underlying.click()
    }
  }
  
  def clickOn(element: WebElement) {
    click on element
  }
  
  def clickOn(query: Query)(implicit driver: WebDriver) {
    click on query
  }
  
  def clickOn(queryString: String)(implicit driver: WebDriver) {
    click on queryString
  }
  
  def clickOn(element: Element) {
    click on element
  }
  
  def submit()(implicit driver: WebDriver) {
    (switch to activeElement).underlying.submit()
  }
  
  /**
   * Sets the amount of time the driver should wait when searching for an element that is not immediately present.
   *
   * <p>
   * When searching for requested elements, Selenium will poll the page until the requested element (or at least one of multiple requested
   * elements) is found or this "implicit wait" timeout has expired.
   * If the timeout expires, Selenium will throw <code>NoSuchElementException</code>, which ScalaTest's Selenium DSL will wrap in a <code>TestFailedException</code>.
   * </p>
   *
   * <p>
   * You can alternatively set this timeout to zero and use ScalaTest's <code>eventually</code> construct.
   * </p>
   *
   * <p>
   * This method invokes <code>manage.timeouts.implicitlyWait</code> on the passed <code>WebDriver</code>. See the documentation of Selenium's
   * <code>WebDriver#Timeouts</code> interface for more information.
   * </p>
   *
   * @param timeout the time span to implicitly wait
   * @param driver the <code>WebDriver</code> on which to set the implicit wait
   */
  def implicitlyWait(timeout: Span)(implicit driver: WebDriver) {
    driver.manage.timeouts.implicitlyWait(timeout.totalNanos, TimeUnit.NANOSECONDS)
  }
  
  def wait[T](timeout: Span, interval: Span = Span(500L, Milliseconds))(f: => T)(implicit driver: WebDriver) {
    new WebDriverWait(driver, timeout.totalNanos / 1000000000L, interval.totalNanos / 1000000)
      .until(new ExpectedCondition[T]() {
        override def apply(driver: WebDriver) = {
          f
        }
      })
  }
  
  def quit()(implicit driver: WebDriver) {
    driver.quit()
  }
  
  def windowHandle(implicit driver: WebDriver): String = driver.getWindowHandle
  def windowHandles(implicit driver: WebDriver): Set[String] = driver.getWindowHandles.asScala.toSet
  
  object switch {
    def to[T](target: SwitchTarget[T])(implicit driver: WebDriver): T = {
      target.switch(driver)
    }
  }
  val activeElement = new ActiveElementTarget()
  val alert = new AlertTarget()
  val defaultContent = new DefaultContentTarget()
  def frame(index: Int) = new FrameIndexTarget(index)
  def frame(nameOrId: String) = new FrameNameOrIdTarget(nameOrId)
  def frame(element: WebElement) = new FrameWebElementTarget(element)
  def frame(element: Element) = new FrameElementTarget(element)
  def frame(query: Query)(implicit driver: WebDriver) = new FrameWebElementTarget(query.webElement)
  def window(nameOrHandle: String) = new WindowTarget(nameOrHandle)
  
  def switchTo[T](target: SwitchTarget[T])(implicit driver: WebDriver): T = switch to target
  
  def goBack()(implicit driver: WebDriver) {
    driver.navigate.back()
  }
  
  def goForward()(implicit driver: WebDriver) {
    driver.navigate.forward()
  }
  
  def reloadPage()(implicit driver: WebDriver) {
    driver.navigate.refresh()
  }
  
  object add {
    private def addCookie(cookie: Cookie)(implicit driver: WebDriver) {
      driver.manage.addCookie(cookie)
    }
    
    // Default values determined from http://code.google.com/p/selenium/source/browse/trunk/java/client/src/org/openqa/selenium/Cookie.java
    def cookie(name: String, value: String, path: String = "/", expiry: Date = null, domain: String = null, secure: Boolean = false)(implicit driver: WebDriver) { 
      addCookie(new Cookie(name, value, domain, path, expiry, secure))
    }
  }
  
  def cookie(name: String)(implicit driver: WebDriver): WrappedCookie = {
    getCookie(name)
  }
  
  private def getCookie(name: String)(implicit driver: WebDriver): WrappedCookie = {
    driver.manage.getCookies.asScala.toList.find(_.getName == name) match {
      case Some(cookie) => 
        new WrappedCookie(cookie)
      case None =>
        throw new TestFailedException(
                     sde => Some("Cookie '" + name + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "getCookie", 1)
                   )
    }
  }
  
  object delete {
    private def deleteCookie(name: String)(implicit driver: WebDriver) {
      val cookie = getCookie(name)
      if (cookie == null) 
        throw new TestFailedException(
                     sde => Some("Cookie '" + name + "' not found."),
                     None,
                     getStackDepthFun("WebBrowser.scala", "deleteCookie", 1)
                   )
      driver.manage.deleteCookie(cookie.underlying)
    }
    
    def cookie(name: String)(implicit driver: WebDriver) {
      deleteCookie(name)
    }
    
    def all(cookies: CookiesNoun)(implicit driver: WebDriver) {
      driver.manage.deleteAllCookies()
    }
  }

  def addCookie(name: String, value: String, path: String = "/", expiry: Date = null, domain: String = null, secure: Boolean = false)(implicit driver: WebDriver) {
    add cookie (name, value, path, expiry, domain, secure)
  }
  
  def deleteCookie(name: String)(implicit driver: WebDriver) {
    delete cookie name
  }
  
  def deleteAllCookies()(implicit driver: WebDriver) {
    delete all cookies
  }
  
  def isScreenshotSupported(implicit driver: WebDriver): Boolean = driver.isInstanceOf[TakesScreenshot]
  
  object capture {
    
    private var targetDir = new File(System.getProperty("java.io.tmpdir"))
    
    @deprecated("The 'capture set <dir name>' form will be removed in the next 2.0 milestone. Please use setCaptureDir(<dir name>) instead.")
    def set(targetDirPath: String) {
      targetDir = 
        if (targetDirPath.endsWith(File.separator))
          new File(targetDirPath)
        else
          new File(targetDirPath + File.separator)
      if (!targetDir.exists)
        targetDir.mkdirs()
    }
    
    def to(fileName: String)(implicit driver: WebDriver) {
      driver match {
        case takesScreenshot: TakesScreenshot => 
          val tmpFile = takesScreenshot.getScreenshotAs(OutputType.FILE)
          val outFile = new File(targetDir, if (fileName.toLowerCase.endsWith(".png")) fileName else fileName + ".png")
          new FileOutputStream(outFile) getChannel() transferFrom(
          new FileInputStream(tmpFile) getChannel, 0, Long.MaxValue )
        case _ =>
          throw new UnsupportedOperationException("Screen capture is not support by " + driver.getClass.getName)
      }
    }
    
    def apply()(implicit driver: WebDriver): File = {
      driver match {
        case takesScreenshot: TakesScreenshot => 
          val tmpFile = takesScreenshot.getScreenshotAs(OutputType.FILE)
          val fileName = tmpFile.getName
          val outFile = new File(targetDir, if (fileName.toLowerCase.endsWith(".png")) fileName else fileName + ".png")
          new FileOutputStream(outFile) getChannel() transferFrom(
          new FileInputStream(tmpFile) getChannel, 0, Long.MaxValue )
          outFile
        case _ =>
          throw new UnsupportedOperationException("Screen capture is not support by " + driver.getClass.getName)
      }
    }
  }
  
  def captureTo(fileName: String)(implicit driver: WebDriver) {
    capture to fileName
  }
  
  def setCaptureDir(targetDirPath: String) {
    capture set targetDirPath
  }
  
  def withScreenshot(fun: => Unit)(implicit driver: WebDriver) {
    try {
      fun
    }
    catch {
      case e: org.scalatest.exceptions.ModifiableMessage[_] =>
        throw e.modifyMessage{ (currentMessage: Option[String]) => 
          val captureFile: File = capture.apply()
          currentMessage match {
            case Some(currentMsg) => 
              Some(currentMsg + "; screenshot captured in " + captureFile.getAbsolutePath)
            case None => 
              Some("screenshot captured in " + captureFile.getAbsolutePath)
          }
        }
    }
  }
  
  /**
   * Executes JavaScript in the context of the currently selected frame or window.
   */
  def executeScript(script: String, args: AnyRef*)(implicit driver: WebDriver): AnyRef =
    driver match {
      case executor: JavascriptExecutor => executor.executeScript(script, args.toArray : _*)
      case _ => throw new UnsupportedOperationException("Web driver " + driver.getClass.getName + " does not support javascript execution.")
    }
  
  /**
   * Executes an asynchronous piece of JavaScript in the context of the currently selected frame or window.
   */
  def executeAsyncScript(script: String, args: AnyRef*)(implicit driver: WebDriver): AnyRef =
    driver match {
      case executor: JavascriptExecutor => executor.executeAsyncScript(script, args.toArray : _*)
      case _ => throw new UnsupportedOperationException("Web driver " + driver.getClass.getName + " does not support javascript execution.")
    }
  
  /**
   * Sets the amount of time to wait for an asynchronous script to finish execution before throwing an exception.
   */
  def setScriptTimeout(timeout: Span)(implicit driver: WebDriver) {
    driver.manage().timeouts().setScriptTimeout(timeout.totalNanos, TimeUnit.NANOSECONDS);
  }
  
  private def getStackDepthFun(fileName: String, methodName: String, adjustment: Int = 0): (StackDepthException => Int) = { sde =>
    getStackDepth(sde.getStackTrace, fileName, methodName, adjustment)
  }
  
  private def getStackDepth(stackTrace: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0) = {
    val stackTraceList = stackTrace.toList

    val fileNameIsDesiredList: List[Boolean] =
      for (element <- stackTraceList) yield
        element.getFileName == fileName // such as "Checkers.scala"

    val methodNameIsDesiredList: List[Boolean] =
      for (element <- stackTraceList) yield
        element.getMethodName == methodName // such as "check"

    // For element 0, the previous file name was not desired, because there is no previous
    // one, so you start with false. For element 1, it depends on whether element 0 of the stack trace
    // had the desired file name, and so forth.
    val previousFileNameIsDesiredList: List[Boolean] = false :: (fileNameIsDesiredList.dropRight(1))

    // Zip these two related lists together. They now have two boolean values together, when both
    // are true, that's a stack trace element that should be included in the stack depth.
    val zipped1 = methodNameIsDesiredList zip previousFileNameIsDesiredList
    val methodNameAndPreviousFileNameAreDesiredList: List[Boolean] =
      for ((methodNameIsDesired, previousFileNameIsDesired) <- zipped1) yield
        methodNameIsDesired && previousFileNameIsDesired

    // Zip the two lists together, that when one or the other is true is an include.
    val zipped2 = fileNameIsDesiredList zip methodNameAndPreviousFileNameAreDesiredList
    val includeInStackDepthList: List[Boolean] =
      for ((fileNameIsDesired, methodNameAndPreviousFileNameAreDesired) <- zipped2) yield
        fileNameIsDesired || methodNameAndPreviousFileNameAreDesired

    val includeDepth = includeInStackDepthList.takeWhile(include => include).length
    val depth = if (includeDepth == 0 && stackTrace(0).getFileName != fileName && stackTrace(0).getMethodName != methodName) 
      stackTraceList.takeWhile(st => st.getFileName != fileName || st.getMethodName != methodName).length
    else
      includeDepth
    
    depth + adjustment
  }

  // Clears the text field or area, then presses the passed keys
  def enter(value: String)(implicit driver: WebDriver) {
    val ae = switch to activeElement
    ae match {
      case tf: TextField => tf.value = value
      case ta: TextArea => ta.value = value
      case _ => 
        throw new TestFailedException(
                     sde => Some("Currently selected element is neither a text field nor a text area"),
                     None,
                     getStackDepthFun("WebBrowser.scala", "switch", 1)
                   )
    }
  }

  def pressKeys(value: String)(implicit driver: WebDriver) {
    val ae: WebElement = driver.switchTo.activeElement
    ae.sendKeys(value)
  }
}

/**
 * Companion object that facilitates the importing of <code>WebBrowser</code> members as 
 * an alternative to mixing it in. One use case is to import <code>WebBrowser</code> members so you can use
 * them in the Scala interpreter.
 */
object WebBrowser extends WebBrowser

/**
 * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for HTMLUnit (an <code>org.openqa.selenium.htmlunit.HtmlUnitDriver</code>), with JavaScript
 * enabled by default.
 *
 * <p>
 * Note: You can disable JavaScript with:
 * </p>
 *
 * <pre>
 * webDriver.setJavascriptEnabled(false)
 * </pre>
 */
trait HtmlUnit extends WebBrowser with ScreenshotCapturer {

  /**
   * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for HTMLUnit (an <code>org.openqa.selenium.htmlunit.HtmlUnitDriver</code>), with JavaScript
   * enabled by default.
   *
   * <p>
   * Note: You can disable JavaScript with:
   * </p>
   *
   * <pre>
   * webDriver.setJavascriptEnabled(false)
   * </pre>
   */
  implicit val webDriver = new HtmlUnitDriver()

  webDriver.setJavascriptEnabled(true)

  /**
   * Captures a screenshot and saves it as a file in the specified directory.
   */
  def captureScreenshot(directory: String) {
    capture to directory
  }
}

/**
 * Companion object that facilitates the importing of <code>HtmlUnit</code> members as 
 * an alternative to mixing it in. One use case is to import <code>HtmlUnit</code> members so you can use
 * them in the Scala interpreter.
 */
object HtmlUnit extends HtmlUnit

/**
 * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Firefox (an <code>org.openqa.selenium.firefox.FirefoxDriver</code>).
 *
 * <p>
 * The <code>FirefoxDriver</code> uses the <code>FirefoxProfile</code> defined as <code>firefoxProfile</code>. By default this is just a <code>new FirefoxProfile</code>.
 * You can mutate this object to modify the profile, or override <code>firefoxProfile</code>.
 * </p>
 */
trait Firefox extends WebBrowser with ScreenshotCapturer {

  /**
   * The <code>FirefoxProfile</code> passed to the constructor of the <code>FirefoxDriver</code> returned by <code>webDriver</code>.
   *
   * <p>
   * The <code>FirefoxDriver</code> uses the <code>FirefoxProfile</code> defined as <code>firefoxProfile</code>. By default this is just a <code>new FirefoxProfile</code>.
   * You can mutate this object to modify the profile, or override <code>firefoxProfile</code>.
   * </p>
   */
  val firefoxProfile = new FirefoxProfile()

  /**
   * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Firefox (an <code>org.openqa.selenium.firefox.FirefoxDriver</code>), with a default
   * Firefox profile.
   *
   * <p>
   * The <code>FirefoxDriver</code> uses the <code>FirefoxProfile</code> defined as <code>firefoxProfile</code>. By default this is just a <code>new FirefoxProfile</code>.
   * You can mutate this object to modify the profile, or override <code>firefoxProfile</code>.
   * </p>
   */
  implicit val webDriver = new FirefoxDriver(firefoxProfile)

  /**
   * Captures a screenshot and saves it as a file in the specified directory.
   */
  def captureScreenshot(directory: String) {
    capture to directory
  }
}

/**
 * Companion object that facilitates the importing of <code>Firefox</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Firefox</code> members so you can use
 * them in the Scala interpreter.
 */
object Firefox extends Firefox

/**
 * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Safari (an <code>org.openqa.selenium.safari.SafariDriver</code>).
 */
trait Safari extends WebBrowser with ScreenshotCapturer {
  /**
   * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Safari (an <code>org.openqa.selenium.safari.SafariDriver</code>).
   */
  implicit val webDriver = new SafariDriver()

  /**
   * Captures a screenshot and saves it as a file in the specified directory.
   */
  def captureScreenshot(directory: String) {
    capture to directory
  }
}

/**
 * Companion object that facilitates the importing of <code>Safari</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Safari</code> members so you can use
 * them in the Scala interpreter.
 */
object Safari extends Safari

/**
 * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Chrome (an <code>org.openqa.selenium.chrome.ChromeDriver</code>).
 */
trait Chrome extends WebBrowser with ScreenshotCapturer {
  /**
   * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Chrome (an <code>org.openqa.selenium.chrome.ChromeDriver</code>).
   */
  implicit val webDriver = new ChromeDriver()

  /**
   * Captures a screenshot and saves it as a file in the specified directory.
   */
  def captureScreenshot(directory: String) {
    capture to directory
  }
}

/**
 * Companion object that facilitates the importing of <code>Chrome</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Chrome</code> members so you can use
 * them in the Scala interpreter.
 */
object Chrome extends Chrome

/**
 * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Internet Explorer (an <code>org.openqa.selenium.ie.InternetExplorerDriver</code>).
 */
trait InternetExplorer extends WebBrowser with ScreenshotCapturer {
  /**
   * <code>WebBrowser</code> subtrait that defines an implicit <code>WebDriver</code> for Internet Explorer (an <code>org.openqa.selenium.ie.InternetExplorerDriver</code>).
   */
  implicit val webDriver = new InternetExplorerDriver()

  /**
   * Captures a screenshot and saves it as a file in the specified directory.
   */
  def captureScreenshot(directory: String) {
    capture to directory
  }
}

/**
 * Companion object that facilitates the importing of <code>InternetExplorer</code> members as 
 * an alternative to mixing it in. One use case is to import <code>InternetExplorer</code> members so you can use
 * them in the Scala interpreter.
 */
object InternetExplorer extends InternetExplorer

/*
 * <p>
 * If you mix in <a href="../ScreenshotOnFailure.html"><code>ScreenshotOnFailure</code></a>, ScalaTest will capture a screenshot and store it to either the system temp directory
 * or a directory you choose, and send the filename to the report, associated with the failed test. The <code>ScreenshotOnFailure</code> trait requires that it be
 * mixed into a <a href="../ScreenshotCapturer.html"><code>ScreenshotCapturer</code></a>, which trait <code>WebBrowser</code> does not extend. To satisfy this
 * requirement, you can extend one of <code>WebBrowser</code>'s subtraits, such as:
 * </p>
 * 
 * <pre class="stHighlight">
 * class WebAppSpec extends Firefox with ScreenshotOnFailure {
 *   // ...
 * }
 * </pre>
 *
*/
