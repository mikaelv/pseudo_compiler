package pseudoc

import org.scalajs.dom
import org.scalajs.dom.{document, window}

object WebMain {
  def main(args: Array[String]): Unit = {
    // Wait for the DOM to be ready
    dom.document.addEventListener("DOMContentLoaded", { (_: dom.Event) =>
      setupUI()
    })
  }

  private def setupUI(): Unit = {
    // Create a basic UI for the pseudo code interpreter
    val body = document.body
    
    // Clear any existing content
    body.innerHTML = ""
    
    // Create title
    val title = document.createElement("h1")
    title.textContent = "Pseudo Code Interpreter"
    body.appendChild(title)
    
    // Create dropdown for algorithm examples
    val dropdownLabel = document.createElement("label")
    dropdownLabel.textContent = "Algorithm Examples:"
    body.appendChild(dropdownLabel)
    
    val dropdown = document.createElement("select").asInstanceOf[dom.html.Select]
    dropdown.id = "algorithmSelector"
    dropdown.onchange = (_: dom.Event) => loadSelectedAlgorithm()
    
    // Add options to dropdown
    val defaultOption = document.createElement("option").asInstanceOf[dom.html.Option]
    defaultOption.value = "default"
    defaultOption.textContent = "-- Select an example --"
    dropdown.appendChild(defaultOption)
    
    AlgorithmExamples.examples.foreach { case (key, example) =>
      val option = document.createElement("option").asInstanceOf[dom.html.Option]
      option.value = key
      option.textContent = example.name
      dropdown.appendChild(option)
    }
    
    body.appendChild(dropdown)
    
    // Create Ace editor
    val inputLabel = document.createElement("label")
    inputLabel.textContent = "Enter your pseudo code:"
    body.appendChild(inputLabel)
    
    val editorDiv = document.createElement("div").asInstanceOf[dom.html.Div]
    editorDiv.id = "pseudoCodeInput"
    
    body.appendChild(editorDiv)
    
    // Initialize Ace Editor
    val ace = dom.window.asInstanceOf[scala.scalajs.js.Dynamic].ace
    if (ace != null) {
      val editor = ace.edit("pseudoCodeInput")
      editor.setTheme("ace/theme/textmate")
      editor.session.setMode("ace/mode/pseudocode")
      editor.setValue("""Algorithme: hello_world
Variables:
  x : entier
  message : chaine

Debut
  x <- 25
  message <- "Hello World "
  Ecrire(message, x, "\NL")
Fin""", -1)
      
      // Configure editor options
      editor.setOptions(scala.scalajs.js.Dynamic.literal(
        "fontSize" -> "14px",
        "showLineNumbers" -> true,
        "wrap" -> true,
        "tabSize" -> 2
      ))
      
      // Store reference to editor for later use
      dom.window.asInstanceOf[scala.scalajs.js.Dynamic].pseudoCodeEditor = editor
    }
    
    // Create run button
    val runButton = document.createElement("button").asInstanceOf[dom.html.Button]
    runButton.textContent = "Run Code"
    runButton.onclick = (_: dom.MouseEvent) => runPseudoCode()
    body.appendChild(runButton)
    
    // Create output area
    val outputLabel = document.createElement("label")
    outputLabel.textContent = "Output:"
    body.appendChild(outputLabel)
    
    val outputArea = document.createElement("pre").asInstanceOf[dom.html.Pre]
    outputArea.id = "output"
    body.appendChild(outputArea)
    
  }
  
  private def runPseudoCode(): Unit = {
    val outputArea = document.getElementById("output").asInstanceOf[dom.html.Pre]
    
    // Get code from Ace editor
    val editor = dom.window.asInstanceOf[scala.scalajs.js.Dynamic].pseudoCodeEditor
    val code = if (editor != null) {
      editor.getValue().asInstanceOf[String]
    } else {
      // Fallback if Ace not initialized
      ""
    }
    
    if (code.trim.isEmpty) {
      outputArea.textContent = "Please enter some pseudo code to run."
      return
    }
    
    // Create a web console implementation
    val webConsole = WebConsoleIO()
    
    // Run the interpreter
    PseudoInterpreter.run(code, webConsole) match {
      case Left(error) =>
        outputArea.textContent = s"Error: $error"
        outputArea.className = "error"
        
      case Right(result) =>
        outputArea.textContent = result.console.getOutput
        outputArea.className = "success"
    }
  }
  
  private def loadSelectedAlgorithm(): Unit = {
    val dropdown = document.getElementById("algorithmSelector").asInstanceOf[dom.html.Select]
    
    val selectedKey = dropdown.value
    
    if (selectedKey != "default") {
      AlgorithmExamples.examples.get(selectedKey) match {
        case Some(example) =>
          // Set code in Ace editor
          val editor = dom.window.asInstanceOf[scala.scalajs.js.Dynamic].pseudoCodeEditor
          if (editor != null) {
            editor.setValue(example.code, -1)
          }
        case None =>
          // Should not happen, but handle gracefully
          val editor = dom.window.asInstanceOf[scala.scalajs.js.Dynamic].pseudoCodeEditor
          if (editor != null) {
            editor.setValue("// Algorithm not found", -1)
          }
      }
    }
  }
}