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
    
    // Create input textarea
    val inputLabel = document.createElement("label")
    inputLabel.textContent = "Enter your pseudo code:"
    body.appendChild(inputLabel)
    
    val inputArea = document.createElement("textarea").asInstanceOf[dom.html.TextArea]
    inputArea.id = "pseudoCodeInput"
    inputArea.style.width = "100%"
    inputArea.style.height = "200px"
    inputArea.style.fontFamily = "monospace"
    inputArea.value = """Algorithme: hello_world
Variables:
  x : entier
  message : chaine

Debut
  x <- 25
  message <- "Hello World "
  Ecrire(message, x, "\NL")
Fin"""
    body.appendChild(inputArea)
    
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
    outputArea.style.width = "100%"
    outputArea.style.height = "200px"
    outputArea.style.border = "1px solid #ccc"
    outputArea.style.padding = "10px"
    outputArea.style.backgroundColor = "#f9f9f9"
    outputArea.style.overflow = "auto"
    body.appendChild(outputArea)
    
    // Add some basic styling
    body.style.fontFamily = "Arial, sans-serif"
    body.style.margin = "20px"
  }
  
  private def runPseudoCode(): Unit = {
    val inputArea = document.getElementById("pseudoCodeInput").asInstanceOf[dom.html.TextArea]
    val outputArea = document.getElementById("output").asInstanceOf[dom.html.Pre]
    
    val code = inputArea.value
    
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
        outputArea.style.color = "red"
        
      case Right(result) =>
        outputArea.textContent = result.console.getOutput
        outputArea.style.color = "black"
    }
  }
  
  private def loadSelectedAlgorithm(): Unit = {
    val dropdown = document.getElementById("algorithmSelector").asInstanceOf[dom.html.Select]
    val inputArea = document.getElementById("pseudoCodeInput").asInstanceOf[dom.html.TextArea]
    
    val selectedKey = dropdown.value
    
    if (selectedKey != "default") {
      AlgorithmExamples.examples.get(selectedKey) match {
        case Some(example) =>
          inputArea.value = example.code
        case None =>
          // Should not happen, but handle gracefully
          inputArea.value = "// Algorithm not found"
      }
    }
  }
}