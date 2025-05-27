# Pseudo Compiler Project Guidelines

## Running tests
Do not try to build or run tests, the human prefers to do it himself.

## Code Style Preferences

### Immutability
- Prefer immutable classes and data structures
- When state changes are needed, return new instances rather than modifying existing ones
- Avoid mutable state in object companions

### Testing
- Test console output by capturing it in tests
- Use TestConsoleOutput for assertions in tests
- Add assertions for expected output content

## Project Structure
- Console output operations are managed through the ConsoleOutput trait
- Two implementations available:
  - DefaultConsoleOutput: Prints to console and captures output
  - TestConsoleOutput: Only captures output for testing

## Example Usage

```scala
// For regular execution with console output
val result = Ast.eval(stmt, vars, DefaultConsoleOutput())

// For testing without console output
val testConsole = Ast.eval(stmt, vars, TestConsoleOutput())
testConsole.getOutput shouldBe "expected output"
```