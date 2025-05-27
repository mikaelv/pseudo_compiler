# Pseudo Compiler Project Guidelines

## Running tests
Do not try to build or run tests, the human prefers to do it himself.

## Code Style Preferences

### Immutability
- Prefer immutable classes and data structures
- When state changes are needed, return new instances rather than modifying existing ones
- Avoid mutable state in object companions

### Type Safety
- Use type-specific classes rather than generic ones when possible
- Enforce type compatibility at compile time using typed classes
- Avoid implicit type conversions between strings and integers
- Perform additional runtime type checking when needed

### Parser Ordering
- Order parsers carefully when they might have overlapping patterns
- For alternative parsers (using `|`), place the more specific patterns first
- Be aware that the `expressionString` parser can match empty sequences, which may cause ambiguity

### Testing
- Test console output by capturing it in tests
- Use TestConsoleOutput for assertions in tests
- Add assertions for expected output content

## Project Structure
- Console output operations are managed through the ConsoleOutput trait
- Two implementations available:
  - DefaultConsoleOutput: Prints to console and captures output
  - TestConsoleOutput: Only captures output for testing
- The interpreter now returns EvalResult containing both console output and updated variables

## Variable Assignment Implementation
- Variable assignments are type-specific with StringAssignment and IntAssignment
- The Assignment trait is sealed to ensure type safety
- Type checking happens both at compile time and runtime
- Parser tries integer assignments before string assignments to avoid ambiguity

## Example Usage

```scala
// For regular execution with console output
val result = PseudoInterpreter.eval(stmt, vars, DefaultConsoleOutput())

// For testing without console output
val testConsole = PseudoInterpreter.eval(stmt, vars, TestConsoleOutput())
testConsole.getOutput shouldBe "expected output"

// For operations that need to track variable state
val evalResult = PseudoInterpreter.evalWithVars(stmt, vars, console)
val updatedVars = evalResult.vars
val output = evalResult.console.getOutput
```