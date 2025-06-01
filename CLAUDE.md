# Pseudo Compiler Project Guidelines

## Running tests
use sbt

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

### Parser Design
- **Unified Assignment Parser**: Single assignment parser tries different expression types in sequence
- Order parsers carefully when they might have overlapping patterns
- For alternative parsers (using `|`), place the more specific patterns first
- Be aware that the `expressionString` parser can match empty sequences, which may cause ambiguity
- Assignment parsing: `identifier ~ "<-" ~ (IntExpression | StringExpression | BoolExpression)`

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
- **Unified Assignment Type**: Uses a single `Assignment(variable: String, value: Expression)` case class
- Eliminates parser ambiguity by avoiding multiple assignment types (StringAssignment, IntAssignment, BoolAssignment)
- Type checking happens at evaluation time based on the expression type and variable's declared type
- Expression types are determined using pattern matching on TypedExpression subtypes
- Type compatibility is enforced in the VarMap with boxing support for Java types

## Type Compatibility and Boxing
- **VarMap Type Checking**: Handles Java boxing compatibility automatically
- Int/Integer compatibility: `classOf[Int]` and `classOf[java.lang.Integer]` are treated as compatible
- Boolean/boolean compatibility: `classOf[Boolean]` and `classOf[java.lang.Boolean]` are treated as compatible
- This resolves runtime type errors when Scala's boxed types don't match expected primitive types

## Example Usage

```scala
// Unified assignment creation
val assignment = Assignment("x", IntLiteral(42))
val stringAssign = Assignment("msg", StringLiteral("hello"))
val boolAssign = Assignment("flag", BoolLiteral(true))

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