# Pseudo Compiler Project Guidelines

## Methodology
Always write tests first, ask for validation before implementing the production code.

## Running tests
use sbt

## Commits
- All commits should start with [CLAUDE]
- Do not add links to claude.ai or "authored by"
- Use the exact phrase(s) from user request(s) in the commit message
- Can add your own implementation details as well


## Array Type System Implementation

### Array Type Architecture
- **ArrayExpression trait**: Extends `TypedExpression[Array[Int]]` for compile-time type safety
- **ArrayRef and ArrayLiteral**: Case classes implementing array references and literals
- **ArrayAccess**: Extends `IntExpression` for array element access operations
- **ArrayVariableDecl**: Specialized declaration class for arrays with size information

### Array Syntax Support
- **Array literals**: Use curly braces `{1, 2, 3}` instead of square brackets
- **Array access**: Use square brackets for indexing `arr[2]`
- **Size declarations**: Support syntax `name [size] : arraytype` (e.g., `arr [10] : tableau d'entier`)
- **Multiple type names**: Support French (`tableau d'entier`) and English (`arrayint`) variants

### Parser Implementation Insights
- **Expression precedence**: Order matters - `intExpr` must be tried before `arrayExpr` to handle array access correctly
- **Type-aware parsing**: Use `variableReference` instead of manual filtering for proper symbol table lookup
- **Separate parsers**: Split variable declarations into `regularVariableDecl` and `arrayVariableDecl` functions
- **Parser architecture**: Create separate classes for different declaration types instead of modifying existing structures

### Array Evaluation and Type Checking
- **Size-aware initialization**: Use `Array.fill(size)(0)` for proper size-based array creation
- **Type checking**: Handle `Array[Int]` compatibility in VarMap with boxing support
- **Error handling**: Provide user-friendly type names instead of JVM internal representations
- **Runtime validation**: Check array types at evaluation time with clear error messages

### Development Guidance Patterns
- **Unicode handling**: Be careful with Unicode characters in source files (right single quotation mark vs ASCII apostrophe)
- **Architecture decisions**: Prefer sealed traits and case classes for AST nodes over mutable structures
- **Test organization**: Place array tests in `PseudoCodeParserTest` rather than separate test files
- **Error reporting**: Implement meaningful error messages that help users understand type mismatches

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