# Lambda Calculus Interpreter

## Project Components
### Lambda Expression Representation
The project uses a Tree Data Architecture (TDA) to represent lambda expressions, consisting of three main constructors and an additional one for macros:
- `Var String`: Represents variables (lowercase English letters).
- `App Lambda Lambda`: Represents function application.
- `Abs String Lambda`: Represents function abstraction ($\lambda$ abstractions).
- `Macro String`: Represents named sub-expressions (introduced in later stages).

### Evaluation & Reduction Strategies
The interpreter focuses on reaching the Normal Form of an expression through $\beta$-reduction. Key implemented logic includes:
- Variable Capture Prevention: Functions like `vars`, `freeVars`, and `newVars` are used to detect and resolve name collisions by renaming variables during substitution.
- Reduction Steps:
  - Normal Order Strategy: Reduces the leftmost, outermost redex first.
  - Applicative Order Strategy: Reduces the leftmost, innermost redex first.
- `simplify`: A high-level function that repeatedly applies reduction steps until the expression reaches its normal form.

### Parsing
To transform strings into the Lambda TDA, a custom Monadic Parser was implemented.

## Standard Library (Default.hs)
The project includes a "Standard Library" of common lambda combinators, implemented purely as lambda expressions:
- Booleans: `TRUE`, `FALSE`, `AND`, `OR`, `NOT`, `XOR`.
- Pairs: `PAIR`, `FIRST`, `SECOND`.
- Natural Numbers: `N0`, `N1`, `N2`, `SUCC`, `PRED`, `ADD`, `SUB`, `MULT`.

### Implementation Functions
- `vars` / `freeVars`: Lists all variables or only free variables in an expression.
- `isNormalForm`: Checks if an expression has no more reducible redexes.
- `reduce`: Performs a single $\beta$-reduction step while avoiding variable capture.
- `normalStep` / `applicativeStep`: Applies a single reduction step based on the chosen strategy.
- `parseLambda` / `parseLine`: Converts string input into internal TDA structures or bindings.
- `simplifyCtx`: Evaluates expressions within a context containing macros.
