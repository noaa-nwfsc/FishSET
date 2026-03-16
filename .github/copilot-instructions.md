## 1. Review Philosophy & Response Format
* Only comment when you have HIGH CONFIDENCE (>80%) that a genuine issue or standard violation exists.
* Be concise: use one sentence per comment when possible. 
* Focus on actionable feedback. Do not leave "maybe" or "consider" comments unless suggesting a significant performance or security improvement.
* Format responses as: (1) The problem, (2) Why it matters, (3) Suggested fix in a Markdown code block.

## 2. CI Pipeline & Scope Constraints (DO NOT REVIEW)
* The repository automatically runs `devtools::check()`. Do NOT comment on basic R syntax, missing test configurations, or basic build issues that `R CMD check` will catch.
* **SQL Code:** Ignore all embedded SQL queries and strings. Do not attempt to optimize or review SQL syntax.
* **Formatting:** Do not comment on spacing, indentation, or quotes.

## 3. R & Shiny Coding Standards
* **Naming Conventions:** * STRICTLY enforce `snake_case` for all variables. Do NOT use `camelCase`.
  * UI elements must use descriptive prefixes (e.g., `input_name`, `output_plot`).
  * Reactive variables must be prefixed with `rv_` (e.g., `rv_data`).
  * Buttons must be suffixed with `_btn` (e.g., `run_model_btn`).
  * Modules must be suffixed with `_ui` or `_server` (e.g., `zone_closure_ui`).
* **Modularity & UI:**
  * Enforce the use of Shiny modules (`_ui` and `_server` pairs) to split logic.
  * Ensure UI layouts use `bslib` responsive functions (`page_fillable()`, `page_fluid()`, `fillable = TRUE`).
* **Reactivity Best Practices:**
  * Flag unnecessary or nested reactivity. Do not wrap `render*` functions around static text.
  * **CRITICAL:** Ensure `req()` is used in server-side logic before processing any input values to prevent invalid inputs.

## 4. Priority Areas: Error Handling & Documentation
* **Error Catching (CRITICAL):** Look closely for missing error handling during data loading or processing. Enforce the use of `tryCatch()` blocks. Ensure custom, user-friendly error messages are provided for the UI.
* **Documentation:** * Enforce `roxygen2` style documentation for all standalone functions in the `R/` directory. 
  * Require comments describing the purpose, expected inputs, and expected outputs for complex reactive expressions.

## 5. C++ and Rcpp Standards
* **Indexing (CRITICAL):** Carefully review array and matrix indexing in C++ code. Remember that R is 1-based but C++ is 0-based. Flag potential off-by-one errors when interfacing between the two.
* **Type Safety:** Ensure safe and appropriate type mapping between R objects and Rcpp data types (e.g., `NumericVector`, `DataFrame`).

## 6. Performance Optimization
* Flag expensive operations that should be cached (`reactiveCache`).
* Suggest `session$onSessionEnded()` for cleaning up background tasks.
* Suggest `ExtendedTask` for computationally expensive background operations that would otherwise block the Shiny UI.
