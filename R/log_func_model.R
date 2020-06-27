#' Log user-created functions or models
#'
#' @param x Name of function.
#' @details  Logs function name, arguments, and, call. Use this function to log user-defined likelihood functions.
#' @export log_func_model
#' @examples
#' \dontrun{
#' my_func <- function(a, b) {
#'   a + b
#' }
#' log_func_model(my_func)
#' }
#'
log_func_model <- function(x) {
  user_function <- list()
  user_function$functionID <- deparse(substitute(x))
  user_function$args <- names(dput(formals(x)))
  user_function$msg <- args(x)
  log_call(user_function)
}
