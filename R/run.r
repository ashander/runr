#' Run a simulation function
#' 
#' @param data a data.frame (or tibble, in future) 
#' @param fun a function 
#' @param fixed_parameters an environment or list 
#' @param ... additional parameters passed to `fun`
#' @details `fun` must have signature (a1, a2, <...>, aN, fixed_params, ...) where
#'          1) N is the number of columns in data (the names of these arguments
#'          don't matter). Note <...> elides intervening arguments and is NOT
#'          R's ... parameter! 2) the ... parameter is optional 3) The function
#'          must return a data.frame.
#' @return a data.frame including the columns of data and the return from `fun` 
#' @importFrom dplyr do_ %>% rowwise
#' @importFrom lazyeval interp
#' @examples
#' growth <- function(n, r, K, b) {
#'   # Ricker-like growth curve in n = log N
#'   n  + r - exp(n) / K - b
#' }
#' data <- expand.grid(
#'                     b = seq(0.01, 0.5, length.out=10),
#'                     K = exp(seq(0.1, 5, length.out=10)),
#'                     r = seq(0.5, 3.5, length.out=10)
#'                     )
#' initial_data = list(N0=0.9, T=5, reps=100)
#' 
#' growth_runner <- function(r, K, b, ic) {
#'   n0 = ic$N0  
#'   T = ic$T
#'   reps = ic$reps
#'   out <- lapply(1:reps, function(i) {
#'                   for(t in 1:T) {
#'                     n0 <- growth(n0, r, K, b)
#'                     }
#'                   return(n0)
#'                     })
#'   data.frame(n_final = do.call(rbind, out))
#' }
#' 
#' 
#' output <- run(data, growth_runner, initial_data)
#' head(cbind(data, output))
#' @export

run <- function(data, fun, fixed_parameters, ...) {
  assert_that(is.environment(fixed_parameters) || is.list(fixed_parameters), 
              is.function(fun),
              is.data.frame(data), 
              ncol(data) > 0)
  fun_args <- formals(fun)
  assert_that(length(fun_args) >= ncol(data) + 1 &&
              length(fun_args) <= ncol(data) + 2)
  if (length(fun_args) == ncol(data) + 2)
    assert_that(names(fun_args)[ncol(data) + 2] == "...")
  if (names(fun_args)[length(fun_args)] == "...")
      assert_that(length(fun_args) == ncol(data) + 2)

  fixed_parameters <- as.environment(fixed_parameters)
  data %>%
    rowwise %>%
    do_(interp( ~ do.call(fun, c(., fixed_parameters, ...)))) %>%
    as.data.frame()
}

