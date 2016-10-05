#' Automatic build of anomaly detection model
#'
#' @param data data.frame with input data
#' @param func function name of method
#' @param initial_support maximum length of frequent itemsets
#' @param top_outlier_threshold number of top unique outliers
#' @param iteration_timeout timeout of one iteration
#' @return model of outlier detection
#' @importFrom R.utils evalWithTimeout
#' @export
#' @examples
#' library("fpmoutliers")
#' data("iris")
#' model <- fpmoutliers::build(iris)
#'
build <- function(data, func=FPI, initial_support=0.5, top_outlier_threshold=3, iteration_timeout=10){
  model <- list()
  running <- TRUE
  ms <- initial_support
  iteration <- 0

  while(running){
    tryCatch({
      model <- evalWithTimeout({
        do.call(func,list(data=data, minSupport = ms, mlen = 0))
      }, timeout = iteration_timeout, onTimeout="error");
      if(length(which(model$scores == max(model$scores)))>top_outlier_threshold){
        ms <- ms - ms/10.0
      } else {
        running <- FALSE
      }

    }, error= function(err) {
      if (!"TimeoutException"  %in% class(err)) {
        stop(paste("Unexpecterd error",err))
      }
      message("Timeout")
      running <<- FALSE
    })
    iteration <- iteration + 1
    if(iteration>100) {
      running <- FALSE
    }
  }
  model
}
