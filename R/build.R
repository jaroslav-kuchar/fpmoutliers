#' @title Automatic build of the anomaly detection model
#'
#' @description An experimental implementataion that automatically builds an anomaly detection model
#'
#' @param data \code{data.frame} or \code{transactions} from \code{arules} with input data
#' @param func function name of the method that will be use during the automatic build
#' @param initial_support initial maximum support
#' @param top_outlier_threshold number of top unique outliers as a stopping condition
#' @param iteration_timeout timeout of one iteration
#' @return model of outlier detection
#' @importFrom R.utils withTimeout
#' @export
#' @examples
#' # simple build with default parameters (FPI method)
#' library("fpmoutliers")
#' data("iris")
#' model <- fpmoutliers::build(iris[sample(nrow(iris), 5),])
#'
#' \donttest{
#' # using other anomaly detection methods for the automatic build (e.g. LFPOF)
#' library("fpmoutliers")
#' data("iris")
#' model <- fpmoutliers::build(iris[sample(nrow(iris), 5),], func=LFPOF)
#' }
build <- function(data, func=FPI, initial_support=0.5, top_outlier_threshold=3, iteration_timeout=10){
  model <- list()
  running <- TRUE
  ms <- initial_support
  iteration <- 0

  while(running){
    tryCatch({
      model <- withTimeout({
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
