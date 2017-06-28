#' Random Effect Ninety-Five Percent Confidence Intervals
#'
#' This function computes a random effect 95\% percent confidence interval from
#' a fitted nlme object. That is, it computes an estimate of individual
#' differences (i.e., random effect variation) for a fixed effect. This
#' function is specific to 2-level models.
#'
#' @param model A fitted nlme object
#'
#' @param Param The random effect of interest. Use quotes.
#'
#' @examples randomCI(model1, "time")
#'
#' @export
#'
randomCI <- function(model, Param) {
        Param <- noquote(Param)
        if (Param == "Intercept") {
                fe <- matrix(as.numeric(data.frame(intervals(model)[1])["(Intercept)",]))[2,1]
                sdRandomVar <- matrix(as.numeric(data.frame(intervals(model)[2])["sd((Intercept))",]))[2,1]
                ciLower <- fe - 1.96*sdRandomVar
                ciUpper <- fe + 1.96*sdRandomVar
                return(round(c(ciLower, ciUpper), 2))
        }  else {
                paramFixed <- sprintf("%s", Param)
                paramRandom <- sprintf("sd(%s)", Param)
                fe <- matrix(as.numeric(data.frame(intervals(model)[1])[paramFixed,]))[2,1]
                sdRandomVar <- matrix(as.numeric(data.frame(intervals(model)[2])[paramRandom,]))[2,1]
                ciLower <- fe - 1.96*sdRandomVar
                ciUpper <- fe + 1.96*sdRandomVar
                return(round(c(ciLower, ciUpper), 2))
        }
}
