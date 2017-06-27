#' Intraclass Correlation
#'
#' This function computes an intraclass correlation value from
#' an empty random intercept fitted nlme model object. This function
#' is specific to 2-level models.
#'
#' @param model A fitted nlme object
#'
#' @examples icc(model1)
#'
#' @export
icc <- function(model) {
        vars <- as.numeric(VarCorr(model)[,"Variance"])
        matrix <- matrix(vars, nrow=2)
        compute <- matrix[1,1]/(matrix[1,1] + matrix[2,1])
        return(round(compute, 2))
}
