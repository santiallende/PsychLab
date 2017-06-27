#' Pseudo R-Squared
#'
#' This function computes a pseudo R-squared value for the
#' amount of variance explained in a variance component
#' of nested nlme model objects. This function is specific to
#' 2-level models. Note that model1 is nested in model2.
#'
#' @param model1 A fitted nlme object with fewer fixed effect
#' parameters than model B, but with an equal number of variance components.
#'
#' @param model2 A fitted nlme object with more fixed effect
#' parameters than model B, but with an equal number of variance components.
#'
#' @param varParam The variance component that you are interested in
#' computing an pseudo R-squared for. That is, the variance component
#' that you expect to change due to adding a fixed effect.
#'
#' @examples randomCI(model1, "time")
#'
#' @export
#' @importFrom nlme intervals
pseudoR <- function(model1, model2, varParam) {
        varParam <- noquote(varParam)
        if (varParam == "residual") {
                #extract sd of residual variance and square it to compute variance
                residVar1 <- data.frame(intervals(model1)[3])["est.",]^2
                residVar2 <- data.frame(intervals(model2)[3])["est.",]^2
                #compute pseudo-R2
                compute <- (residVar1 - residVar2) / residVar1
                return(round(compute*100, 2))
        }
        if (varParam == "Intercept") {
                #extract sd of intercept and square it to compute variance
                InterceptVar1 <- data.frame(intervals(model1)[2])[1,2]^2
                InterceptVar2 <- data.frame(intervals(model2)[2])[1,2]^2
                #compute pseudo-R2
                compute <- (InterceptVar1 - InterceptVar2) / InterceptVar1
                return(round(compute*100, 2))
        }  else {
                paramRandom <- sprintf("sd(%s)", varParam)
                #extract sd of varParam and square it to compute variance
                paramVar1 <- data.frame(intervals(model1)[2])[paramRandom,][1,2]^2
                paramVar2 <- data.frame(intervals(model2)[2])[paramRandom,][1,2]^2
                #compute pseudo-R2
                compute <- (paramVar1 - paramVar2) / paramVar1
                return(round(compute*100, 2))
        }
}
