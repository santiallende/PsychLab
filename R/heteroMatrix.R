#' Heterogenous Variance-Covariance Matrix
#'
#' This function computes a heterogenous variance-covariance matrix from a fitted nlme object.
#'
#' @param model A fitted nlme object
#'
#' @examples heteroMatrix(model)
#'
#' @export

heteroMatrix <- function(model) {
        weights <- coef(model$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE) #extract #weights
        cors <- corMatrix(model$modelStruct$corStruct) #extract rho matricies for id's
        cors <- cors[order(sapply(cors, length), decreasing = T)] #order id rho matricies with complete #matricies at top
        cors <- cors[[1]] #select first element in ordered list to grab id that has a complete rho matrix
        vars <- c(weights)^2*model$sigma^2 #create variances
        covs <- outer(vars,vars,function(x,y) sqrt(x)*sqrt(y)) #create shared variances
        matrix <- cors*covs #create hetero var-cov matrix
        print(matrix)
}
