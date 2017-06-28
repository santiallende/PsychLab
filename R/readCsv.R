#' Read CSV File
#'
#' This function reads a CSV file in a standard way.
#' Header is set to TRUE. stringsAsFactors is set to FALSE.
#' na.strings defaults to c("", " ", "NA", "."). You may
#' use naString to specify an additional NA symbol.
#'
#'
#' @param file A csv file in your working directory. Use quotes.
#'
#' @param naString An optional argument specifying one extra character/string/number to assign NA to.
#' Use quotes if it is a character or string. Only one extra character/string/number currently supported.
#'
#' @examples readCsv(myFile.csv)
#'
#' @export

readCsv <- function(file, naString = NULL) {
        if (is.null(naString) == T) {
                read.csv(file,
                         header=T,
                         na.strings = c("", " ", "NA", "."),
                         stringsAsFactors=FALSE)
        } else {
                read.csv(file,
                         header=T,
                         na.strings = c("", " ", "NA", ".", naString),
                         stringsAsFactors=FALSE)
        }
}

