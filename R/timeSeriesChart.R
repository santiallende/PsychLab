#' Time Series Chart
#'
#' Creates a time series chart with data structured in long format. Dependenceis:
#' ggplot2 and plotly.
#'
#' @param data Is the dataframe that you are interested in visualizing.
#'
#' @param timepoint Is the name of the column containing ordered time points.
#'
#' @param response Is the name of the column containing the dependent (y axis) variable values.
#'
#' @param tx Is optional and may be specified to create an interaction graph. It is the name
#' of the column containing treatment group membership.
#'
#' @examples timeSeriesChart(meanMcs, time, value, MBSR1CBT2)
#'
#' @export

timeSeriesChart <- function(data, timepoint, response, tx) {
        if (missing(tx)) {
                line <- ggplot(data, aes_string(timepoint, response, group = 1))
                line + stat_summary(fun.y = mean, geom = "point", colour = "skyblue3") +
                        stat_summary(fun.y = mean, geom = "line", colour = "skyblue3") +
                        stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
                                     width = 0.5, colour = "skyblue3") +
                        labs(x = "timepoint", y = "response", title = substitute(data))
        } else {
                line <- ggplot(data, aes_string(timepoint, response, group = 1, colour = tx))
                line + stat_summary(fun.y = mean, geom = "point", aes_string(group = tx)) +
                        stat_summary(fun.y = mean, geom = "line", aes_string(group = tx)) +
                        geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.1, position_dodge(0.1))
                        labs(x = "timepoint", y = "response", colour = "tx", title = substitute(data))
        }
}
