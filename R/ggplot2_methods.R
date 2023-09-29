#' @name ggplot
#' @rdname ggplot
#' @inherit ggplot2::ggplot
#' @title Create a new \code{ggplot} from a \code{tidySpatialExperiment}
#' @return `ggplot`
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     ggplot(ggplot2::aes(x = .cell, y = array_row)) +
#'     ggplot2::geom_point()
NULL
