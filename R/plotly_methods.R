#' @name plot_ly
#' @rdname plot_ly
#' @inherit ttservice::plot_ly
#' @return `plotly`
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     plot_ly(x = ~ array_col, y = ~ array_row) 
#' 
#' @importFrom ttservice plot_ly
#' @export
plot_ly.SpatialExperiment <- function(data = data.frame(), ..., 
                                      type = NULL, name = NULL,
                                      color = NULL, colors = NULL, 
                                      alpha = NULL, stroke = NULL, 
                                      strokes = NULL, alpha_stroke = 1,
                                      size = NULL, sizes = c(10, 100),
                                      span = NULL, spans = c(1, 20),
                                      symbol = NULL, symbols = NULL,
                                      linetype = NULL, linetypes = NULL,
                                      split = NULL, frame = NULL,
                                      width = NULL, height = NULL, 
                                      source="A") {
    data |>
        as_tibble() |>
        ttservice::plot_ly(..., type = type, name = name, 
                           color = color, colors = colors, 
                           alpha = alpha, stroke = stroke, 
                           strokes = strokes, alpha_stroke = alpha_stroke, 
                           size = size, sizes = sizes,
                           span = span, spans = spans,
                           symbol = symbol, symbols = symbols,
                           linetype = linetype, linetypes = linetypes,
                           split = split, frame = frame,
                           width = width, height = height, 
                           source = source
        )
}
