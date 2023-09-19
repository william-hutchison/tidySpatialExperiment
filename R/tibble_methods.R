#' @name as_tibble
#' @rdname as_tibble
#' @inherit tibble::as_tibble
#' @return `tibble`
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     as_tibble()
#'     
#' @importFrom purrr reduce
#' @importFrom purrr map
#' @importFrom tidyr spread
#' @importFrom tibble enframe
#' @importFrom SummarizedExperiment colData
#' @importFrom pkgconfig get_config
#' @export
as_tibble.SpatialExperiment <- function(x, ...,
    .name_repair=c("check_unique", "unique", "universal", "minimal"),
    rownames=pkgconfig::get_config("tibble::rownames", NULL)) {
    
    # Extract cell metadata
    x_col_data <- 
      x %>%
      colData()
    
    # Add cell label column to cell metadata from SpatialExperiment column names
    x_col_data[c_(x)$name] <- 
      colnames(x)
    
    # Convert to tibble
    x_col_data %>% 
      tibble::as_tibble() %>%
      dplyr::relocate(c_(x)$name) %>%
      
      # Add spatial coordinate information to cell metadata
      left_join(x %>% spatialCoords() %>% tibble::as_tibble(rownames = c_(x)$name), by = c_(x)$name, multiple = "first") %>%

        # Attach reduced dimensions
        when(

            # Only if I have reduced dimensions and special datasets
            ncol(x@int_colData@listData$reducedDims) > 0 ~ (.) %>% cbind(
              special_datasets_to_tibble(x, ...)
            ),

            # Otherwise skip
            ~ (.)
        )
}

#' @name glimpse
#' @rdname glimpse
#' @inherit pillar::glimpse
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     glimpse()
NULL
