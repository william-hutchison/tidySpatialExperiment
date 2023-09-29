#' @name tbl_format_header
#' @rdname tbl_format_header
#' @inherit pillar::tbl_format_header
#' 
#' @examples
#' # TODO
#' 
#' @importFrom rlang names2
#' @importFrom pillar align
#' @importFrom pillar get_extent
#' @importFrom pillar style_subtle
#' @importFrom pillar tbl_format_header
#' @export
tbl_format_header.tidySpatialExperiment <- function(x, setup, ...) {

    number_of_features <- x |> attr("number_of_features")
    assay_names <- x |> attr("assay_names")
  
    named_header <- setup$tbl_sum
  
    # Change name
    names(named_header) <- "A SpatialExperiment-tibble abstraction"
  
    if (all(names2(named_header) == "")) {
        header <- named_header
    } else {
        header <-
            paste0(
                align(paste0(names2(named_header), ":"), space = NBSP),
                " ",
                named_header
            ) %>%
      
            # Add further info single-cell
            append(sprintf(
                "\033[90m Features=%s | Cells=%s | Assays=%s\033[39m",
                number_of_features,
                nrow(x),
                assay_names %>% paste(collapse=", ")
            ), after = 1)
    }
  
    style_subtle(pillar___format_comment(header, width = setup$width))
}

#' @name formatting
#' @rdname formatting
#' @aliases print
#' @inherit tibble::formatting
#' @return Prints a message to the console describing
#'   the contents of the `tidySingleCellExperiment`.
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     print()
#'     
#' @importFrom vctrs new_data_frame
#' @importFrom SummarizedExperiment assayNames
#' @export
print.SpatialExperiment <- function(x, ..., n = NULL, width = NULL) {

    x |>
        as_tibble(n_dimensions_to_return = 5) |>
        new_data_frame(class = c("tidySpatialExperiment", "tbl")) %>%
        add_attr(nrow(x), "number_of_features") %>%
        add_attr(assays(x) %>% names, "assay_names") %>%
        print()
  
    invisible(x)
}
