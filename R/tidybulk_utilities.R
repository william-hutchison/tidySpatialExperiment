#' This is a generalisation of ifelse that accepts an object and return an objects
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom purrr as_mapper
#'
#' @param .x A tibble
#' @param .p A boolean
#' @param .f1 A function
#' @param .f2 A function
#'
#' @return A tibble
ifelse_pipe <- tidySingleCellExperiment:::ifelse_pipe

#' as_SummarizedExperiment
#'
#' @keywords internal
#' @noRd
#' 
#' @description as_SummarizedExperiment creates a `SummarizedExperiment` object from a `tbl` or `tidybulk` tbl formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#'
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang quo_squash
#' @importFrom utils data
#' @importFrom tidyr pivot_longer
#'
#' @param .data A tibble
#' @param .sample The name of the sample column
#' @param .transcript The name of the transcript/gene column
#' @param .abundance The name of the transcript/gene abundance column
#'
#' @return A `SummarizedExperiment` object
as_SummarizedExperiment <- tidySingleCellExperiment:::as_SummarizedExperiment

#' Get column names either from user or from attributes
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom rlang quo_is_symbol
#' @importFrom rlang quo_is_symbolic
#'
#' @param .data A tibble
#' @param .sample A character name of the sample column
#' @param .transcript A character name of the transcript/gene column
#' @param .abundance A character name of the read count column
#'
#' @return A list of column enquo or error
get_sample_transcript_counts <- tidySingleCellExperiment:::get_sample_transcript_counts

get_tt_columns <- tidySingleCellExperiment:::get_tt_columns

#' get_x_y_annotation_columns
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom magrittr equals
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A `tbl` (with at least three columns for sample, feature and transcript abundance) or `SummarizedExperiment` (more convenient if abstracted to tibble with library(tidySummarizedExperiment))
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' @param .abundance_scaled The name of the transcript/gene scaled abundance column
#'
#' @description This function recognise what are the sample-wise columns and transcript-wise columns
#'
#' @return A list
get_x_y_annotation_columns <- tidySingleCellExperiment:::get_x_y_annotation_columns

#' Get matrix from tibble
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom magrittr set_rownames
#' @importFrom rlang quo_is_null
#'
#' @param tbl A tibble
#' @param rownames The column name of the input tibble that will become the rownames of the output matrix
#' @param do_check A boolean
#'
#' @return A matrix
#'
#' @examples
#' tibble(.feature = "CD3G", count=1) |>
#'     as_matrix(rownames=.feature)
as_matrix <- tidySingleCellExperiment:::as_matrix
