
setMethod(
    f = "show",
    signature = "SpatialExperiment",
    definition = function(object) {
        if (
          isTRUE(x = getOption(x = "restore_SpatialExperiment_show", default = FALSE))
        ) {
            f <-getMethod(
              f = "show",
              signature = "SummarizedExperiment",
              where = asNamespace(ns = "SummarizedExperiment")
            )
            f(object = object)

        } else {  print(object)  }
    }
)

setClass("tidySpatialExperiment", contains = "SpatialExperiment")

#' Extract and join information for features.
#'
#'
#' @description join_features() extracts and joins information for specified features
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom ttservice join_features
#' @importFrom tibble as_tibble
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#'
#' @name join_features
#' @rdname join_features
#'
#' @param .data A SpatialExperiment object
#' @param features A vector of feature identifiers to join
#' @param all If TRUE return all
#' @param exclude_zeros If TRUE exclude zero values
#' @param shape Format of the returned table "long" or "wide"
#' @param ... Parameters to pass to join wide, i.e. assay name to extract feature abundance from and gene prefix, for shape="wide"
#'
#' @details This function extracts information for specified features and returns the information in either long or wide format.
#'
#' @return An object containing the information.for the specified features
#'
#' @examples
#' `%>%` <- magrittr::`%>%`
#' example(read10xVisium)
#'
#' spe %>%
#'     join_features(features = "ENSMUSG00000025900")
#'
#' @export
NULL

#' join_features
#'
#' @docType methods
#' @rdname join_features
#'
#' @return An object containing the information.for the specified features
#'
setMethod("join_features", "SpatialExperiment",  function(.data,
                                               features = NULL,
                                               all = FALSE,
                                               exclude_zeros = FALSE,
                                               shape = "long", ...)
{

        # CRAN Note
        .cell = NULL
        .feature = NULL

        # Shape is long
        if (shape == "long") {
          
            # Join feature abundance with colData by index
            .data %>%
                colData() %>%
                tibble::as_tibble(rownames = c_(.data)$name) %>%
                tibble::rowid_to_column("index") %>%
                dplyr::mutate(index = as.character(index)) %>%
                dplyr::left_join(
                    get_abundance_sc_long(
                        .data = .data,
                        features = features,
                        all = all,
                        exclude_zeros = exclude_zeros
                    ),
                    by = "index"
                ) %>%
                dplyr::mutate("index" = NULL)
          
          # Print message about changing data type
          message("tidySpatialExperiment says: A data frame is returned for independent data analysis.")
        }

        # Shape is wide
        else {
          colData(.data) <- 
            .data %>% 
                colData() %>%
                tibble::as_tibble(rownames = c_(.data)$name) %>%
                tibble::rowid_to_column("index") %>%
                dplyr::mutate(index = as.character(index)) %>%
                left_join(
                    get_abundance_sc_wide(
                        .data = .data,
                        features = features,
                        all = all, ...
                    ),
                    by = "index") %>%
                dplyr::mutate("index" = NULL) %>%
                as_meta_data(.data)
            .data
        } 
})

#' Aggregate cells
#'
#' @description Combine cells into groups based on shared variables and aggregate feature counts.
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom tibble enframe
#' @importFrom Matrix rowSums
#'
#' @name aggregate_cells
#' @rdname aggregate_cells
#' 
#' @param .data A tidySpatialExperiment object
#' @param .sample A vector of variables by which cells are aggregated
#' @param slot The slot to which the function is applied
#' @param assays The assay to which the function is applied
#' @param aggregation_function The method of cell-feature value aggregation
#' 
#' @return A SummarizedExperiment object
#' 
#' @examples 
#' `%>%` <- magrittr::`%>%`
#' example(read10xVisium)
#'
#' spe %>%
#'     aggregate_cells(sample_id, assays = "counts")
#'
#' @export
aggregate_cells <- function(.data, .sample = NULL, slot = "data", assays = NULL, aggregation_function = rowSums) {
  
  .sample = enquo(.sample)
  
  # Subset only wanted assays
  if(!is.null(assays)){
    .data@assays@data = .data@assays@data[assays]
  }
  
  .data %>%
    
    nest(data = -!!.sample) %>%
    mutate(.aggregated_cells = as.integer(map(data, ~ ncol(.x)))) %>% 
    mutate(data = map(data, ~ 
                        
                        # loop over assays
                        map2(
                          as.list(assays(.x)), names(.x@assays),
                          
                          # Get counts
                          ~  .x %>%
                            aggregation_function(na.rm = T) %>%
                            enframe(
                              name  = "feature",
                              value = sprintf("%s", .y)
                            ) %>%
                            mutate(feature = as.character(feature)) 
                        ) %>%
                        Reduce(function(...) full_join(..., by=c("feature")), .)
                      
    )) %>%
    left_join(.data %>% as_tibble() %>% subset(!!.sample), by = quo_names(.sample)) %>%
    unnest(data) %>%
    
    drop_class("tidySpatialExperiment_nested") |> 
    
    as_SummarizedExperiment(.sample = !!.sample, .transcript = feature, .abundance = !!as.symbol(names(.data@assays)))
}