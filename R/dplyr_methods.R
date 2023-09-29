#' @name bind_rows
#' @rdname bind_rows
#' @inherit ttservice::bind_rows
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     bind_rows(spe)
#'     
#' @importFrom rlang flatten_if
#' @importFrom rlang is_spliced
#' @importFrom rlang dots_values
#' @importFrom ttservice bind_rows
#' @importFrom SingleCellExperiment cbind
#' @export
bind_rows.SpatialExperiment <- function(..., .id = NULL, add.cell.ids = NULL) {
  
    tts <- flatten_if(dots_values(...), is_spliced)
    SingleCellExperiment::cbind(tts[[1]], tts[[2]], deparse.level = 0)
}

#' @importFrom rlang flatten_if
#' @importFrom rlang is_spliced
#' @importFrom rlang dots_values
#' @importFrom ttservice bind_cols
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment colData<-
bind_cols_ <- function(..., .id = NULL) {
  
  tts <- tts <- flatten_if(dots_values(...), is_spliced)
  colData(tts[[1]]) <- 
      bind_cols(
          colData(tts[[1]]) %>% 
              as.data.frame(),
           tts[[2]], 
          .id=.id) %>% 
      DataFrame()
  
  tts[[1]]
}

#' @rdname bind_rows
#' @aliases bind_cols
#' 
#' @examples 
#' example(read10xVisium)
#' spe |>
#'    bind_cols(1:99)
#' 
#' @export
bind_cols.SpatialExperiment <- bind_cols_

#' @name filter
#' @rdname filter
#' @inherit dplyr::filter
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     filter(in_tissue == TRUE)
#'    
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @export
filter.SpatialExperiment <- function(.data, ..., .preserve = FALSE) {

    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(
        .data,
        (enquos(..., .ignore_empty = "all") %>% map(~ quo_name(.x)) %>% unlist)
    )) {
        .data <- ping_old_special_column_into_metadata(.data)
    }
  
    new_meta <- .data %>%
        as_tibble() %>%
        rowid_to_column("index") %>%
      
        dplyr::filter(..., .preserve=.preserve) %>% 
        as_meta_data(.data)
  
    # Try to solve missing colnames
    if (colnames(.data) %>% is.null() ) {
        message("tidySpatialExperiment says: the input object does not have cell names (colnames(...)). \n Therefore, the cell column in the filtered tibble abstraction will still include an incremental integer vector.")
        new_meta <- new_meta %>% mutate(!!c_(.data)$symbol := as.integer(!!c_(.data)$symbol))
  
    }
    
    # Use index to subset cells from original SpatialExperiment object
    .data[, new_meta[["index"]]]
}

#' @name mutate
#' @rdname mutate
#' @inherit dplyr::mutate
#' @family single table verbs
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     mutate(array_col = 1)
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment colData<-
#' @importFrom rlang enquos
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @export
mutate.SpatialExperiment <- function(.data, ...) {
  
    # Check that we are not modifying a key column
    cols <- enquos(...) %>% names()
    
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(
        .data,
        (enquos(..., .ignore_empty = "all") %>% map(~ quo_name(.x)) %>% unlist)
    )) {
        .data <- ping_old_special_column_into_metadata(.data)
    }
    
    tst <-
        intersect(
          cols %>%
              names(),
          get_special_columns(.data) %>%
              c(get_needed_columns(.data))
        ) %>%
        length() %>%
        gt(0)
    
    if (tst) {
        columns <-
            get_special_columns(.data) %>%
            c(get_needed_columns()) %>%
            paste(collapse = ", ")
        stop(
            "tidySpatialExperiment says: you are trying to rename a column that is view only",
            columns, " ",
            "(it is not present in the colData). If you want to mutate a view-only column, make a copy and mutate that one."
        )
    }
  
    # Extract colData for mutation and save to SpatialExperiment
    colData(.data) <-
        .data %>%
        colData() %>%
        as_tibble(rownames = c_(.data)$name) %>%
        dplyr::mutate(...) %>%
        as_meta_data(.data)
    
    .data
}

#' @name left_join
#' @rdname left_join
#' @inherit dplyr::left_join
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     left_join(
#'         spe |>
#'             filter(in_tissue == TRUE) |>
#'             mutate(new_column = 1)
#'         )
#' 
#' @importFrom SummarizedExperiment colData
#' @importFrom dplyr left_join
#' @importFrom dplyr count
#' @export
left_join.SpatialExperiment <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"),
    ...) {

    # Deprecation of special column names
    if (is_sample_feature_deprecated_used( x, when(by, !is.null(.) ~ by, ~ colnames(y))) ) {
        x <- ping_old_special_column_into_metadata(x)
    }
    
    # Join colData and assign to the returned SpatialExperiment object's colData
    colData(x) <-
        x %>%
        colData() %>%
        tibble::as_tibble(rownames = c_(x)$name) %>%
        dplyr::left_join(
            y %>%
                colData() %>%
                tibble::as_tibble(rownames = c_(y)$name),
            by = by, copy = copy, suffix = suffix, ...) %>%
        as_meta_data(x)
    x
}

#' @name inner_join
#' @rdname inner_join
#' @inherit dplyr::inner_join
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     inner_join(
#'         spe |>
#'             filter(in_tissue == TRUE) |>
#'             mutate(new_column = 1)
#'         )
#' 
#' @importFrom SummarizedExperiment colData
#' @importFrom dplyr inner_join
#' @importFrom dplyr pull
#' @export
inner_join.SpatialExperiment <- function(x, y, by=NULL, copy=FALSE, suffix=c(".x", ".y"), ...) {

    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(x, when(by, !is.null(.) ~ by, ~ colnames(y))) ) {
        x <- ping_old_special_column_into_metadata(x)
    }
  
    # Join colData and attach to the smaller SpatialExperimemt object's colData
    if (ncol(x) < ncol(y)) {
        colData(x) <-
            x %>%
            colData() %>%
            tibble::as_tibble(rownames = c_(x)$name) %>%
            dplyr::left_join(
                y %>%
                    colData() %>%
                    tibble::as_tibble(rownames = c_(y)$name),
                by = by, copy = copy, suffix = suffix, ...) %>%
            as_meta_data(x)
        x
        
    } else {
        colData(y) <-
            y %>%
            colData() %>%
            tibble::as_tibble(rownames = c_(y)$name) %>%
            dplyr::left_join(
                x %>%
                    colData() %>%
                    tibble::as_tibble(rownames = c_(x)$name),
                by = by, copy = copy, suffix = suffix, ...) %>%
          as_meta_data(y)
        y
    }
}

#' @name right_join
#' @rdname right_join
#' @inherit dplyr::right_join
#'
#' @examples
#' example(read10xVisium)
#' 
#' spe |>
#'     right_join(
#'         spe |>
#'             filter(in_tissue == TRUE) |>
#'             mutate(new_column = 1)
#'         )
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom dplyr right_join
#' @importFrom dplyr pull
#' @export
right_join.SpatialExperiment <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
    ...) {

    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(x, when(by, !is.null(.) ~ by, ~ colnames(y))) ) {
        x <- ping_old_special_column_into_metadata(x)
    }
  
    # Join colData and assign to the returned SpatialExperiment object's colData
    colData(y) <-
        y %>%
        colData() %>%
        tibble::as_tibble(rownames = c_(y)$name) %>%
        dplyr::left_join(
            x %>%
                colData() %>%
                tibble::as_tibble(rownames = c_(x)$name),
            by = by, copy = copy, suffix = suffix, ...) %>%
      as_meta_data(y)
    
    y
}

#' @name select
#' @rdname select
#' @inherit dplyr::select
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     select(in_tissue)
#'     
#' @importFrom SummarizedExperiment colData
#' @importFrom tibble as_tibble
#' @export
select.SpatialExperiment <- function(.data, ...) {
  
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(
        .data,
        (enquos(..., .ignore_empty = "all") %>% map(~ quo_name(.x)) %>% unlist)
    )) {
        .data <- ping_old_special_column_into_metadata(.data)
    }
  
    .data %>%
        colData() %>%
        tibble::as_tibble(rownames = c_(.data)$name) %>%
        select_helper(...) %>%
        when(
            # If key columns are missing
            (get_needed_columns(.data) %in% colnames(.)) %>%
                all() %>%
                `!`() ~ {
                message("tidySpatialExperiment says: Key columns are missing. A data frame is returned for independent data analysis.")
                (.)
            },
      
            # If valid SpatialExperiment meta data
            ~ {
                colData(.data) <- (.) %>% as_meta_data(.data)
                .data
            }
        )
}

#' @name sample_n
#' @rdname sample_n
#' @aliases sample_frac
#' @inherit dplyr::sample_n
#' @return `tidySpatialExperiment`
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     sample_n(10)
#' spe |>
#'     sample_frac(0.1)
#'  
#' @importFrom SummarizedExperiment colData
#' @importFrom dplyr sample_n
#' @export
sample_n.SpatialExperiment <- function(tbl, size, replace=FALSE,
    weight=NULL, .env=NULL, ...) {
    lifecycle::signal_superseded("1.0.0", "sample_n()", "slice_sample()")

    new_meta <-
        tbl %>%
        as_tibble() %>%
        dplyr::sample_n(size, replace = replace, weight = weight, .env = .env, ...)

    count_cells <- new_meta %>% select(!!c_(tbl)$symbol) %>% count(!!c_(tbl)$symbol)

    # If repeated cells
    if (count_cells$n %>% max() %>% gt(1) ) {
        message("tidySpatialExperiment says: When sampling with replacement a data frame is returned for independent data analysis.")
        new_meta
    } else {
        new_obj <- tbl[,  new_meta %>% pull(!!c_(tbl)$symbol)]
        new_obj
    }
}

#' @name sample_frac
#' @rdname sample_n
#' @importFrom SummarizedExperiment colData
#' @importFrom dplyr sample_frac
#' 
#' @export
sample_frac.SpatialExperiment <- function(tbl, size=1, replace=FALSE,
    weight=NULL, .env=NULL, ...) {
    lifecycle::signal_superseded("1.0.0", "sample_frac()", "slice_sample()")

    new_meta <- tbl %>%
        as_tibble() %>%
        dplyr::sample_frac(size, replace = replace, weight = weight, .env = .env, ...)

    count_cells <- new_meta %>% select(!!c_(tbl)$symbol) %>% count(!!c_(tbl)$symbol)

    # If repeted cells
    if (count_cells$n %>% max() %>% gt(1) ) {
        message("tidySpatialExperiment says: When sampling with replacement a data frame is returned for independent data analysis.")
        tbl %>%
            as_tibble() %>%
            right_join(new_meta %>% select(!!c_(tbl)$symbol),  by = c_(tbl)$name)
    } else {
        new_obj <- tbl[,  new_meta %>% pull(!!c_(tbl)$symbol)]
        new_obj
    }
}

#' @rdname count
#' @aliases add_count
#' @importFrom dplyr add_count
#'     
#' @export
add_count.SpatialExperiment <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
    
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(
        x,
        (enquos(..., .ignore_empty = "all") %>% map(~ quo_name(.x)) %>% unlist)
    )) {
       x <- ping_old_special_column_into_metadata(x)
    }
    
    colData(x) <- 
        x %>%
        colData() %>%
        tibble::as_tibble(rownames = c_(x)$name) %>% 
        dplyr::add_count(..., wt = !!enquo(wt), sort = sort, name = name)  %>%
        as_meta_data(x)
    
    x
}

#' @name group_by
#' @rdname group_by
#' @inherit dplyr::group_by
#' 
#' @importFrom tidySingleCellExperiment group_by
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     group_by(sample_id)
NULL

#' @name summarise
#' @aliases summarize
#' @inherit dplyr::summarise
#' @family single table verbs
#' 
#' @importFrom tidySingleCellExperiment summarise
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     summarise(mean(array_row))
NULL

#' @name arrange
#' @rdname arrange
#' @inherit dplyr::arrange
#' @family single table verbs
#' 
#' @importFrom tidySingleCellExperiment arrange
#' 
#' @examples
#' example(read10xVisium)
#' 
#' spe |>
#'     arrange(array_row)
NULL

#' @name rename
#' @rdname rename
#' @inherit dplyr::rename
#' @family single table verbs
#' 
#' @importFrom tidySingleCellExperiment rename
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     rename(in_liver = in_tissue)
NULL

#' @name rowwise
#' @rdname rowwise
#' @inherit dplyr::rowwise
#'
#' @importFrom tidySingleCellExperiment rowwise
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     rowwise()
NULL

#' @name count
#' @rdname count
#' @inherit dplyr::count
#'
#' @importFrom tidySingleCellExperiment count
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     count()
#' spe |>
#'     add_count()
NULL

#' @name slice
#' @rdname slice
#' @aliases slice_head slice_tail 
#'   slice_sample slice_min slice_max
#' @inherit dplyr::slice
#' @family single table verbs
#'
#' @importFrom tidySingleCellExperiment slice
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'    slice(1)
NULL

#' @name distinct
#' @rdname distinct
#' @inherit dplyr::distinct
#'
#' @importFrom tidySingleCellExperiment distinct
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'    distinct(sample_id)
NULL

#' @name pull
#' @rdname pull
#' @inherit dplyr::pull
#'
#' @importFrom tidySingleCellExperiment pull
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     pull(in_tissue)
NULL
