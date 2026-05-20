#' @name bind_rows
#' @rdname bind_rows
#' @inherit ttservice::bind_rows
#' 
#' @examples
#' # Note: "dplyr" does not provide a generic function for `bind_rows`. Therefore, the generic 
#' # function `bind_rows` located in "ttservice" should be called explicitly with 
#' # `ttservice::bind_rows` to avoid conflicts.
#' 
#' example(read10xVisium)
#' spe |>
#'     ttservice::bind_rows(spe)
NULL

#' @name bind_cols
#' @rdname bind_cols
#' @inherit ttservice::bind_cols
#' 
#' @examples 
#' # Note: "dplyr" does not provide a generic function for `bind_cols`. Therefore, the generic 
#' # function `bind_cols` located in "ttservice" should be called explicitly with 
#' # `ttservice::bind_cols` to avoid conflicts.
#' 
#' example(read10xVisium)
#' spe |>
#'     ttservice::bind_cols(1:99)
NULL

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
        (enquos(..., .ignore_empty = "all") |> map(~ quo_name(.x)) |> unlist())
    )) {
        .data <- 
            ping_old_special_column_into_metadata(.data)
    }
    
    new_meta <- 
        .data |>
        as_tibble() |>
        rowid_to_column("index") |>
        
        dplyr::filter(..., .preserve = .preserve) |> 
        as_meta_data(.data)
      
    # Try to solve missing colnames
    if (colnames(.data) |> is.null()) {
        message(
            "tidySpatialExperiment says: the input object does not have cell names (colnames(...)).
            Therefore, the cell column in the filtered tibble abstraction will still include an 
            incremental integer vector."
        )
        new_meta <- 
            new_meta |> 
            mutate(!!c_(.data)$symbol := as.integer(!!c_(.data)$symbol))
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
    cols <- 
        enquos(...) |> 
        names()
    
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(
        .data,
        (enquos(..., .ignore_empty = "all") |> map(~ quo_name(.x)) |> unlist())
    )) {
        .data <- ping_old_special_column_into_metadata(.data)
    }
    
    tst <-
        intersect(
            cols |>
                names(),
            get_special_columns(.data) |>
                c(get_needed_columns(.data))
        ) |>
        length() |>
        gt(0)
    
    if (tst) {
        columns <-
            get_special_columns(.data) |>
            c(get_needed_columns()) |>
            paste(collapse = ", ")
        stop(
            "tidySpatialExperiment says: you are trying to rename a column that is view only",
            columns, " ",
            "(it is not present in the colData). If you want to mutate a view-only column, make a 
            copy and mutate that one."
        )
    }
    
    # Extract colData for mutation and save to SpatialExperiment
    colData(.data) <-
        .data |>
        colData() |>
        as_tibble(rownames = c_(.data)$name) |>
        dplyr::mutate(...) |>
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
#' @importFrom tibble as_tibble
#' @importFrom tibble add_column
#' @importFrom dplyr left_join
#' @importFrom dplyr count
#' @export
left_join.SpatialExperiment <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                                        ...) {
    
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(x, when(by, !is.null(.) ~ by, ~ colnames(y)))) {
        x <- ping_old_special_column_into_metadata(x)
    }

    # Convert y colData to tibble format, return error message or continue with supplied tibble
    if (inherits(y, "SingleCellExperiment") | inherits(y, "SpatialExperiment")) {
        y <-
            y |>
            colData() |>
            tibble::as_tibble(rownames = c_(y)$name)
    }
    if (! inherits(y, "tbl_df")) {
        stop(
            "tidySpatialExperiment says: `y` must be a tibble, a SpatialExperiment object or a 
            SingleCellExperiment object."
        )
    }

    # Join data and assign to the returned SpatialExperiment object's colData
    colData(x) <-
        x |>
        colData() |>
        tibble::as_tibble() |>
        tibble::add_column(
            .cell = rownames(colData(x)),
            .before = 1
        ) |>
        dplyr::left_join(y, by = by, copy = copy, suffix = suffix, ...) |>
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
#' @importFrom tibble as_tibble
#' @importFrom tibble add_column
#' @importFrom dplyr left_join
#' @importFrom dplyr pull
#' @export
inner_join.SpatialExperiment <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
                                         ...) {
  
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(x, when(by, !is.null(.) ~ by, ~ colnames(y))) ) {
        x <- ping_old_special_column_into_metadata(x)
    }        

    # Convert y colData to tibble format, return error message or continue with supplied tibble
    if (inherits(y, "SingleCellExperiment") | inherits(y, "SpatialExperiment")) {
        y <-
            y |>
            colData() |>
            tibble::as_tibble(rownames = c_(y)$name)
    }
    if (! inherits(y, "tbl_df")) {
        stop(
            "tidySpatialExperiment says: `y` must be a tibble, a SpatialExperiment object or a 
            SingleCellExperiment object."
        )
    }

    # Filter x to overlapping rows with y in by column
    x <-
        x |>
        dplyr::filter(!!sym(by) %in% y[[by]])

    # Join data and assign to the returned SpatialExperiment object's colData
    colData(x) <-
        x |>
        colData() |>
        tibble::as_tibble() |>
        tibble::add_column(
            .cell = rownames(colData(x)),
            .before = 1
        ) |>
        dplyr::left_join(y, by = by, copy = copy, suffix = suffix, ...) |>
        as_meta_data(x)
    x
}

#' @name right_join
#' @rdname right_join
#' @inherit dplyr::right_join
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     right_join(
#'         spe |>
#'             filter(in_tissue == TRUE) |>
#'             mutate(new_column = 1)
#'         )
#'
#' @importFrom SummarizedExperiment colData
#' @importFrom tibble as_tibble
#' @importFrom tibble add_column
#' @importFrom dplyr left_join
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @export
right_join.SpatialExperiment <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),
                                         ...) {
  
    # Deprecation of special column names
    if (is_sample_feature_deprecated_used(x, when(by, !is.null(.) ~ by, ~ colnames(y))) ) {
        x <- ping_old_special_column_into_metadata(x)
    }        

    # Convert y colData to tibble format, return error message or continue with supplied tibble
    if (inherits(y, "SingleCellExperiment") | inherits(y, "SpatialExperiment")) {
        y <-
            y |>
            colData() |>
            tibble::as_tibble(rownames = c_(y)$name)
    }
    if (! inherits(y, "tbl_df")) {
        stop(
            "tidySpatialExperiment says: `y` must be a tibble, a SpatialExperiment object or a 
            SingleCellExperiment object."
        )
    }

    # Filter x to overlapping rows with y in by column
    x <-
        x |>
        dplyr::filter(!!sym(by) %in% y[[by]])

    # Join data and assign to the returned SpatialExperiment object's colData
    colData(x) <-
        x |>
        colData() |>
        tibble::as_tibble() |>
        tibble::add_column(
            .cell = rownames(colData(x)),
            .before = 1
        ) |>
        dplyr::left_join(y, by = by, copy = copy, suffix = suffix, ...) |>
        as_meta_data(x)
    x
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
        (enquos(..., .ignore_empty = "all") |> map(~ quo_name(.x)) |> unlist())
    )) {
        .data <- ping_old_special_column_into_metadata(.data)
    }
    
    .data |>
        colData() |>
        tibble::as_tibble(rownames = c_(.data)$name) |>
        select_helper(...) |>
        when(
            # If key columns are missing
            (get_needed_columns(.data) %in% colnames(.)) |>
                all() |>
                lapply(`!`) |>
                unlist() ~ {
                    message(
                        "tidySpatialExperiment says: Key columns are missing. A data frame is 
                        returned for independent data analysis."
                    )
                    (.)
            },
        
            # If valid SpatialExperiment meta data
            ~ {
                colData(.data) <- (.) |> as_meta_data(.data)
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
sample_n.SpatialExperiment <- function(tbl, size, replace = FALSE, weight = NULL, .env = NULL, 
                                       ...) {
    
    lifecycle::signal_superseded("1.0.0", "sample_n()", "slice_sample()")
    
    new_meta <-
        tbl |>
        as_tibble() |>
        dplyr::sample_n(size, replace = replace, weight = weight, .env = .env, ...)
    
    count_cells <- new_meta |> select(!!c_(tbl)$symbol) |> count(!!c_(tbl)$symbol)
    
    # If repeated cells
    if (count_cells$n |> max() |> gt(1)) {
        message("tidySpatialExperiment says: When sampling with replacement a data frame is returned for independent data analysis.")
        new_meta
    } else {
        new_obj <- tbl[,  new_meta |> pull(!!c_(tbl)$symbol)]
        new_obj
    }
}

#' @name sample_frac
#' @rdname sample_n
#' @importFrom SummarizedExperiment colData
#' @importFrom dplyr sample_frac
#' 
#' @export
sample_frac.SpatialExperiment <- function(tbl, size = 1, replace = FALSE, weight = NULL, 
                                          .env = NULL, ...) {
    
    lifecycle::signal_superseded("1.0.0", "sample_frac()", "slice_sample()")
    
    new_meta <- tbl |>
        as_tibble() |>
        dplyr::sample_frac(size, replace = replace, weight = weight, .env = .env, ...)
    
    count_cells <- new_meta |> select(!!c_(tbl)$symbol) |> count(!!c_(tbl)$symbol)
    
    # If repeted cells
    if (count_cells$n |> max() |> gt(1) ) {
        message(
            "tidySpatialExperiment says: When sampling with replacement a data frame is returned 
            for independent data analysis."
        )
        tbl |>
            as_tibble() |>
            right_join(new_meta |> select(!!c_(tbl)$symbol),  by = c_(tbl)$name)
    } else {
        new_obj <- tbl[,  new_meta |> pull(!!c_(tbl)$symbol)]
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
        (enquos(..., .ignore_empty = "all") |> map(~ quo_name(.x)) |> unlist())
    )) {
        x <- ping_old_special_column_into_metadata(x)
    }
    
    colData(x) <- 
        x |>
        colData() |>
        tibble::as_tibble(rownames = c_(x)$name) |> 
        dplyr::add_count(..., wt = !!enquo(wt), sort = sort, name = name) |>
        as_meta_data(x)
    
    x
}

#' @name group_by
#' @rdname group_by
#' @inherit dplyr::group_by
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
#' @importFrom dplyr arrange
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
#' @examples
#' example(read10xVisium)
#' spe |>
#'     rename(in_liver = in_tissue)
NULL

#' @name rowwise
#' @rdname rowwise
#' @inherit dplyr::rowwise
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
#' @examples
#' example(read10xVisium)
#' spe |>
#'    slice(1)
NULL

#' @name distinct
#' @rdname distinct
#' @inherit dplyr::distinct
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
#' @examples
#' example(read10xVisium)
#' spe |>
#'     pull(in_tissue)
NULL
