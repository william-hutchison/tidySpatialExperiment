#' @name unnest
#' @rdname unnest
#' @inherit tidyr::unnest
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     nest(data = -sample_id) |>
#'     unnest(data)
#'
#' @importFrom tidyr unnest
#' @export
unnest.tidySpatialExperiment_nested <- function(data, cols, ..., keep_empty=FALSE, ptype=NULL,
                                                   names_sep=NULL, names_repair="check_unique", .drop, .id, .sep, .preserve) {

  cols <- enquo(cols)

  unnest_single_cell_experiment(data, !!cols, ..., keep_empty=keep_empty, ptype=ptype,
                                        names_sep=names_sep, names_repair=names_repair)
  }

#' @rdname unnest
#' @importFrom tidyr unnest
#' @importFrom rlang quo_name 
#' @importFrom rlang enquo 
#' @importFrom purrr reduce
#' @importFrom purrr when
#' @importFrom purrr imap
#' @export
unnest_single_cell_experiment  <-  function(data, cols, ..., keep_empty=FALSE, ptype=NULL,
                                            names_sep=NULL, names_repair="check_unique", .drop, .id, .sep, .preserve) {
    # Need this otherwise crashes map
    .data_ <- data
    cols <- enquo(cols)
    
    # Bind nested SpatialExperiment objects
    if(
        .data_ %>% 
            pull(!!cols) %>%
            .[[1]] %>%
            is("SpatialExperiment") %>%
            any()
    ) {
        .data_ %>%
            pull(!!cols) %>%
            reduce(bind_rows)
      
    # Otherwise perform a normal unnest
    } else {
        .data_ %>%
            drop_class("tidySpatialExperiment_nested") %>%
            tidyr::unnest(!!cols, ..., keep_empty=keep_empty, ptype=ptype, names_sep=names_sep, names_repair=names_repair) %>%
            add_class("tidySpatialExperiment_nested")
    }
}

#' @name nest
#' @rdname nest
#' @inherit tidyr::nest
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     nest(data = -sample_id)
#'     
#' @importFrom tidyr nest
#' @importFrom rlang enquos
#' @importFrom rlang :=
#' @export
nest.SpatialExperiment <- function(.data, ..., .names_sep = NULL) {
    cols <- enquos(...)
    col_name_data <- names(cols)

    # Deprecation of special column names
    if(is_sample_feature_deprecated_used(
      .data,
      (enquos(..., .ignore_empty = "all") %>% map(~ quo_name(.x)) %>% unlist)
    )){
      .data= ping_old_special_column_into_metadata(.data)
    }

    my_data__ = .data
    
    my_data__ %>%
      as_tibble() %>%
      
      # Add index column to allow tracking of nested cells
      rowid_to_column("index") %>%
      tidyr::nest(...) |>
      
      # Use index to subset cells from original SpatialExperiment object
      mutate(!!sym(col_name_data) := map(!!sym(col_name_data), ~ .x %>% pull(index))) %>%
      mutate(!!sym(col_name_data) := map(!!sym(col_name_data), ~ my_data__[, .x])) %>% 
  
      # Coerce to tidySpatialExperiment_nested for unnesting
      add_class("tidySpatialExperiment_nested")
}

#' @name extract
#' @rdname extract
#' @inherit tidyr::extract
#' 
#' @examples
#' spe |> 
#'     extract(col = array_row, into = "A", regex = "([[:digit:]]3)")
#'     
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment colData<-
#' @importFrom tidyr extract
#' @export
extract.SpatialExperiment <- function(data, col, into, regex="([[:alnum:]]+)", remove=TRUE,
    convert=FALSE, ...) {
    col <- enquo(col)

    # Deprecation of special column names
    if(is_sample_feature_deprecated_used(
      data,
      c(quo_name(col), into)
    )){
      data= ping_old_special_column_into_metadata(data)
    }

    colData(data) <-
        data %>%
        as_tibble() %>%
        tidyr::extract(col=!!col, into=into, regex=regex, remove=remove, convert=convert, ...) %>%
        as_meta_data(data)


    data
}

#' @name pivot_longer
#' @rdname pivot_longer
#' @inherit tidyr::pivot_longer
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     pivot_longer(c(array_row, array_col), names_to = "dimension", values_to = "location")
#'
#' @export
NULL

#' @name unite
#' @rdname unite
#' @inherit tidyr::unite
#' 
#' @examples
#' example(read10xVisium)
#' spe |>
#'     unite("A", array_row:array_col)
#' 
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment colData<-
#' @export
unite.SpatialExperiment <- function(data, col, ..., sep="_", remove=TRUE, na.rm=FALSE) {

    # Check that we are not modifying a key column
    cols <- enquo(col)

    # Deprecation of special column names
    if(is_sample_feature_deprecated_used(
      data,
      (enquos(..., .ignore_empty = "all") %>% map(~ quo_name(.x)) %>% unlist)
    )){
      data= ping_old_special_column_into_metadata(data)
    }

    tst <-
        intersect(
            cols %>% quo_names(),
            get_special_columns(data) %>% c(get_needed_columns(data))
        ) %>%
        length() %>%
        gt(0) &
        remove

    if (tst) {
        columns =
            get_special_columns(data) %>%
            c(get_needed_columns(data)) %>%
            paste(collapse=", ")
        stop(
            "tidySpatialExperiment says: you are trying to rename a column that is view only",
            columns, " ",
            "(it is not present in the colData). If you want to mutate a view-only column, make a copy and mutate that one."
        )
    }

    colData(data) <- data %>%
        as_tibble() %>%
        tidyr::unite(!!cols, ..., sep=sep, remove=remove, na.rm=na.rm) %>%
        as_meta_data(data)

    data
}

#' @name separate
#' @rdname separate
#' @inherit tidyr::separate
#'
#' @examples
#' example(read10xVisium)
#' spe |>
#'     separate(col = sample_id, into = c("A", "B"), sep = "[[:alnum:]]n")
#'     
#' @importFrom tidyr separate
#' @importFrom SummarizedExperiment colData
#' @importFrom SummarizedExperiment colData<-
#' @export
separate.SpatialExperiment <- function(data, col, into, sep="[^[:alnum:]]+", remove=TRUE,
    convert=FALSE, extra="warn", fill="warn", ...) {

    # Check that we are not modifying a key column
    cols <- enquo(col)

    # Deprecation of special column names
    if(is_sample_feature_deprecated_used(
      data,
      c(quo_names(cols))
    )){
      data= ping_old_special_column_into_metadata(data)
    }

    tst <-
        intersect(
            cols %>% quo_names(),
            get_special_columns(data) %>% c(get_needed_columns(data))
        ) %>%
        length() %>%
        gt(0) &
        remove

    if (tst) {
        columns =
            get_special_columns(data) %>%
            c(get_needed_columns(data)) %>%
            paste(collapse=", ")
        stop(
            "tidySpatialExperiment says: you are trying to rename a column that is view only",
            columns, " ",
            "(it is not present in the colData). If you want to mutate a view-only column, make a copy and mutate that one."
        )
    }

    colData(data) <-
        data %>%
        as_tibble() %>%
        tidyr::separate(!!cols, into=into, sep=sep, remove=remove, convert=convert, extra=extra, fill=fill, ...) %>%
        as_meta_data(data)

    data
}
