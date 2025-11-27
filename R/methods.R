# Define otherwise undefined global variables for R CMD check
utils::globalVariables(c("x", "y", "dimension_x", "dimension_y", "pxl_col_in_fullres", 
                         "pxl_row_in_fullres", ".key", "tidygate_env"))

#' Show
#'
#' @importFrom methods getMethod
#'
#' @keywords internal
#' @noRd
setMethod(
    f = "show",
    signature = "SpatialExperiment",
    definition = function(object) {
        if (
            isTRUE(x = getOption(x = "restore_SpatialExperiment_show", default = FALSE))
        ) {
            f <- methods::getMethod(
                f = "show",
                signature = "SummarizedExperiment",
                where = asNamespace(ns = "SummarizedExperiment")
            )
            f(object = object)

        } else {  
            print(object)
        }
    }
)

setClass("tidySpatialExperiment", contains = "SpatialExperiment")

#' Extract and join information for features.
#'
#' @description join_features() extracts and joins information for specified features
#'
#' @importFrom ttservice join_features
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
#' example(read10xVisium)
#' spe |>
#'     join_features(features = "ENSMUSG00000025900")
NULL

#' join_features
#'
#' @keywords internal
#' @noRd
setMethod("join_features", "SpatialExperiment", function(.data, features = NULL, all = FALSE,
                                                        exclude_zeros = FALSE, shape = "long", 
                                                        ...) {
      
      # Define unbound variable
      index <- NULL

      # Shape is long
      if (shape == "long") {
          
          # Print message about changing data type
          message(
              "tidySpatialExperiment says: A data frame is returned for independent data 
              analysis."
          )
        
          # Join feature abundance with colData by index
          .data |>
              colData() |>
              tibble::as_tibble(rownames = c_(.data)$name) |>
              tibble::rowid_to_column("index") |>
              dplyr::mutate(index = as.character(index)) |>
              dplyr::left_join(
                  get_abundance_sc_long(
                      .data = .data,
                      features = features,
                      all = all,
                      exclude_zeros = exclude_zeros
                  ),
                  by = "index"
              ) |>
              dplyr::mutate("index" = NULL)
      
      # Shape is wide
      } else {
        colData(.data) <- 
          .data |>
              colData() |>
              tibble::as_tibble(rownames = c_(.data)$name) |>
              tibble::rowid_to_column("index") |>
              dplyr::mutate(index = as.character(index)) |>
              left_join(
                  get_abundance_sc_wide(
                      .data = .data,
                      features = features,
                      all = all, ...
                  ),
                  by = "index") |>
              dplyr::mutate("index" = NULL) |>
              as_meta_data(.data)
          .data
      } 
})

#' Aggregate cells
#'
#' @description Combine cells into groups based on shared variables and aggregate feature counts.
#'
#' @importFrom rlang enquo
#' @importFrom tibble enframe
#' @importFrom Matrix rowSums
#' @importFrom dplyr full_join
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
#' example(read10xVisium)
#' spe |>
#'     aggregate_cells(sample_id, assays = "counts")
#'
#' @export
aggregate_cells <- function(.data, .sample = NULL, slot = "data", assays = NULL, 
                            aggregation_function = rowSums) {
  
    # Declare unbound variables
    feature <- NULL
    .sample <- enquo(.sample)
    
    # Subset only wanted assays
    if (!is.null(assays)) {
        .data@assays@data <- .data@assays@data[assays]
    }
    
    .data |>
      
        nest(data = -!!.sample) |>
        mutate(.aggregated_cells = as.integer(map(data, ~ ncol(.x)))) |>
        mutate(data = map(data, ~ 
            
            # loop over assays
            map2(
                as.list(assays(.x)), names(.x@assays),
                
                # Get counts
                ~  .x |>
                  aggregation_function(na.rm = TRUE) |>
                  enframe(
                      name  = "feature",
                      value = sprintf("%s", .y)
                  ) |>
                  mutate(feature = as.character(feature)) 
            ) |>
            reduce(function(...) dplyr::full_join(..., by = c("feature")))

        )) |>
        left_join(
            .data |> 
            as_tibble() |> 
            subset(!!.sample),
        by = quo_names(.sample)
        ) |>
        unnest(data) |>
    
    drop_class("tidySpatialExperiment_nested") |> 
    
    as_SummarizedExperiment(
        .sample = !!.sample, .transcript = feature, .abundance = !!as.symbol(names(.data@assays))
    )
}

#' Rectangle Gating Function
#'
#' @description Determines whether points specified by spatial coordinates are within a defined rectangle.
#'
#' @importFrom dplyr mutate
#'
#' @name rectangle
#' @rdname rectangle
#' 
#' @param spatial_coord1 Numeric vector for x-coordinates (e.g., array_col)
#' @param spatial_coord2 Numeric vector for y-coordinates (e.g., array_row)
#' @param center Numeric vector of length 2 specifying the center of the rectangle (x, y)
#' @param height The height of the rectangle
#' @param width The width of the rectangle
#' 
#' @return Logical vector indicating points within the rectangle
#' 
#' @examples 
#' example(read10xVisium)
#' spe |>
#'     mutate(in_rectangle = rectangle(
#'       array_col, array_row, center = c(50, 50), height = 20, width = 10)
#'       )
#'
#' @export
rectangle <- function(spatial_coord1, spatial_coord2, center, height, width) {
    x_min <- center[1] - width / 2
    x_max <- center[1] + width / 2
    y_min <- center[2] - height / 2
    y_max <- center[2] + height / 2

    within_x <- spatial_coord1 >= x_min & spatial_coord1 <= x_max
    within_y <- spatial_coord2 >= y_min & spatial_coord2 <= y_max

    return(within_x & within_y)
}

#' Ellipse Gating Function
#'
#' @name ellipse
#' @rdname ellipse
#' @description Function to create an ellipse gate in a SpatialExperiment object
#' @param spatial_coord1 Numeric vector for x-coordinates
#' @param spatial_coord2 Numeric vector for y-coordinates
#' @param center Numeric vector (length 2) for ellipse center (x, y)
#' @param axes_lengths Numeric vector (length 2) for the lengths of the major and minor axes of the
#' ellipse
#' @return Logical vector indicating points within the ellipse
#' @examples
#' example(read10xVisium)
#' spe |>
#'     mutate(in_ellipse = ellipse(
#'         array_col, array_row, center = c(50, 50), axes_lengths = c(20, 10))
#'     )
#'     
#' @export
ellipse <- function(spatial_coord1, spatial_coord2, center, axes_lengths) {
    # axes_lengths should be a vector of length 2: [major_axis, minor_axis]

    # Scaling factor to normalize the ellipse to a unit circle
    scale_x <- 1 / axes_lengths[1]
    scale_y <- 1 / axes_lengths[2]

    # Normalized coordinates relative to ellipse center
    normalized_x <- (spatial_coord1 - center[1]) * scale_x
    normalized_y <- (spatial_coord2 - center[2]) * scale_y

    # Check if points are within the unit circle (ellipse after normalization)
    within_ellipse <- (normalized_x^2 + normalized_y^2) <= 1

    return(within_ellipse)
}

#' Gate interactive 
#' 
#' Interactively gate points by their location in space, with image data overlaid. 
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom magick image_read
#' @importFrom magick image_info
#' @importFrom xfun base64_uri
#' @importFrom plotly raster2uri
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom SpatialExperiment imgData
#' @importFrom SpatialExperiment imgRaster
#' @importFrom SpatialExperiment imgSource
#' @importFrom shiny shinyApp
#' @importFrom shiny runApp
#' @importFrom tidygate ui
#' @importFrom tidygate server
#' @param spe A SpatialExperiment object.
#' @param image_index The image to display if multiple are stored within the provided
#' SpatialExperiment object. 
#' @param colour A single colour string compatible with ggplot2. Or, a vector representing the 
#' point colour.
#' @param shape A single ggplot2 shape numeric ranging from 0 to 127. Or, a vector representing the 
#' point shape, coercible to a factor of 6 or less levels.
#' @param alpha A single ggplot2 alpha numeric ranging from 0 to 1.
#' @param size A single ggplot2 size numeric ranging from 0 to 20.
#' @param hide_points A logical. If TRUE, points are hidden during interactive gating, improving 
#' performance for large SpatialExperiment objects. 
#' @param rasterise_points A logical. If TRUE, points are rasterised to an image before interactive 
#' gating is launched, improving performance for large datasets. Some interactive features are 
#' unavailable with rasterisation enabled.
#' @return The input SpatialExperiment object with a new column `.gated`, recording the 
#' gates each X and Y coordinate pair is within. If gates are drawn interactively, they are 
#' temporarily saved to `tidygate_env$gates`
#' @examples
#' example(read10xVisium)
#' data(demo_brush_data, package = "tidySpatialExperiment")
#' 
#' if(interactive()) {
#'     spe |>
#'         gate(colour = "blue", shape = "in_tissue")
#' }
gate_interactive <-
  
  function(spe, image_index, colour, shape, alpha, size, hide_points, rasterise_points) {
    
    available_columns <-
      spe |>
      colData() |>
      colnames()

    # Create tibble with necessary information
    data <- 
      tibble::tibble(
        x = 
          spe |>
          pull(pxl_col_in_fullres), 
        y = 
          spe |>
          pull(pxl_row_in_fullres)
      ) |>
      dplyr::mutate(.key = dplyr::row_number())
    
    # Optionally add colour and shape columns if provided
    if (!is.null(colour)) {
      if(colour %in% available_columns) {
        data <-
          data |>
          dplyr::mutate(colour = 
                          spe |> 
                          dplyr::pull(sym(colour))
          )
      }
    }
    if (!is.null(shape)) {
      if(shape %in% available_columns) {
        data <-
          data |>
          dplyr::mutate(shape = 
                          spe |> 
                          dplyr::pull(sym(shape)) |>
                          factor()
          )
        }
    }

    # Prepare spatial information
    image_info <- 
      SpatialExperiment::imgData(spe)[image_index, ]@listData$data[[1]] |>
      SpatialExperiment::imgRaster() |>
      magick::image_read() |>
      magick::image_info()
    
    image_x_size <- 
      image_info$width /
      SpatialExperiment::imgData(spe)[image_index, ]@listData$scaleFactor
    
    image_y_size <- 
      image_info$height /
      SpatialExperiment::imgData(spe)[image_index, ]@listData$scaleFactor
    
    image_uri <- 
      SpatialExperiment::imgData(spe)[image_index, ]@listData$data[[1]] |>
      SpatialExperiment::imgRaster() |>
      plotly::raster2uri()

    # Create plot
    plot <-
      data |>
      ggplot2::ggplot(ggplot2::aes(x = x, y = y, key = .key)) +
      ggplot2::scale_y_reverse() + # Reverse y axis for image coordinate format
      ggplot2::coord_fixed(
        xlim = c(0, image_x_size), 
        ylim = c(0, image_y_size), 
        expand = FALSE,
        ratio = 1
      )

    # Add points to plot if not hidden
    if (hide_points == FALSE) {
      plot <-
        plot +
        ggplot2::geom_point(alpha = alpha, size = size)
    }

    # Add colour 
    # Set colour to equal column value if provided
    if (!is.null(colour)) {
      if(colour %in% available_columns) {
          plot <- 
            plot + 
            ggplot2::aes(colour = colour)
          
      # Set to equal constant if not a column symbol and remove legend
      } else { 
        plot <- 
          plot + 
          ggplot2::aes(colour = colour) +
          ggplot2::scale_colour_manual(values = colour) +
          ggplot2::guides(colour = "none")
      }
    }
    
    # Add shape 
    if (!is.null(shape)) {
      if(shape %in% available_columns) {
        plot <- 
          plot + 
          ggplot2::aes(shape = as.factor(shape))
      } else {
        plot <- 
          plot + 
          ggplot2::aes(shape = "fixed_shape") +
          ggplot2::scale_shape_manual(values = shape) +
          ggplot2::guides(shape = "none")
      }
    }
    
    # Add alpha
    if (!is.null(alpha)) {
      plot <-
        plot +
        ggplot2::aes(alpha = "fixed_alpha") +
        ggplot2::scale_alpha_manual(values = alpha) +
        ggplot2::guides(alpha = "none")
    }

    # Add size
    if (!is.null(size)) {
      plot <-
        plot +
        ggplot2::aes(size = "fixed_size") +
        ggplot2::scale_size_manual(values = size) +
        ggplot2::guides(size = "none")
    }

  # Create rasterised plot and convert to plotly
  if (rasterise_points == TRUE) {

    # Create version of plot with no borders, axis, margins or legends
    plot_panel <-
      plot +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0), 
        legend.position = "none"
      )

      # Save plot as image
      temp_file <-
        tempfile(fileext = ".png")

      temp_file |>
        ggplot2::ggsave(plot = plot_panel, width = 5, height = 5, dpi = 300)

      # Create plot with only borders, axis, margins and legends
      plot_border <-
        data |>
            ggplot2::ggplot(ggplot2::aes(x = x, y = y, key = .key)) +
            ggplot2::scale_y_reverse() + # Reverse y axis for image coordinate format
            ggplot2::coord_fixed(
            xlim = c(0, image_x_size), 
            ylim = c(0, image_y_size), 
            expand = FALSE,
            ratio = 1
          )  +
        ggplot2::geom_point(alpha = 0) +
        ggplot2::theme_bw()

      plot <-
        plot_border |>
        plotly::ggplotly(tooltip = NULL) |>
        plotly::layout(images = list(
          list(
            source = xfun::base64_uri(temp_file),
            xref = "x",
            yref = "y",
            x = 0,
            y = 0, 
            sizex = image_x_size, 
            sizey = image_y_size,
            yanchor = "top",
            sizing = "stretch",
            layer = "above"
          ),
          list(
            source = image_uri,
            xref = "x",
            yref = "y",
            x = 0,
            y = 0,
            sizex = image_x_size,
            sizey = image_y_size,
            yanchor = "top",
            sizing = "stretch",
            layer = "below"
          )
          ),
        dragmode = "lasso"
        ) |>
        # Prevent any actions which could disalign points and plot
        plotly::config(modeBarButtonsToRemove = c("zoom2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian", "select2d"))
    
    # Convert ggplot directly to plotly
    } else {

      # Convert to plotly and add background image
      plot <-
        plot |>
        plotly::ggplotly(tooltip = NULL) |>
        plotly::layout(images = list(
          list(
            source = image_uri,
            xref = "x",
            yref = "y",
            x = 0,
            y = 0,
            sizex = image_x_size,
            sizey = image_y_size,
            yanchor = "top",
            sizing = "stretch",
            layer = "below"
          )),
        dragmode = "lasso"
        ) |>
        plotly::config(modeBarButtonsToRemove = c("hoverClosestCartesian", "hoverCompareCartesian", "select2d"))
    }

    # Create environment and save input variables
    tidygate_env <<- rlang::env()
    tidygate_env$input_data <- data
    tidygate_env$input_plot <- plot
    tidygate_env$event_count <- 1

    # Launch Shiny App
    app <- shiny::shinyApp(tidygate::ui, tidygate::server)
    gated_vector <-
      shiny::runApp(app, port = 1234) |>
      purrr::map_chr(~ .x |> paste(collapse = ",")) |>
      purrr::map_chr(~ ifelse(.x == "", NA, .x))
    
    # Set to NULL if no gates drawn
    if (nrow(tidygate_env$gates) == 0) {
      tidygate_env$gates <- NULL
    } else {
      message("tidySpatialExperiment says: interactively drawn gates are temporarily saved to tidygate_env$gates")
    }
    
    return(gated_vector)
  }

#' Gate spatial data with pre-recorded lasso selection coordinates
#'
#' A helpful way to repeat previous interactive lasso selections to enable reproducibility. 
#' Programmatic gating is based on the package [gatepoints](https://github.com/wjawaid/gatepoints)
#' by Wajid Jawaid. 
#' 
#' @importFrom tidygate gate
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @param spe A SpatialExperiment object
#' @param programmatic_gates A `data.frame` recording the gate brush data, as output by 
#' `tidygate_env$gates`. The column `x` records X coordinates, the column `y` records Y coordinates 
#' and the column `.gated` records the gate.
#' @return The input SpatialExperiment object with a new column `.gated`, recording the gates each X
#'  and Y coordinate pair is within.
#' @examples
#' example(read10xVisium)
#' data(demo_brush_data, package = "tidySpatialExperiment")
#' 
#' spe |>
#'   gate(programmatic_gates = demo_brush_data)
gate_programmatic <-
  function(spe, programmatic_gates) {
    
    # Format spatial data for tidygate
    data <- 
      tibble::tibble(
        dimension_x = 
          spe |>
          pull(pxl_col_in_fullres), 
        dimension_y = 
          spe |>
          pull(pxl_row_in_fullres)
      ) |>
        
      # Flip y axis
      dplyr::mutate(dimension_y = - dimension_y) |>
    
      # Pass data to tidygate
      dplyr::mutate(.gate_programmatic = tidygate::gate(
       x = dimension_x, y = dimension_y, programmatic_gates = programmatic_gates
      ))
      
    return(data$.gate_programmatic)
  }


#' Interactively gate cells by spatial coordinates
#' 
#' @description
#' Gate cells based on their X and Y coordinates. By default, this function launches an interactive
#' scatter plot with image data overlaid. Colour, shape, size and alpha can be defined as constant 
#' values, or can be controlled by the values of a specified column. 
#' 
#' If previously drawn gates are supplied to the `programmatic_gates` argument, cells will be gated 
#' programmatically. This feature allows the reproduction of previously drawn interactive gates.
#' Programmatic gating is based on the package gatepoints by Wajid Jawaid. 
#' 
#' @param spe A SpatialExperiment object.
#' @param image_index The image to display if multiple are stored within the provided
#' SpatialExperiment object. 
#' @param colour A single colour string compatible with ggplot2. Or, a vector representing the 
#' point colour.
#' @param shape A single ggplot2 shape numeric ranging from 0 to 127. Or, a vector representing the 
#' point shape, coercible to a factor of 6 or less levels.
#' @param alpha A single ggplot2 alpha numeric ranging from 0 to 1.
#' @param size A single ggplot2 size numeric ranging from 0 to 20.
#' @param hide_points A logical. If TRUE, points are hidden during interactive gating, improving 
#' performance for large SpatialExperiment objects. 
#' @param rasterise_points A logical. If TRUE, points are rasterised to an image before interactive 
#' gating is launched, improving performance for large datasets. Some interactive features are 
#' unavailable with rasterisation enabled.
#' @param programmatic_gates A `data.frame` of the gate brush data, as saved in 
#' `tidygate_env$gates`. The column `x` records X coordinates, the column `y` records Y coordinates 
#' and the column `.gate` records the gate number. When this argument is supplied, gates will be 
#' drawn programmatically.
#' @return A vector of strings, of the gates each X and Y coordinate pair is within. If gates are
#' drawn interactively, they are temporarily saved to `tidygate_env$gates`.
#' @examples 
#' example(read10xVisium)
#' data(demo_brush_data, package = "tidySpatialExperiment")
#' 
#' # Gate points interactively
#' if(interactive()) {
#'     spe |>
#'         gate(colour = "blue", shape = "in_tissue")
#' }
#' 
#' # Gate points programmatically
#' spe |>
#'   gate(programmatic_gates = demo_brush_data)
#' @export
gate <-
  function(spe, image_index = 1, colour = NULL, shape = NULL, alpha = 1, size = 2, 
           hide_points = FALSE, rasterise_points = FALSE, programmatic_gates = NULL) {
    
    # Launch interactive gating
    if (is.null(programmatic_gates)) {
      gated_vector <- gate_interactive(spe = spe, image_index = image_index, colour = colour, 
                                       shape = shape, alpha = alpha, size = size, 
                                       hide_points = hide_points, 
                                       rasterise_points = rasterise_points)
      
      # Then apply programmatic gating to select points if hidden
      if (hide_points == TRUE) {
        if (!is.null(tidygate_env$gates)) {
          gated_vector <- gate_programmatic(spe = spe, programmatic_gates = tidygate_env$gates)
        } else {
          gated_vector <- NA
        }
      }
    
    # Launch programmatic gating
    } else {
      gated_vector <- gate_programmatic(spe = spe, programmatic_gates = programmatic_gates)
    }
    
    spe$.gated <- gated_vector
    return(spe)
  }
