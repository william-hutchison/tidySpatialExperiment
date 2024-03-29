% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot2_methods.R
\name{ggplot}
\alias{ggplot}
\title{Create a new \code{ggplot} from a \code{tidySpatialExperiment}}
\value{
\code{ggplot}
}
\description{
\code{ggplot()} initializes a ggplot object. It can be used to
declare the input data frame for a graphic and to specify the
set of plot aesthetics intended to be common throughout all
subsequent layers unless specifically overridden.
}
\details{
\code{ggplot()} is used to construct the initial plot object,
and is almost always followed by a plus sign (\code{+}) to add
components to the plot.

There are three common patterns used to invoke \code{ggplot()}:
\itemize{
\item \verb{ggplot(data = df, mapping = aes(x, y, other aesthetics))}
\item \code{ggplot(data = df)}
\item \code{ggplot()}
}

The first pattern is recommended if all layers use the same
data and the same set of aesthetics, although this method
can also be used when adding a layer using data from another
data frame.

The second pattern specifies the default data frame to use
for the plot, but no aesthetics are defined up front. This
is useful when one data frame is used predominantly for the
plot, but the aesthetics vary from one layer to another.

The third pattern initializes a skeleton \code{ggplot} object, which
is fleshed out as layers are added. This is useful when
multiple data frames are used to produce different layers, as
is often the case in complex graphics.

The \verb{data =} and \verb{mapping =} specifications in the arguments are optional
(and are often omitted in practice), so long as the data and the mapping
values are passed into the function in the right order. In the examples
below, however, they are left in place for clarity.
}
\examples{
example(read10xVisium)
spe |>
    ggplot(ggplot2::aes(x = .cell, y = array_row)) +
    ggplot2::geom_point()
}
