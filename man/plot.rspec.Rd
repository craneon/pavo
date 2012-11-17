\name{plot.rspec}
\alias{plot.rspec}
\title{Plot spectra}
\usage{
  plot.rspec(specs, select = NULL,
    type = c("overlay", "stack", "heatmap"), cols = 2,
    varying = NULL, n = 100, col = "black", xlim = NULL,
    ylim = NULL, ...)
}
\arguments{
  \item{specs}{(required) an \code{rspec} object containing
  spectra to plot}

  \item{select}{specification of which spectra to plot. Can
  be a numeric vector or factor (e.g., \code{sex=='male'})}

  \item{type}{what type of plot should be drawn.
  Possibilities are: \itemize{ \item \code{overlay}
  (default) for plotting multiple spectra in a single panel
  with a common y-axis \item \code{stack} for plotting
  multiple spectra in a vertical arrangement \item
  \code{heatmap} for plotting reflectance values by
  wavelength and a third variable (\code{varying}) }}

  \item{varying}{a numeric vector giving values for third
  variable used in \code{heatplot}}

  \item{n}{number of bins with which to interpolate colors
  and \code{varying} for the heatplot.}

  \item{col}{color of the spec curves. User can either
  provide a single color, a vector of colors, or a
  \code{spec2rgb} object. For the latter, picking spectra
  with \code{select} will also subset the color vector.}

  \item{xlim}{a numeric vector giving the lower an upper
  limits for the x-axis}

  \item{ylim}{a numeric vector giving the lower an upper
  limits for the y-axis}

  \item{...}{additional arguments passed to plot (or image
  for \code{'heatmap'}).}
}
\description{
  Plots reflectance spectra in different arrangements.
}
\examples{
\dontrun{
#INCLUDE EXAMPLE}
}
\author{
  Chad Eliason \email{cme16@zips.uakron.edu}
}
\seealso{
  \code{\link{spec2rgb}}, \code{\link{image}},
  \code{\link{plot}}
}
