\name{projplot}
\alias{projplot}
\title{Hue projection plot}
\usage{
  projplot(tcsres, ...)
}
\arguments{
  \item{tcres}{Color space coordinates, possibly a result
  from the \code{tcs} function, containing values for the
  'x', 'y' and 'z' coordinates as columns (labeled as
  such).}

  \item{...}{additonal parameters to be passed to the
  plotting of data points}
}
\value{
  \code{projplot} creates a 2D plot of color points
  projected from the tetrahedron to its encapsulating
  sphere, and is ideal to visualize differences in hue.
}
\description{
  Produces a projection plot of points in a color space
}
\note{
  \code{projplot} uses the Mollweide projection, and not
  the Robinson projection, which has been used in the past.
  Among other advantages, the Mollweide projection
  preserves area relationships within latitudes without
  distortion.
}
\examples{
\dontrun{
data(sicalis)
vis.sicalis <- vismodel(sicalis, visual='avg.uv')
tcs.sicalis <- tcs(vis.sicalis, by=rep(c('C','T','B'),7))
projplot(tcs.sicalis) }
}
\author{
  Rafael Maia \email{rm72@zips.uakron.edu}
}
\references{
  Stoddard, M. C., & Prum, R. O. (2008). Evolution of avian
  plumage color in a tetrahedral color space: A
  phylogenetic analysis of new world buntings. The American
  Naturalist, 171(6), 755-776.

  Endler, J. A., Westcott, D., Madden, J., & Robson, T.
  (2005). Animal visual systems and the evolution of color
  patterns: Sensory processing illuminates signal
  evolution. Evolution, 59(8), 1795-1818.
}
