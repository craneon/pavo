#' Subset rspec, tcs and vismodel objects
#'
#' Subsets various object types based on a given vector or grep partial matching of data names
#'
#' @method subset rspec
#'
#' @param x (required) an object of class \code{rspec}, \code{tcs} or \code{vismodel}
#' containing spectra, visual model output or colourspace data to subset
#' @param ... class consistency (ignored).
#' @param subset a string used for partial matching of observations
#' @return a subsetted object of the same class as the input object
#'
#' @examples \dontrun{
#'
#' # Load the 'sicalis' dataset 
#' data(sicalis)
#' # Generate a visual model
#' vm1 <- vismodel(sicalis)
#' # Make a tetracolorspace
#' tcs1 <- tcs(vm1)
#' # Subset all 'crown' patches (C in file names)
#' head(subset(sicalis, "B"))
#' subset(vm1, "B")
#' subset(tcs1, "B")[, 1:5]}
#'
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @export subset.rspec

subset.rspec <- function (x, subset, ...) {
  # remove 'wl' column if present
  wl_index <- which(names(x)=='wl')
  if (length(wl_index)==1) {
    wl <- x[, wl_index]
    x <- x[, -wl_index]
  }
  if (is.logical(subset)) {
    subsample <- subset
  } else {
      subsample <- grep(subset, names(x))
    }
  if (length(subsample)==0) {
    warning("Subset condition not found")
  }
  res <- cbind(wl, x[subsample]) # & !is.na(subset)])
  class(res) <- c("rspec", "data.frame")
  res
}

#' @method subset tcs
#' @rdname subset.rspec
#'
subset.tcs <- function (x, subset, ...) {
  # if (!is.logical(subset)) 
  #   stop("'subset' must be logical")
  if (is.logical(subset)) {
    subsample <- subset
  } else {
      subsample <- grep(subset, row.names(x))
    }
  if (length(subsample)==0) {
    warning("Subset condition not found")
  }
  res <- x[subsample, ] # & !is.na(subset)])
  class(res) <- c("tcs", "data.frame")
  res
}

#' @method subset vismodel
#' @rdname subset.rspec
#'
subset.vismodel <- function (x, subset, ...) {
  if (is.logical(subset)) {
    subsample <- subset
  } else {
      subsample <- grep(subset, row.names(x))
    }
  # attr <- attributes(x)
  res <- x[subsample, ]
  res
}
