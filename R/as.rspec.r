#' Convert data to an rspec object
#'
#' Converts data frames or matrices containing spectral data to \code{rspec} object
#'
#' @param specs (required) an \code{rspec} object containing spectra to process
#' @param whichwl specifies which column contains wavelengths. If NULL (default), function
#' searches for column titled "wl" or else returns arbitrary index values
#' @return an object of class \code{rspec} for use in further \code{pavo} functions
#' @export as.rspec
#' @export is.rspec
#' @examples \dontrun{
#' # Generate some fake reflectance data
#' fakedat <- data.frame(refl1 = rnorm(401), refl2 = rnorm(401), wavelength = c(300:700))
#' head(fakedat)
#' # Determine if is rspec object
#' is.rspec(fakedat)
#' # Convert to rspec object
#' fakedat2 <- as.rspec(fakedat)
#' is.rspec(fakedat2)
#' head(fakedat2)
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}

as.rspec <- function(specs, whichwl = NULL) {

if (is.matrix(specs)) {
  name <- colnames(specs)
} else
if (is.data.frame(specs)) {
  name <- names(specs)
} else {
  stop('object must be a data frame or matrix')
}


ind <- sapply(1:ncol(specs), function(x) {sd(diff(specs[,x]))})

if (any(ind==0)) {
  wl_index <- which(ind==0)
  wl <- specs[, wl_index]
  specs <- specs[, -wl_index]
  name <- name[-wl_index]
} else {
  wl <- 1:nrow(specs)
  specs <- specs
  name <- name
  warning('No wavelengths or whichwl provided; using arbitrary index values')
}

res <- as.data.frame(cbind(wl, specs))

names(res) <- c('wl', name)

class(res) <- c('rspec', 'data.frame')

res

}



is.rspec <- function(x) inherits(x, "rspec")
