#' Spectrum to rgb color conversion
#'
#' Calculates rgb values from spectra based on human color matching functions
#'
#' @param specs (required) An rspec object of spectral data with columns as spectra. 
#' 
#' @return a character vector of class \code{spec2rgb} consisting of hexadecimal color values
#' for passing to further plotting functions.
#' @export
#' @examples \dontrun{
#' #INCLUDE EXAMPLE}
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}
#' @references CIE. (1932). Commission Internationale de l'Éclairage Proceedings, 1931. Cambridge: Cambridge University Press.
#' @references Color matching functions obtained from Colour and Vision Research Laboratory 
#' online data respository at http://www.cvrl.org/.
#' @references http://www.cs.rit.edu/~ncs/color/t_spectr.html

spec2rgb <- function(specs) {

wl_index <- which(names(specs)=='wl')
if (length(wl_index > 0)){
  wl <- specs[, wl_index]
  specs <- as.data.frame(specs[, -wl_index])
    } else {
    stop('No wavelengths supplied; no default')
    }

specs <- specs[which(wl==400):which(wl==700),]
specs <- as.matrix(specs)

sens <- pavo:::ciexyz

P2 <- sapply(1:ncol(specs), function(x) specs[, x] / sum(specs[, x]))  # normalize to sum of 1

# Convolute
X <- apply(sens[, 'x'] * P2, 2, sum)
Y <- apply(sens[, 'y'] * P2, 2, sum)
Z <- apply(sens[, 'z'] * P2, 2, sum)
XYZ <- rbind(X, Y, Z)

xyzmat <- rbind(c(3.240479, -1.537150, -0.498535),
                c(-0.969256, 1.875992, 0.041556),
                c(0.055648, -0.204043, 1.057311))

XYZ <- sapply(1:ncol(XYZ), function(x) XYZ[, x] / sum(XYZ[, x]))

rgb1 <- sapply(1:ncol(XYZ), function(x) xyzmat%*%as.matrix(XYZ[, x]))

# normalization
rgb1[rgb1 < 0] <- 0
rgb1[rgb1 > 1] <- 1

colrs <- rgb(r=rgb1[1,], g=rgb1[2,], b=rgb1[3,])

class(colrs) <- c('spec2rgb', 'character')
colrs

}