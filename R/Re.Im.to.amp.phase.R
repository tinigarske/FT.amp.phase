##' Calculate the amplitude and phase of a mode of a Fourier transform
##' given the real and imaginary parts of the mode.
##'
##' .. content for \details{} ..
##' @title Amplitude and phase. 
##' @param Re Real part of the mode of interest of the Fourier
##' Transform. Can be a scalar or a vector.
##' @param Im Imaginary part of the mode of interest of the Fourier
##' Transform. Can be a scalar or a vector. Needs to be the same
##' length as Re.
##' @param n.time.points Number of time points for which the Fourier
##' transform was calculated. Default value is 512 ( = 8 years * 64
##' points per year).
##' @return If Re and Im are scalars, a vector of length 2 with
##' amplitude and phase of the mode. If Re and Im are vectors, a
##' matrix with 2 columns named amp and phase, with the amplitude and
##' phase for each entry of Re and Im.
##' @author Tini
##' @export
Re.Im.to.amp.phase = function(Re, Im, n.time.points = 512) {
  if(length(Re) != length(Im)) stop("Re and Im must have the same length.")
  
  amp = sqrt(Re^2 + Im^2)/(nyears*ppyear)
  phase = atan2(Im, Re)
  if(length(Re)==1) {
    return(c(amp = amp, phase = phase))
  } else {
    return(cbind(amp = amp, phase = phase))
  }
}
