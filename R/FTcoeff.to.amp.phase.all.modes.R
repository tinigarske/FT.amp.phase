##' Calculate the amplitude and phase of several modes of Fourier
##' transforms given the real and imaginary parts of the modes.
##'
##' Content for the details section. Still to be written.
##' @title Amplitude and phase of the full Fourier transform
##' @param FT matrix or dataframe of the modes of interest of several
##' Fourier transforms. Each individual FT is given in a row, and
##' columns with individual modes are expected to be named "Rex" and
##' "Imx", for mode x where x is an integer (although it does not need
##' to be tied to the frequency of the mode).
##' @param n.time.points Number of time points for which the Fourier
##' transform was calculated. Default value is 512 ( = 8 years * 64
##' points per year).
##' @return A matrix with amplitude and phase columns for each mode
##' within the input data FT
##' @author Tini
##' @export
##' @examples
##' ## setting up some parameters:
##' dat.dir = "//fi--didenas3/Dengue/Data/processed/adm1/FTs/"
##' year.i = 2007
##' year.f = 2014
##' ppyear = 64
##' file.type = "EVI"
##' adm0 = "FRA"
##' 
##' n.time.points = (year.f - year.i + 1)*ppyear
##' 
##' ## reading in the data:
##' FT = read.table(paste0(dat.dir, "FTfreqs_", year.i, "-", year.f, "_",
##' ppyear, "ppyear_", file.type, "_adm1_", adm0, ".txt"), header = TRUE,
##' sep = "\t", quote = "")
##' 
##' ## calculating the amplitude and phase for all modes:
##' amp.phase = FTcoeff.to.amp.phase.all.modes(FT, n.time.points =
##' n.time.points)
##' ## now you might want to cbind this to the original FT dataframe:
##' FT = cbind(FT, data.frame(amp.phase))
FTcoeff.to.amp.phase.all.modes = function(FT, n.time.points = 512) {
  re.im.cols = cols.of.mode(names(FT))
  
  amp.phase = NULL
  for(i in 2:length(re.im.cols$re.cols)) {
    amp.phase = cbind(amp.phase, FTcoeff.to.amp.phase(Re = FT[, re.im.cols$re.cols[i]], Im = FT[, re.im.cols$im.cols[i]], n.time.points = n.time.points))
    colnames(amp.phase)[ncol(amp.phase) + c(-1,0)] = paste0(colnames(amp.phase)[ncol(amp.phase) + c(-1,0)], as.numeric(names(re.im.cols$re.cols)[i]))
  }

  return(amp.phase)
}


 ## ## setting up some parameters:
 ## dat.dir = "//fi--didenas3/Dengue/Data/processed/adm1/FTs/"
 ## year.i = 2007
 ## year.f = 2014
 ## ppyear = 64
 ## file.type = "EVI"
 ## adm0 = "FRA"
 
 ## n.time.points = (year.f - year.i + 1)*ppyear
 
 ## ## reading in the data:
 ## FT = read.table(paste0(dat.dir, "FTfreqs_", year.i, "-", year.f, "_",
 ## ppyear, "ppyear_", file.type, "_adm1_", adm0, ".txt"), header = TRUE,
 ## sep = "\t", quote = "")
 
 ## ## calculating the amplitude and phase for all modes:
 ## amp.phase = FTcoeff.to.amp.phase.all.modes(FT, n.time.points = n.time.points)
 ## ## now you might want to cbind this to the original FT dataframe:
 ## FT = cbind(FT, data.frame(amp.phase))



