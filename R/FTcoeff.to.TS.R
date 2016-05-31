##' Calculate the time series for specified time points from the real
##' and imaginary parts of the Fourier coefficients
##'
##' Content for the details section. Still to be written.
##' @title Time series from the Fourier coefficients
##' @param timepoints Time points at which the time series is to be evaluated
##' @param FT Matrix or dataframe containing the Fourier coefficients
##' @param n.modes number of modes that should be considered to calculate the time series. Will always include the constant term, and then the annual, bi-annual , etc modes until n.modes. Default is n.modes = 3. 
##' @param n.time.points.in number of time points upon which calculation of the input Fourier transform was based. Necessary for correct scaling of the time series. Default value is 512 ( = 8 years * 64 points per year)
##' @return A matrix with the time series for each time point and Fourier transform. Time points are given in columns, individual Fourier transforms in rows, so dim(matrix) = c(nrow(FT), length(timepoints). 
##' @author Tini
##' @export
##' examples
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
##' time.points.out = ((0:11)+0.5)/12 ## monthly time points at the mid-month
##' TS = FTcoeff.to.TS(timepoints = time.points.out, FT = FT, n.modes = 4,
##'     n.time.points.in = n.time.points)
##' 
##' plot(time.points.out, TS[1, ], type = "o", ylim = c())
FTcoeff.to.TS = function(timepoints, FT, n.modes = 3, n.time.points.in = 512) {
  re.im.cols = cols.of.mode(names(FT))

  TS.out = matrix(FT[, re.im.cols$re.cols["0"]]/n.time.points.in, nrow = nrow(FT), ncol = length(timepoints))

  for(i in 1:n.modes) {
    TS.out = TS.out + 1/n.time.points.in * (
        outer(FT[, re.im.cols$re.cols[as.character(i)]],
              cos(2 * pi * timepoints * i)) +
        outer(FT[, re.im.cols$im.cols[as.character(i)]],
              sin(2 * pi * timepoints * i))
        )
  }
  return(TS.out)
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
##     ppyear, "ppyear_", file.type, "_adm1_", adm0, ".txt"), header = TRUE,
##     sep = "\t", quote = "")

## time.points.out = ((0:11)+0.5)/12 ## monthly time points at the mid-month
## TS = FTcoeff.to.TS(timepoints = time.points.out, FT = FT, n.modes = 4,
##     n.time.points.in = n.time.points)

## plot(time.points.out, TS[1, ], type = "o", ylim = c())
