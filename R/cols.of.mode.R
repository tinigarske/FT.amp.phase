##' Pull out the columns containing the real and imaginary parts of the FTcoefficients
##'
##' Content for the details section. Still to be written.
##' @title Columns containing the FT coefficients 
##' @param FTnames Column names of the matrix or data frame containing
##' the FT coefficients
##' @return A list of length 2 containing vectors of the indices of
##' columns containing the real and imaginary parts of FT
##' coefficients. The names argument for both the vector of real and
##' imaginary parts details the mode: 1 for the annual mode, 2 for the
##' bi-annual mode etc. The first entry, named 0 contains the constant
##' term in the real vector, and NA in the imaginary vector.
##' @author Tini
cols.of.mode = function(FTnames) {
  if(length(grep("const_term", FTnames)) == 1) {
    cols.of.mode.dide.txt(FTnames)
  } else if(length(grep("const_term", FTnames)) == 0) {
    cols.of.mode.air.temp.paper(FTnames)
  } else stop("cols.of.mode: argument FTnames has at least 2 instances of 'const_term'")
}

cols.of.mode.dide.txt = function(FTnames) {
  ## columns are called const_term, Re0, Im0, etc.
  ## 0 indexes the annual mode, 1 the bi-annual mode, ...
  re.cols = grep("^Re", FTnames)
  re.modes = as.numeric(gsub("Re", "", FTnames[re.cols]))
  im.cols = grep("^Im", FTnames)
  im.modes = as.numeric(gsub("Im", "", FTnames[im.cols]))
  ## making sure that re and im columns are ordered in the same way
  ## (which they should be anyway):
  im.cols = im.cols[match(re.modes, im.modes)]
  im.modes = im.modes[match(re.modes, im.modes)]

  re.cols = c(grep("^const_term", FTnames), re.cols)
  names(re.cols) = c(0, 1+re.modes)
  im.cols = c(NA, im.cols)
  names(im.cols) = c(0, 1+im.modes)
  return(list(re.cols = re.cols, im.cols = im.cols))
}

cols.of.mode.air.temp.paper = function(FTnames) {
  ## columns are called H0, ReH1, ImH1, etc.
  ## 0 indexes the constant term, 1 the annual mode, 2 the bi-annual mode, ...
  re.cols = grep("^ReH", FTnames)
  re.modes = as.numeric(gsub("ReH", "", FTnames[re.cols]))
  im.cols = grep("^ImH", FTnames)
  im.modes = as.numeric(gsub("ImH", "", FTnames[im.cols]))
  ## making sure that re and im columns are ordered in the same way
  ## (which they should be anyway):
  im.cols = im.cols[match(re.modes, im.cols)]
  im.modes = im.modes[match(re.modes, im.modes)]

  re.cols = c(grep("^H0", FTnames), re.cols)
  names(re.cols) = c(0, re.modes)
  im.cols = c(NA, im.modes)
  names(im.cols) = c(0, im.modes)
  return(list(re.cols = re.cols, im.cols = im.cols))

}
