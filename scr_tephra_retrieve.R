list.of.packages <- c("R.utils", "ncdf4", "naivebayes", "psych")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(R.utils)
library(ncdf4)
library(naivebayes)
library(psych)

#setwd("/Change/theWorking/Directory/")
gunzip("sample/furuno_2020-08-10_032400_el01.nc.gz", remove=FALSE, overwrite=T)
ncin <- "sample/furuno_2020-08-10_032400_el01.nc"
ncfile <- nc_open(ncin, write=TRUE)

#radar parameter
REF <- ncvar_get(ncfile, "REF")
data.frame(zh=matrix(REF, ncol=1)) -> zhh.df

#testing tephra retrieval 
model <- readRDS("class_bayesian.rds")
predict(model, zhh.df, type=('c')) -> class
as.numeric(as.character(class)) -> class2
data.frame(zhh.df, class=class) -> zhh.df
read.csv("coefficient_ca.csv") -> coef_ca
ca <-  ifelse(zhh.df$zh <= 0,0,
                  ifelse(zhh.df$class == "4", coef_ca[1,3]*zhh.df$zh^coef_ca[1,4],
                         ifelse(zhh.df$class == "5", coef_ca[2,3]*zhh.df$zh^coef_ca[2,4],
                                ifelse(zhh.df$class == "6", coef_ca[3,3]*zhh.df$zh^coef_ca[3,4],
                                       ifelse(zhh.df$class == "7", coef_ca[4,3]*zhh.df$zh^coef_ca[4,4],
                                              ifelse(zhh.df$class == "8", coef_ca[5,3]*zhh.df$zh^coef_ca[5,4],
                                                     coef_ca[6,3]*zhh.df$zh^coef_ca[6,4]))))))

read.csv("coefficient_ra.csv") -> coef_ra
ra <-  ifelse(zhh.df$zh <= 0,0,
              ifelse(zhh.df$class == "4", coef_ra[1,3]*zhh.df$zh^coef_ra[1,4],
                     ifelse(zhh.df$class == "5", coef_ra[2,3]*zhh.df$zh^coef_ra[2,4],
                            ifelse(zhh.df$class == "6", coef_ra[3,3]*zhh.df$zh^coef_ra[3,4],
                                   ifelse(zhh.df$class == "7", coef_ra[4,3]*zhh.df$zh^coef_ra[4,4],
                                          ifelse(zhh.df$class == "8", coef_ra[5,3]*zhh.df$zh^coef_ra[5,4],
                                                 coef_ra[6,3]*zhh.df$zh^coef_ra[6,4]))))))


#saved the data
## collect the data into the original NetCDF file
ncfile$dim[["BINS"]] -> xdim
ncfile$dim[["RAYS"]] -> ydim
mv <- 1.e30
var_cl <- ncvar_def('CLASS', 'ND', list(xdim,ydim))
var_ca <- ncvar_def('CA', 'gr/m^3', list(xdim,ydim), mv, "Ash concentration")
var_ra <- ncvar_def('RA', 'kg/m^-2/h^-1',  list(xdim,ydim), mv, "Fall out rate")
ncfile <- ncvar_add(ncfile, var_ca)
ncfile <- ncvar_add(ncfile, var_ra)
ncfile <- ncvar_add(ncfile, var_cl)
nx <- length(xdim$vals)
ny <- length(ydim$vals)
matrix(class2, nx, ny) -> class3
matrix(ca, nx, ny) -> ca2
matrix(ra, nx, ny) -> ra2
ncvar_put(ncfile, var_cl, class3, start=c(1,1)) -> adds
ncvar_put(ncfile, var_ca, ca2, start=c(1,1)) -> adds
ncvar_put(ncfile, var_ra, ra2, start=c(1,1)) -> adds
ncfile
nc_close(ncfile)
