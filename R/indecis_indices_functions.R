# Author: Fergus Reig Gracia <http://fergusreig.es/>; Environmental Hydrology, Climate and Human Activity Interactions, Geoenvironmental Processes, IPE, CSIC <http://www.ipe.csic.es/hidrologia-ambiental/>
# Version: 1.0

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/> <http://www.gnu.org/licenses/gpl.txt/>.
#####################################################################

#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom utils read.csv

#' @import chron
#' @import SPEI
#' @import weathermetrics
library(chron)
library(SPEI)
library(weathermetrics)

# Abreviaturas
TMEAN = "tg" # daily mean temperature TG, ℃
TMIN = "tn" # daily minimum temperature TN, ℃
TMAX = "tx" # daily maximum temperature TX, ℃
PRECIPITATION = "rr" # daily precipitation sum RR, mm
LAT = "lat" # lat, degree

# Necesitamos
# mean radiation, W/m-2
# eto
## insolation sunshine duration
## w average wind
## lat latitude
## tdew dew point
## rh relative humidity

# No usamos
# daily averaged sea level pressure PP


SPRING = "spring" #marzo, abril, mayo - 3, 4, 5
SUMMER = "summer" #junio, julio, agosto - 6, 7, 8
FALL = "fall" #septiembre, octubre, nobiembre - 9, 10, 11
WINTER = "winter" # diciembre, enero, febrero - 12, 1, 2

MONTH = "month"
YEAR = "year"
SEASON = "season"

JAN = "Jan"
FEB = "Feb"
MAR = "Mar"
APR = "Apr"
MAY = "May"
JUN = "Jun"
JUL = "Jul"
AUG = "Aug"
SEP = "Sep"
OCT = "Oct"
NOV = "Nov"
DEC = "Dec"

#' Operation de data para los valores de oks == ok
#'
#' @param ok ok
#' @param oks oks
#' @param data data
#' @param operation operation
#' @param ... ...
#' @return  operation
#' @examples
#' calcf_data__(years[12], years, max, maxf)
calcf_data__ = function(ok, oks, data, operation, ...){
  return(operation(data[oks==ok], ...))
}

#' Operation de data, agrupando los datos por valores de names
#'
#' @param data_names names
#' @param data data
#' @param operation operation
#' @param ... ...
#' @return operation
#' @examples
#' calc = calcf_data_(names=months_years(chron(names(pr.value))), data=pr.value, operation=maxf)
calcf_data_ = function(data_names, data, operation, ...){
  average = sapply(unique(data_names), calcf_data__, oks=data_names, data=data, operation=operation, ...)
  average[average==Inf | average==-Inf] = NA
  names(average) = unique(data_names)

  if(sumf(grepl(SPRING, names(average)))>0){
    average = array(c(average[grepl(SPRING, names(average))], average[grepl(SUMMER, names(average))], average[grepl(FALL, names(average))], average[grepl(WINTER, names(average))]), dim=c(length(average)/4, 4), dimnames=list(unique(substr(names(average), nchar(names(average))-3, nchar(names(average)))), c(SPRING, SUMMER, FALL, WINTER)))
  }else{
    if(sumf(grepl(JAN, names(average)))>0){
      average = array(c(average[grepl(JAN, names(average))], average[grepl(FEB, names(average))], average[grepl(MAR, names(average))], average[grepl(APR, names(average))], average[grepl(MAY, names(average))], average[grepl(JUN, names(average))], average[grepl(JUL, names(average))], average[grepl(AUG, names(average))], average[grepl(SEP, names(average))], average[grepl(OCT, names(average))],average[grepl(NOV, names(average))], average[grepl(DEC, names(average))]), dim=c(length(average)/12, 12), dimnames=list(unique(substr(names(average), nchar(names(average))-3, nchar(names(average)))), c(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)))
    }
  }
  return(average)
}

quantile_null = function(x, ...){
  if(is.null(x)){
    return(NULL)
  }else{
    return(quantile(x, ..., na.rm = TRUE))
  }
}

#' Operation de data, agrupando los datos por valores de names
#'
#' @param data data
#' @param date date 
#' @param extract_names Operation to split data
#' @param operation Main operation
#' @param data_names names of each period of time
#' @param ... ...
#' @return result operation
#' @examples
#' calcf_data(data=data, extract_names=years, operation=meanf)
calcf_data = function(data, date, extract_names, data_names, operation, ...){
  if(missing(data) || is.null(data) || length(data)==0){
    return(NULL)
  }
  if(!missing(...) && sum(sapply(list(...), is.null))>0){
    return(NULL)
  }
  if(missing(data_names) || is.null(data_names[1]) || is.na(data_names[1])){
    if(missing(date) || is.null(date[1]) || is.na(date[1])){
      date = chron(names(data))
    }
    data_names = extract_names(date)
  }
  return(calcf_data_(data_names=data_names, data=data, operation=operation, ...))
}

#' Estación del año de una fecha dada
#'
#' @param max maximum temperature
#' @param min minimum temperature
#' @param mean medium temperature
#' @param extract_names Operation to split data
#' @param operation Main operation
#' @param ... ...
#' @return result operation
#' @examples
#' calcf_data_max_min_mean(time)
calcf_data_max_min_mean = function(max, min, mean, extract_names, operation, ...){
  return(data.frame(max=calcf_data(max, extract_names, operation, ...), min=calcf_data(min, extract_names, operation, ...), mean=calcf_data(mean, extract_names, operation, ...)))
}

#' Data with months and years in names 
#' @param data data
#' @return dates
#' @examples
#' byMonths_chron(data=tmax.value)
byMonths_chron = function(data){
  return(chron(gsub("_", "/", paste0("1", "_", names(data))), format=c(dates = "d/m/yy", times = "h:m:s"), out.format=c(dates = "m/d/y", times = "h:m:s")))
}

#' Seasonals
#'
#' @param time chron
#' @return seasonals
#' @examples
#' seasonals(time=chron(names(tmax.value)))
seasonals = function(time){
  # https://stackoverflow.com/questions/28030936/r-need-to-extract-month-and-assign-season
  seasons <- factor(quarters(as.chron(time+31)), labels = c(WINTER, SPRING, SUMMER, FALL))
  return(seasons)
}

#' Seasonals by years
#'
#' @param time chron
#' @return seasonals by years
#' @examples
#' seasonals_years(time=chron(names(tmax.value)))
seasonals_years = function(time){
  return(paste(seasonals(time), years(time), sep="_"))
}

#' Months by years
#'
#' @param time chron 
#' @return months by years
#' @examples
#' months_years(time=chron(names(tmax.value)))
months_years = function(time){
  return(paste(months(time), years(time), sep="_"))
}

#' Function to select data
#' @param time.scale month, season or year
#' @return function
select_time_function = function(time.scale){
  if(time.scale==MONTH){
    extract_names = months_years
  }else{
    if(time.scale==SEASON){
      extract_names = seasonals_years
    }else{ #time.scale==YEAR
      extract_names = years
    }
  }
  return(extract_names)
}

#' Et0
#'
#' @param tmin minimum temperature
#' @param tmax maximum temperature
#' @param insolation sunshine duration
#' @param w average wind
#' @param lat latitude
#' @param tdew dew point
#' @param rh relative humidity
#' @return et0
#' @export
#' @examples
#' calc_eto(tmin = tmin.value, tmax = tmax.value, 
#' insolation = insolation.value, w = w.value, lat=lat, tdew = dew_point.value)
calc_eto = function(tmin, tmax, insolation, w, lat, tdew, rh=NA){
  time = chron(names(tmin))
  time1 = time[2:length(time)]
  time1 = c(time1, 
    time[length(time)] +
    as.numeric(as.character(days(meanf(time1-time[-length(time)]))))
    )

  # Fao-56 Penman-Monteith
  dat_mlen = time1 - time
  dat_msum = as.POSIXlt(time)$yday + round(dat_mlen/2)
  data <- penman_fao_diario(Tmin=tmin, Tmax=tmax, U2=w, J=dat_msum, lat=lat, Rs=NA, tsun=insolation, ed=NA, Tdew=tdew, RH=rh, P=NA, P0=NA, z=NA, crop="short")
  data <- data*dat_mlen

  data=tmin
  return(data)
}

#' Dew point to relative humidity
#'
#' @param tmax maximum temperature
#' @param tmin minimum temperature
#' @param td dew point
#' @return rh
#' @export
#' @examples
#' td_to_rh(tmax = tmax.value, tmin = tmin.value, td = dew_point.value)
td_to_rh <- function(tmax, tmin, td){
  t <- (tmax+tmin)/2
  b <- 17.625*t/(243.04+t)
  a <- -(243.04*b+(b-17.625)*td)/(td+243.04)
  r <- 100*exp(a)
  r[r<0] <- 0
  r[r>100] <- 100
  return(r)
}

#' Dew point to water vapour pressure
#' @param td dew point
#' @return vapor
#' @export 
#' @examples
#' td_to_vapor(td = dew_point.value)
td_to_vapor <- function(td){
  l = 2420000 # J/Kg Calor latente de vaporización del agua
  r = 461 #J/(K*Kg) Constante de los gases ideales de vapor de agua
  t = 273 #K, temperatura de referencia
  e = 6.11 #hPa, presión de vapor a la temperatura de referencia
  td = td + 273.15 # Pasamos la temperatus a Kelvin
  r = e * exp((l/r) * (1/t - 1/td))
  return(r)
}

#' Average temperature
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' average_temp(data=tmax.value)
average_temp = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calcf_data(data, extract_names=select_time_function(time.scale), data_names=data_names, operation=mean, na.rm = na.rm))
}

#' Maximum temperature
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year  
#' @return maximum temperature
#' @export
#' @examples
#' maximum_temp(data=tmax.value)
maximum_temp = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=max, na.rm = na.rm))
}

#' Minimum temperature
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return minimum temperature
#' @export
#' @examples
#' minimum_temp(data=tmax.value)
minimum_temp = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=min, na.rm = na.rm))
}

#' SPI: Standardized Precipitation Index
#' 1, 3, 6 and 12 month SPI
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param scale scale
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPI
#' @export
#' @examples
#' calc_spi(data = pr.value)
calc_spi = function(data, data_names=NULL, scale=3, na.rm=FALSE){
  if(is.null(data)) { return(NULL) }
  byMonths = calcf_data(data=data, extract_names=months_years, operation=sum, na.rm=na.rm)
  if(sum(is.na(byMonths))==0){
    byMonths.vector = array(t(byMonths), dim=length(byMonths))
    spi.vector = array(spi(byMonths.vector, scale=scale, na.rm = TRUE)$fitted[, 1])
  }else{
    spi.vector = NA
  }
  spi.matrix = t(array(spi.vector, dim=c(dim(byMonths)[2], dim(byMonths)[1])))
  colnames(spi.matrix)=colnames(byMonths)
  rownames(spi.matrix)=rownames(byMonths)
  return(spi.matrix)
}

#' 125. SPEI: Standardized Precipitation Evapotranspiration Index
#' 1, 3, 6 and 12 month SPEI
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param scale scale
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPEI
#' @export
#' @examples
#' calc_spei(eto = eto.value, pr = pr.value)
calc_spei = function(eto, pr, data_names=NULL, scale=3, na.rm=FALSE){
  if(is.null(eto) | is.null(pr)) { return(NULL) }
  data = pr - eto 
  byMonths = calcf_data(data=data, extract_names=months_years, operation=sum, na.rm=na.rm)
  if(sum(is.na(byMonths))==0){
    byMonths.vector = array(t(byMonths), dim=length(byMonths))
    spei.vector = array(spei(byMonths.vector, scale=scale, na.rm = TRUE)$fitted[, 1])
  }else{
    spei.vector = NA
  }
  spei.matrix = t(array(spei.vector, dim=c(dim(byMonths)[2], dim(byMonths)[1])))
  colnames(spei.matrix)=colnames(byMonths)
  rownames(spei.matrix)=rownames(byMonths)
  return(spei.matrix)
}

