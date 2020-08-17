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
NULL

#' @import chron
#' @import SPEI
#' @import weathermetrics
NULL

library(chron)
library(SPEI)
library(weathermetrics)

# Abreviaturas
TMEAN = "tg" # daily mean temperature TG, Celsius
TMIN = "tn" # daily minimum temperature TN, Celsius
TMAX = "tx" # daily maximum temperature TX, Celsius
PRECIPITATION = "rr" # daily precipitation sum RR, mm
LAT = "lat" # latitude, degree
RADIATION = "radiation" # net radiation, J/m2
RADIATION_W = "radiation_w" # net radiation, W/m2
DEWPOINT = "dewpoint" #dew point, Celsius
WIND = "wind" #average wind, m/s
HUMIDITY = "humidity" #relative humidity, %
VAPOUR = "vapour" #water vapour pressure, hPa
WINDGUST = "windgust" #maximum wind gust, m/s
EVAPOTRANSPIRATION = "evaporation" #Evapotranspiration, mm
ETO = "eto" #Eto, mm
SNOWFALL = "snowfall" # snowfall, m of water equivalent
SNOWFALLMM = "snowfallmm" # snowfall, mm of water equivalent
SWE = "swe" #swe, m of water equivalent
INSOLATION = "insolation" #insolation, hours of sun -> MAL
CLOUD = "cloud" # cloud cover, %
CLOUD100 = "cloud100" #cloud base below 100 meter, %
RADIATIONTOA = "radiationtoa" #solar radiation at TOA (alt top of the atmosphere empirically obtained), W/m2
MDE = "mde" # digital elevation model, m
SNOWDEPTH = "snowdepth" #snow depth, mm snow
SNOWDENSITY = "snowdensity" # snow density, kg m-3
RADIATIONTEMPERATURE = "radiationtemperature" # radiation temperature, Celsius

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
HYDROYEAR = "hydrological_years"

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
#' @keywords internal
calcf_data__ = function(ok, oks, data, operation, ...){
  return(operation(data[oks==ok], ...))
}

#' Operation de data, agrupando los datos por valores de names
#'
#' @param data_names names
#' @param data data
#' @param operation operation
#' @param time.scale month, season, year or hydrological_years
#' @param ... ...
#' @return operation
#' @keywords internal
calcf_data_ = function(data_names, data, operation, time.scale, ...){
  average = sapply(unique(data_names), calcf_data__, oks=data_names, data=data, operation=operation, ...)
  average[average==Inf | average==-Inf] = NA
  names(average) = unique(data_names)

  if(sum(grepl(SPRING, names(average)), na.rm = TRUE)>0 & length(average)>4){
    average[1] = NA
    average = average[-length(average)]
    average = array(c(average[grepl(WINTER, names(average))], average[grepl(SPRING, names(average))], average[grepl(SUMMER, names(average))], average[grepl(FALL, names(average))]), dim=c(ceiling(length(average)/4), 4), dimnames=list(unique(substr(names(average), nchar(names(average))-3, nchar(names(average)))), c(WINTER, SPRING, SUMMER, FALL)))
  }else{
    if(sum(grepl(JAN, names(average)), na.rm = TRUE)>0 & length(average)>12){
      average = array(c(average[grepl(JAN, names(average))], average[grepl(FEB, names(average))], average[grepl(MAR, names(average))], average[grepl(APR, names(average))], average[grepl(MAY, names(average))], average[grepl(JUN, names(average))], average[grepl(JUL, names(average))], average[grepl(AUG, names(average))], average[grepl(SEP, names(average))], average[grepl(OCT, names(average))],average[grepl(NOV, names(average))], average[grepl(DEC, names(average))]), dim=c(ceiling(length(average)/12), 12), dimnames=list(unique(substr(names(average), nchar(names(average))-3, nchar(names(average)))), c(JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)))
    }
  }
  if(time.scale==HYDROYEAR){
    average[1] = NA
  }
  return(average)
}

#' quantile null
#'
#' @param x x
#' @param ... ...
#' @return operation
#' @keywords internal
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
#' @param time.scale month, season, year or hydrological_years
#' @param extract_names Operation to split data
#' @param data_names names of each period of time
#' @param operation Main operation
#' @param ... ...
#' @return result operation
#' @keywords internal
calcf_data = function(data, date, time.scale, extract_names=select_time_function, data_names, operation, ...){
  extract_names = extract_names(time.scale)
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
  }else{
    if(length(data_names)!=length(data)){
      stop(paste("Name number", time.scale))
    }
  }
  data_calc = calcf_data_(data_names=data_names, data=data, operation=operation, time.scale=time.scale, ...)
  data_calc[is.nan(data_calc)] = 0
  return(data_calc)
}

#' Data with months and years in names 
#'
#' @param data data
#' @return dates
#' @keywords internal
byMonths_chron = function(data){
  return(chron(gsub("_", "/", paste0("1", "_", names(data))), format=c(dates = "d/m/yy", times = "h:m:s"), out.format=c(dates = "m/d/y", times = "h:m:s")))
}

#' Select quarter days
#'
#' @param functionValues functionValues
#' @param selectFunction selectFunction
#' @param selectValues selectValues
#' @param na.rm na.rm
#' @return quarter days
#' @keywords internal
months_quarter = function(functionValues, selectFunction, selectValues, na.rm = FALSE){
  if(missing(selectValues)){
    selectValues = functionValues
  }
  dataMonth <- function(n, functionValues){
    return(functionValues[grepl(sprintf("%02d", n), substr(names(functionValues), 1, 2))])
  }
  sumMonth <- function(n, functionValues){
    return(sum(dataMonth(n, functionValues)))
  }
  agregateMonth <- function(n, functionValues){
    return(sum(functionValues[n:(n+2)]))
  }
  valueMonths <- sapply(1:12, sumMonth, functionValues)
  valueMonths <- sapply(1:10, agregateMonth, valueMonths)
  months = which(valueMonths==selectFunction(valueMonths, na.rm=na.rm), arr.ind = TRUE, useNames = TRUE)[1]
  if(!is.na(months)){
      selectValues = c(dataMonth(months, selectValues), dataMonth(months+1, selectValues), dataMonth(months+2, selectValues))
  }else{
    selectValues = NA
  }
  return(selectValues)
}

#' Seasonals
#'
#' @param time chron
#' @return seasonals
#' @keywords internal
seasonals = function(time){
  # https://stackoverflow.com/questions/28030936/r-need-to-extract-month-and-assign-season
  labelSeasons <- c(WINTER, SPRING, SUMMER, FALL)
  qTime <- quarters(as.chron(time+31))
  if(length(unique(qTime))>1){
    seasons <- factor(qTime, labels = labelSeasons)
  }else{
    seasons <- labelSeasons[as.numeric(qTime)]
  }
  return(seasons)
}

#' Hydrological years
#'
#' @param time chron
#' @return seasonals by years
#' @keywords internal
hydrological_years = function(time){
  changeMonths <- as.numeric(months(time))%in%c(10, 11, 12)
  timeNames <- as.numeric(as.character(years(time)))
  timeNames[changeMonths] <- timeNames[changeMonths] + 1
  return(timeNames)
}

#' Seasonals by years
#'
#' @param time chron
#' @return seasonals by years
#' @keywords internal
seasonals_years = function(time){
  changeMonths <- as.numeric(months(time))==12
  timeNames <- paste(seasonals(time), years(time), sep="_")
  timeNames[changeMonths] <- paste(seasonals(time[changeMonths]), as.numeric(as.character(years(time[changeMonths])))+1, sep="_")
  return(timeNames)
}

#' Months by years
#'
#' @param time chron 
#' @return months by years
#' @keywords internal
months_years = function(time){
  return(paste(months(time), years(time), sep="_"))
}

#' Function to select all "time" data
#'
#' @param time.scale month, season or year
#' @return function
#' @keywords internal
select_all_time_function = function(time.scale){
  if(time.scale==MONTH){
    return(function(time)  { 
      return(months(time))
    })
  }else{
    if(time.scale==SEASON){
     return(function(time)  {
       return(seasonals(time)) 
     })
    }else{ 
      if(time.scale==HYDROYEAR){
        return(function(time)  {
          return(YEAR) 
        })
      }else{ #time.scale==YEAR
        return(function(...) { 
          return(YEAR) 
        })
      }
    }
  }
}

#' Name data station or month 
#' @param data data 
#' @param value value for month, season or year 
#' @param time.scale month, season or year
#' @return function
#' @keywords internal
select_value_for_data = function(data, value, time.scale){
  return(value[select_all_time_function(time.scale)(chron(names(data)[1]))==names(value)])
}

#' Function to select data
#' @param time.scale month, season or year
#' @return function
#' @keywords internal
select_time_function = function(time.scale){
  if(time.scale==MONTH){
    extract_names = months_years
  }else{
    if(time.scale==SEASON){
      extract_names = seasonals_years
    }else{
      if(time.scale==HYDROYEAR){
        extract_names = hydrological_years
      }else{ #time.scale==YEAR
        extract_names = chron::years
      }
    }
  }
  return(extract_names)
}

#' Et0
#'
#' @param tmin daily minimum temperature, Celsius, Celsius
#' @param tmax maximum temperature, Celsius
#' @param toa radiation toa, J/m2
#' @param w average wind, m/s at 10m
#' @param mde mde
#' @param lat latitude
#' @param tdew dew point, Celsius
#' @param radiation radiation, J m-2 
#' @param insolation insolation, hours
#' @param rh relative humidity, percentage
#' @param na.rm na.rm
#' @return et0
#' @keywords internal
calc_eto = function(tmin, tmax, toa, w, mde, lat, tdew, radiation=NA, insolation=NA, rh=NA, na.rm=FALSE){
  # toa = toa/(24*60*60)

  radiation = radiation/1000000
  toa = toa/1000000

  w = 0.75*w # De 10 metros a 2 metros de altura
  time = chron(names(tmin))
  time1 = time[2:length(time)]
  time1 = c(time1, 
    time[length(time)] +
    as.numeric(as.character(days(mean(time1-time[-length(time)], na.rm=na.rm))))
    )

  # Fao-56 Penman-Monteith
  dat_mlen = time1 - time
  dat_msum = as.POSIXlt(time)$yday + round(dat_mlen/2)
  # Tmin=tmin; Tmax=tmax; Ra=toa; Rs=radiation; tsun=insolation; U2=w; J=dat_msum; lat=lat; ed=NA; Tdew=tdew; RH=rh; P=NA; P0=NA; z=mde; crop="short"
  data <- penman_fao_diario(Tmin=tmin, Tmax=tmax, Ra=toa, Rs=radiation, tsun=NA, U2=w, J=dat_msum, lat=lat, ed=NA, Tdew=tdew, RH=rh, P=NA, P0=NA, z=mde, crop="short")
  # data2 <- penman_fao_diario(Tmin=tmin, Tmax=tmax, Ra=NA, Rs=NA, tsun=insolation, U2=w, J=dat_msum, lat=lat, ed=NA, Tdew=tdew, RH=rh, P=NA, P0=NA, z=mde, crop="short")
  data <- array(data*dat_mlen)
  names(data) <- names(tmin)
  # print(paste("Values < 0:", sum(data<0, na.rm=TRUE)))
  data[data<0] = 0
  return(data)
}

#' Dew point to relative humidity
#'
#' @param tmax maximum temperature
#' @param tmin daily minimum temperature, Celsius
#' @param td dew point
#' @return rh
#' @keywords internal
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
#' @keywords internal
td_to_vapor <- function(td){
  l = 2420000 # J/Kg Calor latente de vaporización del agua
  r = 461 #J/(K*Kg) Constante de los gases ideales de vapor de agua
  t = 273 #K, temperatura de referencia
  e = 6.11 #hPa, presión de vapor a la temperatura de referencia
  td = td + 273.15 # Pasamos la temperatus a Kelvin
  r = e * exp((l/r) * (1/t - 1/td))
  return(r)
}

#' Transforma radiancia en insolación
#'
#' @param radiation radiation
#' @param lat lat
#' @param mde mde
#' @return insolación en horas
#' @keywords internal
r_to_in = function(radiation, lat, mde) {
  series = radiation
  time.chron = chron(names(radiation))
  t = 20111
  for (t in 1:length(time.chron)) {
    # dat_mlen = as.POSIXlt(time.chron[t + 1])$yday - as.POSIXlt(time.chron[t])$yday
    # if (is.na(dat_mlen) | dat_mlen < 0) {
    #   dat_mlen = 9   
    # }
    dat_mlen = 1
    dat_msum = as.POSIXlt(time.chron[t])$yday + round(dat_mlen / 2)
    series[t] = penman_rs(J = dat_msum, lat = lat, tsun = radiation[t]/1000000, z = mde, ret = INSOLATION) #MJ/m2
    # series2 = penman_rs(J = dat_msum, lat = lat, tsun = series[t], z = mde, ret = RADIATION)*1000000
    # series[t] = insolation(zenith=lat, jd = dat_msum, height = mde, visibility, RH, tempK, O3, alphag)
  }

# plot(radiation*(24*60*60)/(1000000), type="l")
# plot(series, type="l")

# # a=nc_open("descargas/mean_surface_downward_short_wave_radiation_flux_2009.nc")
# a=nc_open("descargas/surface_solar_radiation_downwards_2009.nc")
# # a=nc_open("descargas/maximum_2m_temperature_since_previous_post_processing_2009.nc")
# a=nc_open("descargas/total_precipitation_2009.nc")
# aa=ncvar_get(a, a$var[[1]]$name, c(1,1,1), c(-1,-1,48))
# aa[90,,1]=270
# aa[,135,1]=270
# image(aa[,163:1,1])
# bb1=apply(aa[,,1:12], c(1,2), sumf)*3600
# bb2=apply(aa[,,13:24], c(1,2), sumf)*3600
# bb2[,121] = -1000
# image(bb2)

# aa=ncvar_get(a, a$var[[1]]$name, c(90,135,1), c(1,1,-1))
# aaTime=ncvar_get(a, "time") #hours since 1900-01-01 00:00:00.0
# aaTime=chron(aaTime/24, origin=c(month=1, day=1, year=1900))

# bb = aa[seq(1, length(aa), 24)]
# for(i in 1:23){
#   print(seq(1, length(aa), 24)[1]+i)
#   bb = bb+aa[seq(1, length(aa), 24)+i]
# }
# # bb = bb*3600

# plot(bb/1000000, type="l")

# meanf(bb)


  # series = series/10
  series[series < 0] <- 0
  series[series == Inf] <- NA
  series[series == -Inf] <- NA

  # #' Correción de insolación
  # #'
  # #' @param data.week datos
  # #' @param g latitud en grados
  # #'
  # #' @return datos corregidos
  # #' @export
  # #'
  # #' @examples
  # #' Tenemos un test: test_in_final_correction
  # in_final_correction =  function(data.week, g) {

  #   #' Valores in, día juliano, latitud en grados de los puntos de los que se extraen los valores de in
  #   #'
  #   #' @param inmax datos
  #   #' @param ji fecha a calcular
  #   #' @param g latitud den grados (mismo orden y número de datos que inmax)
  #   #'
  #   #' @return datos corregidos
  #   #' @export
  #   #'
  #   #' @examples
  #   in_final_correction_t = function(inmax, ji, g) {
  #     inmax[inmax < 0] = 0
  #     inmax[inmax == Inf] = NA
  #     inmax[inmax == -Inf] = NA
  #     a <- 0.409 * sin(yday(ji) * 2 * pi / 365 - 1.39)
  #     w <- acos(-tan(g) * tan(a))
  #     n <- w * 24 / pi
  #     n.ok <- inmax > n && !is.na(inmax)
  #     inmax[n.ok] <- n[n.ok]
  #     return(inmax)
  #   }

  #   t = rownames(data.week)[1]

  #   inmax.name <- colnames(data.week[t,])
  #   if(is.null(inmax.name)){
  #     inmax.name <- names(data.week[t,])
  #   }
  #   g <- g[inmax.name]

  #   for (t in rownames(data.week)) {
  #     chron.t = chronf(t)
  #     data.week[t,] = in_final_correction_t(inmax = data.week[t,], ji = chron.t, g=g)
  #   }
  #   return(data.week)
  # }

  # data_siar = in_final_correction(data.week=data_siar, g=geo$g)
  return(series)
}

#' Average temperature
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return average temperature
#' @keywords internal
average_temp = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calcf_data(data, time.scale=time.scale, data_names=data_names, operation=mean, na.rm = na.rm))
}

#' Maximum temperature
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return maximum temperature
#' @keywords internal
maximum_temp = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calcf_data(data=data, time.scale=time.scale, data_names=data_names, operation=max, na.rm = na.rm))
}

#' Minimum temperature
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return minimum temperature
#' @keywords internal
minimum_temp = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calcf_data(data=data, time.scale=time.scale, data_names=data_names, operation=min, na.rm = na.rm))
}

#' SPI: Standardized Precipitation Index
#' 1, 3, 6 and 12 month SPI
#' 
#' @param data daily precipitation, mm
#' @param data_names names of each period of time
#' @param scale scale
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPI
#' @keywords internal
calc_spi = function(data, data_names=NULL, scale=3, na.rm=FALSE){
  if(is.null(data)) { return(NULL) }
  byMonths = calcf_data(data=data, time.scale=MONTH, operation=sum, na.rm=FALSE, data_names=NULL)
  # byMonths = byMonths[as.character(1979:2017), ]
  if((na.rm & sum(!is.na(byMonths))!=0) | (!na.rm & sum(is.na(byMonths))==0)){
    byMonths.vector = array(t(byMonths), dim=length(byMonths))
    spi.vector = array(spi(byMonths.vector, scale=scale, na.rm = na.rm)$fitted[, 1])
  }else{
    spi.vector = NA
  }
  spi.matrix = t(array(spi.vector, dim=c(dim(byMonths)[2], dim(byMonths)[1])))
  colnames(spi.matrix)=colnames(byMonths)
  rownames(spi.matrix)=rownames(byMonths)
  spi.matrix[is.na(byMonths)] = NA
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
#' @keywords internal
calc_spei = function(eto, pr, data_names=NULL, scale=3, na.rm=FALSE){
  if(is.null(eto) | is.null(pr)) { return(NULL) }
  data = pr - eto
  byMonths = calcf_data(data=data, time.scale=MONTH, operation=sum, na.rm=FALSE, data_names=NULL)
  if((na.rm & sum(!is.na(byMonths))!=0) | (!na.rm & sum(is.na(byMonths))==0)){
    byMonths.vector = array(t(byMonths), dim=length(byMonths))
    spei.vector = array(spei(byMonths.vector, scale=scale, na.rm = na.rm)$fitted[, 1])
  }else{
    spei.vector = NA
  }
  spei.vector[spei.vector < -5] <- -5
  spei.vector[spei.vector > 5] <- 5
  spei.matrix = t(array(spei.vector, dim=c(dim(byMonths)[2], dim(byMonths)[1])))
  colnames(spei.matrix)=colnames(byMonths)
  rownames(spei.matrix)=rownames(byMonths)
  spei.matrix[is.na(byMonths)] = NA
  return(spei.matrix)
}

