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

# setwd("/mnt/dostb2/fuendetodos/DATOS/paquete_sequia")
# load("main.R")
# load("custom_functions.R")

#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom utils read.csv

#' @import chron
#' @import SPEI
#' @import weathermetrics
library(chron)
library(SPEI)
library(weathermetrics)

# source("Fire_Danger_Index_Functions.R")
# source("penman_fao_dia.R")

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
  # arguments <- list(...)
  # write(arguments[[1]], file="prueba.csv")
  # return(operation(data[oks==ok], arguments[[1]]))
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
  # arguments <- list(...)
  # write(arguments[[1]], file="prueba_2.csv")
  average = sapply(unique(data_names), calcf_data__, oks=data_names, data=data, operation=operation, ...)
  average[average==Inf | average==-Inf] = NA
  names(average) = unique(data_names)

  if(sum(grepl(SPRING, names(average)))>0){
    # average = data.frame(SPRING=average[grepl(SPRING, names(average))], SUMMER= average[grepl(SUMMER, names(average))],FALL=average[grepl(FALL, names(average))], WINTER=average[grepl(WINTER, names(average))])
    # rownames(average) = substr(rownames(average), nchar(rownames(average))-3, nchar(rownames(average)))
    average = array(c(average[grepl(SPRING, names(average))], average[grepl(SUMMER, names(average))], average[grepl(FALL, names(average))], average[grepl(WINTER, names(average))]), dim=c(length(average)/4, 4), dimnames=list(unique(substr(names(average), nchar(names(average))-3, nchar(names(average)))), c("SPRING", "SUMMER", "FALL", "WINTER")))
  }else{
    if(sum(grepl(JAN, names(average)))>0){
      # average = data.frame(JAN=average[grepl(JAN, names(average))], FEB= average[grepl(FEB, names(average))], MAR=average[grepl(MAR, names(average))], APR=average[grepl(APR, names(average))], MAY=average[grepl(MAY, names(average))], JUN=average[grepl(JUN, names(average))], JUL=average[grepl(JUL, names(average))], AUG= average[grepl(AUG, names(average))], SEP=average[grepl(SEP, names(average))], OCT=average[grepl(OCT, names(average))], NOV=average[grepl(NOV, names(average))], DEC=average[grepl(DEC, names(average))])
      # rownames(average) = substr(rownames(average), nchar(rownames(average))-3, nchar(rownames(average)))
      average = array(c(average[grepl(JAN, names(average))], average[grepl(FEB, names(average))], average[grepl(MAR, names(average))], average[grepl(APR, names(average))], average[grepl(MAY, names(average))], average[grepl(JUN, names(average))], average[grepl(JUL, names(average))], average[grepl(AUG, names(average))], average[grepl(SEP, names(average))], average[grepl(OCT, names(average))],average[grepl(NOV, names(average))], average[grepl(DEC, names(average))]), dim=c(length(average)/12, 12), dimnames=list(unique(substr(names(average), nchar(names(average))-3, nchar(names(average)))), c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")))
    }
  }
  return(average)
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

#' Data in reference interval
#' Does nothing
#' @param data data
#' @param date date
#' @return Data in reference interval
#' @examples
#' ref_data(data=tmax.value)
ref_data = function(data, date){
  return(data)
}

#' Mean data
#'
#' @param data data
#' @return mean
#' @examples
#' days_mean(data=tmax.value)
days_mean = function(data){
  return(meanf(data))
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

#' Calculate all indexes for all time scales
#'
#' @param tmin.value minimum temperature
#' @param tmax.value maximum temperature
#' @param taverage.value medium temperature
#' @param insolation.value sunshine duration
#' @param w.value average wind
#' @param w_max.value maximum wind gust
#' @param pr.value precipitation
#' @param snow.value snowfall
#' @param snow_depth.value snow depth
#' @param cloud_cover.value cloud cover
#' @param cloud_cover_less_100.value cloud base below 100 meter
#' @param radiation.value net radiation 
#' @param dew_point.value dew point
#' @param toa.value toa
#' @param lat latitude
#' @return all indexes
#' @export
#' @examples
#' calculate_all_scales(tmin.value=tmin.value, tmax.value=tmax.value, 
#' taverage.value=taverage.value, insolation.value=insolation.value,
#' w.value=w.value, w_max.value=w_max.value, pr.value=pr.value,
#' snow.value=snow.value, snow_depth.value=snow_depth.value,
#' cloud_cover.value=cloud_cover.value,
#' cloud_cover_less_100.value=cloud_cover_less_100.value,
#' radiation.value=radiation.value, dew_point.value=dew_point.value,
#' lat=lat)
calculate_all_scales = function(tmin.value=NULL, tmax.value=NULL, taverage.value=NULL, insolation.value=NULL, w.value=NULL, w_max.value=NULL, pr.value=NULL, snow.value=NULL, snow_depth.value=NULL, cloud_cover.value=NULL, cloud_cover_less_100.value=NULL, radiation.value=NULL, dew_point.value=NULL, toa.value=NULL, lat=NULL){
  data_year=calculate_all(tmin.value, tmax.value, taverage.value, insolation.value, w.value, w_max.value, pr.value, snow.value, snow_depth.value, cloud_cover.value, cloud_cover_less_100.value, radiation.value, dew_point.value, toa.value, lat, time.scale=YEAR)
  data_month=calculate_all(tmin.value, tmax.value, taverage.value, insolation.value, w.value, w_max.value, pr.value, snow.value, snow_depth.value, cloud_cover.value, cloud_cover_less_100.value, radiation.value, dew_point.value, toa.value, lat, time.scale=MONTH)
  data_season=calculate_all(tmin.value, tmax.value, taverage.value, insolation.value, w.value, w_max.value, pr.value, snow.value, snow_depth.value, cloud_cover.value, cloud_cover_less_100.value, radiation.value, dew_point.value, toa.value, lat, time.scale=SEASON)
  return(list(year=data_year, month=data_month, season=data_season))
}

index_result_names = c(paste0("index_", c(1:134)), "index_125_3", "index_125_6", "index_125_12", "index_124_3", "index_124_6", "index_124_12", "index_1_min", "index_3_min", "index_4_min", "index_1_mean", "index_3_mean", "index_4_mean")


#' Calculate all indexes
#'
#' @param tmin.value minimum temperature
#' @param tmax.value maximum temperature
#' @param taverage.value medium temperature
#' @param insolation.value sunshine duration
#' @param w.value average wind
#' @param w_max.value maximum wind gust
#' @param pr.value precipitation
#' @param snow.value snowfall
#' @param snow_depth.value snow depth
#' @param cloud_cover.value cloud cover
#' @param cloud_cover_less_100.value cloud base below 100 meter
#' @param radiation.value net radiation 
#' @param dew_point.value dew point
#' @param toa.value toa
#' @param lat latitude
#' @param time.scale month, season or year
#' @param data_names names of each period of time  
#' @return all indexes
#' @export
#' @examples
#' calculate_all(tmin.value=tmin.value, tmax.value=tmax.value,
#' taverage.value=taverage.value, insolation.value=insolation.value,
#' w.value=w.value, w_max.value=w_max.value, pr.value=pr.value,
#' snow.value=snow.value, snow_depth.value=snow_depth.value,
#' cloud_cover.value=cloud_cover.value,
#' cloud_cover_less_100.value=cloud_cover_less_100.value,
#' radiation.value=radiation.value, dew_point.value=dew_point.value,
#' lat=lat)
calculate_all = function(tmin.value=NULL, tmax.value=NULL, taverage.value=NULL, insolation.value=NULL, w.value=NULL, w_max.value=NULL, pr.value=NULL, snow.value=NULL, snow_depth.value=NULL, cloud_cover.value=NULL, cloud_cover_less_100.value=NULL, radiation.value=NULL, dew_point.value=NULL, toa.value=NULL, lat=NULL, time.scale=YEAR, data_names=NULL){
  
  no_null = function(){
   if(!is.null(tmin.value)){ return(tmin.value)
   }else{
      if(!is.null(tmax.value)){ return(tmax.value)
      }else{
        if(!is.null(taverage.value)){ return(taverage.value)
        }else{
          if(!is.null(insolation.value)){ return(insolation.value)
          }else{
            if(!is.null(w.value)){ return(w.value)
            }else{
              if(!is.null(w_max.value)){ return(w_max.value)
              }else{
                if(!is.null(pr.value)){ return(pr.value)
                }else{
                  if(!is.null(snow.value)){ return(snow.value)
                  }else{
                    if(!is.null(snow_depth.value)){ return(snow_depth.value)
                    }else{
                      if(!is.null(cloud_cover.value)){ return(cloud_cover.value)
                      }else{
                        if(!is.null(cloud_cover_less_100.value)){ return(cloud_cover_less_100.value)
                        }else{
                          if(!is.null(radiation.value)){ return(radiation.value)
                          }else{
                            if(!is.null(dew_point.value)){ return(dew_point.value)
                            }else{
                              if(!is.null(toa.value)){ return(toa.value)
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if(is.null(data_names)){
    data = no_null()
    date = chron(names(data))
    extract_names=select_time_function(time.scale)
    data_names = extract_names(date)
  }

  # calcf_data = function(data, extract_names, data_names, operation, ...){
  #   return(calcf_data_normal(data, extract_names, data_names, operation, ...))
  # }

  result_list = list()
  result_list[length(index_result_names)+1] = NA
  names(result_list) = index_result_names

  vapor.value = rh.value = eto.value = taverage.value = tci.value = NULL

  if(!is.null(dew_point.value)){
    vapor.value = td_to_vapor(dew_point.value)
  }
  if(!is.null(tmax.value) & !is.null(tmin.value) & !is.null(dew_point.value)){
    rh.value = td_to_rh(tmax=tmax.value, tmin=tmin.value, td=dew_point.value)
  }
  if(!is.null(tmin.value) & !is.null(tmax.value) & !is.null(insolation.value) & !is.null(w.value) & !is.null(lat) & !is.null(dew_point.value)){
    eto.value = calc_eto(tmin = tmin.value, tmax = tmax.value, insolation = insolation.value, w = w.value, lat=lat, tdew = dew_point.value)
  }
  if(is.null(taverage.value) & !is.null(tmax.value) & !is.null(tmin.value)){
    taverage.value = (tmax.value+tmin.value)/2
  }

  # system.time({calculate1(data=data, time.scale=time.scale)})
  # system.time({calculate1(data=data, data_names=data_names, time.scale=time.scale)})
  # extract_names=select_time_function(time.scale)
  # operation=meanf
  # system.time({ calcf_data(data, extract_names=extract_names, operation=operation) })

  # system.time({ date = chron(names(data)) })
  # system.time({ names = extract_names(date) })
  # system.time({ calcf_data_(names=names, data=data, operation=operation) })

  # Salidas mensuales, estacionales y anuales, ¿3 tablas? 
  if(!is.null(tmax.value)){
    if("index_1" %in% names(result_list)){#Media de la máxima
      result_list$index_1 = calculate1(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_3" %in% names(result_list)){
      result_list$index_3 = calculate3(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_4" %in% names(result_list)){
      result_list$index_4 = calculate4(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_8" %in% names(result_list)){
      result_list$index_8 = calculate8(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_16" %in% names(result_list)){
      result_list$index_16 = calculate16(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_19" %in% names(result_list)){
      result_list$index_19 = calculate19(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_20" %in% names(result_list)){
      result_list$index_20 = calculate20(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_21" %in% names(result_list)){
      result_list$index_21 = calculate21(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_25" %in% names(result_list)){
      result_list$index_25 = calculate25(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_26" %in% names(result_list)){
      result_list$index_26 = calculate26(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_28" %in% names(result_list)){
      result_list$index_28 = calculate28(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if("index_29" %in% names(result_list)){
      result_list$index_29 = calculate29(data=tmax.value, data_names=data_names, time.scale=time.scale)
    }
    if(time.scale==YEAR){
      if("index_36" %in% names(result_list)){
        result_list$index_36 = calculate36(data = tmax.value, data_names=data_names)
      }
      if("index_37" %in% names(result_list)){
       result_list$index_37 = calculate37(data = tmax.value, data_names=data_names)
      }
      if("index_38" %in% names(result_list)){
        result_list$index_38 = calculate38(data = tmax.value, data_names=data_names)
      }
      if("index_43" %in% names(result_list)){
        result_list$index_43 = calculate43(data = tmax.value, data_names=data_names)
      }
    }
  }
  if(!is.null(tmin.value)){
    if("index_1_min" %in% names(result_list)){
      result_list$index_1_min = calculate1(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_3_min" %in% names(result_list)){
      result_list$index_3_min = calculate3(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_4_min" %in% names(result_list)){
      result_list$index_4_min = calculate4(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_9" %in% names(result_list)){
      result_list$index_9 = calculate9(data=tmin.value, time.scale=time.scale)
    }
    if("index_10" %in% names(result_list)){
      result_list$index_10 = calculate10(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_11" %in% names(result_list)){
      result_list$index_11 = calculate11(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_14" %in% names(result_list)){
      result_list$index_14 = calculate14(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_17" %in% names(result_list)){
      result_list$index_17 = calculate17(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_22" %in% names(result_list)){
      result_list$index_22 = calculate22(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_23" %in% names(result_list)){
      result_list$index_23 = calculate23(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_24" %in% names(result_list)){
      result_list$index_24 = calculate24(data=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_27" %in% names(result_list)){
      result_list$index_27 = calculate27(data=tmin.value, time.scale=time.scale, data_names=data_names)
     }
    if(time.scale==YEAR){
      if("index_39" %in% names(result_list)){
        result_list$index_39 = calculate39(data = tmin.value, data_names=data_names)
      }
      if("index_40" %in% names(result_list)){
        result_list$index_40 = calculate40(data = tmin.value, data_names=data_names)
      }
    }
  }
  if(!is.null(taverage.value)){
    if("index_1_mean" %in% names(result_list)){
      result_list$index_1_mean = calculate1(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_3_mean" %in% names(result_list)){
      result_list$index_3_mean = calculate3(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_4_mean" %in% names(result_list)){
      result_list$index_4_mean = calculate4(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_15" %in% names(result_list)){
      result_list$index_15 = calculate15(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_31" %in% names(result_list)){
      result_list$index_31 = calculate31(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_32" %in% names(result_list)){
      result_list$index_32 = calculate32(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_35" %in% names(result_list)){
      result_list$index_35 = calculate35(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_78" %in% names(result_list)){
      result_list$index_78 = calculate78(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_69" %in% names(result_list)){
      result_list$index_69 = calculate69(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_70" %in% names(result_list)){
      result_list$index_70 = calculate70(data = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if(time.scale==YEAR){
      if("index_33" %in% names(result_list)){
        result_list$index_33 = calculate33(data = taverage.value, data_names=data_names)
      }
      if("index_34" %in% names(result_list)){
        result_list$index_34 = calculate34(data = taverage.value, data_names=data_names)
      }
      if("index_41" %in% names(result_list)){
        result_list$index_41 = calculate41(data = taverage.value, data_names=data_names)
      }
      if("index_42" %in% names(result_list)){
        result_list$index_42 = calculate42(data = taverage.value, data_names=data_names)
      }
      if("index_79" %in% names(result_list)){
        result_list$index_79 = calculate79(data = taverage.value, data_names=data_names)
      }
      if("index_80" %in% names(result_list)){
        result_list$index_80 = calculate80(data = taverage.value, data_names=data_names)
      }
      if("index_81" %in% names(result_list)){
        result_list$index_81 = calculate81(data = taverage.value, data_names=data_names)
      }
      if("index_100" %in% names(result_list)){
        result_list$index_100 = calculate100(data = taverage.value, data_names=data_names)
      }
    }
  }
  if(!is.null(tmax.value) & !is.null(tmin.value)){
    if("index_12" %in% names(result_list)){
      result_list$index_12 = calculate12(max=tmax.value, min=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_13" %in% names(result_list)){
      result_list$index_13 = calculate13(max=tmax.value, min=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_18" %in% names(result_list)){
      result_list$index_18 = calculate18(max=tmax.value, min=tmin.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_30" %in% names(result_list)){
      result_list$index_30 = calculate30(max=tmax.value, min=tmin.value, time.scale=time.scale, data_names=data_names)
    }
  }  
  if(!is.null(pr.value)){
    if("index_44" %in% names(result_list)){
      result_list$index_44 = calculate44(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_45" %in% names(result_list)){
      result_list$index_45 = calculate45(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_46" %in% names(result_list)){
      result_list$index_46 = calculate46(data = pr.value, time.scale=time.scale)
    }
    if("index_47" %in% names(result_list)){
      result_list$index_47 = calculate47(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_48" %in% names(result_list)){
      result_list$index_48 = calculate48(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_49" %in% names(result_list)){
      result_list$index_49 = calculate49(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_51" %in% names(result_list)){
      result_list$index_51 = calculate51(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_52" %in% names(result_list)){
      result_list$index_52 = calculate52(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_53" %in% names(result_list)){
      result_list$index_53 = calculate53(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_56" %in% names(result_list)){
      result_list$index_56 = calculate56(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_57" %in% names(result_list)){
      result_list$index_57 = calculate57(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_58" %in% names(result_list)){
      result_list$index_58 = calculate58(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_59" %in% names(result_list)){
      result_list$index_59 = calculate59(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_60" %in% names(result_list)){
      result_list$index_60 = calculate60(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_61" %in% names(result_list)){
      result_list$index_61 = calculate61(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_62" %in% names(result_list)){
      result_list$index_62 = calculate62(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_66" %in% names(result_list)){
      result_list$index_66 = calculate66(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_67" %in% names(result_list)){
      result_list$index_67 = calculate67(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_68" %in% names(result_list)){
      result_list$index_68 = calculate68(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_71" %in% names(result_list)){
      result_list$index_71 = calculate71(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_72" %in% names(result_list)){
      result_list$index_72 = calculate72(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_73" %in% names(result_list)){
      result_list$index_73 = calculate73(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_74" %in% names(result_list)){
      result_list$index_74 = calculate74(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_75" %in% names(result_list)){
      result_list$index_75 = calculate75(data = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if(time.scale==YEAR){
      if("index_50" %in% names(result_list)){
        result_list$index_50 = calculate50(data = pr.value, data_names=data_names)
      }
      if("index_63" %in% names(result_list)){
        result_list$index_63 = calculate63(data = pr.value, data_names=data_names)
      }
      if("index_64" %in% names(result_list)){
        result_list$index_64 = calculate64(data = pr.value, data_names=data_names)
      }
      if("index_65" %in% names(result_list)){
        result_list$index_65 = calculate65(data = pr.value, data_names=data_names)
      }
      if("index_124" %in% names(result_list)){
        result_list$index_124 = calculate124(data = pr.value, scale=1)
      }
      if("index_124_3" %in% names(result_list)){
        result_list$index_124_3 = calculate124(data = pr.value, scale=3)
      }
      if("index_124_6" %in% names(result_list)){
        result_list$index_124_6 = calculate124(data = pr.value, scale=6)
      }
      if("index_124_12" %in% names(result_list)){
        result_list$index_124_12 = calculate124(data = pr.value, scale=12)
      }
    }
  } 
  # if(!is.null(freezing.rain.value)){
    # if("calculate54" %in% names(result_list)){
      #  result_list$index_54 = calculate54(data = freezing.rain.value, time.scale=time.scale)
    # }
  # }
  if(!is.null(pr.value) & !is.null(eto.value)){
    if("index_55" %in% names(result_list)){
      result_list$index_55 = calculate55(pr = pr.value, eto = eto.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_95" %in% names(result_list)){
      result_list$index_95 = calculate95(eto = eto.value, pr = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_96" %in% names(result_list)){
      result_list$index_96 = calculate96(eto = eto.value, pr = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if(time.scale==YEAR){
      if("index_125" %in% names(result_list)){
        result_list$index_125 = calculate125(eto = eto.value, pr = pr.value, scale=1)
      }
      if("index_125_3" %in% names(result_list)){
        result_list$index_125_3 = calculate125(eto = eto.value, pr = pr.value, scale=3)
      }
      if("index_125_6" %in% names(result_list)){
        result_list$index_125_6 = calculate125(eto = eto.value, pr = pr.value, scale=6)
      }
      if("index_125_12" %in% names(result_list)){
        result_list$index_125_12 = calculate125(eto = eto.value, pr = pr.value, scale=12)
      }
    }
  }
  if(!is.null(pr.value) & !is.null(taverage.value)){
    if("index_76" %in% names(result_list)){
      result_list$index_76 = calculate76(pr=pr.value, taverage=taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_77" %in% names(result_list)){
      result_list$index_77 = calculate77(pr=pr.value, taverage=taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_82" %in% names(result_list)){
      result_list$index_82 = calculate82(taverage = taverage.value, pr = pr.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_83" %in% names(result_list)){
      result_list$index_83 = calculate83(taverage = taverage.value, pr = pr.value, time.scale=time.scale, data_names=data_names)
    }
    # if("index_84" %in% names(result_list)){
      # result_list$index_84 = calculate84(taverage = taverage.value, pr = pr.value, time.scale=time.scale)
    # }
    if("index_97" %in% names(result_list)){
      result_list$index_97 = calculate97(pr = pr.value, taverage = taverage.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_98" %in% names(result_list)){
      result_list$index_98 = calculate98(taverage = taverage.value, pr = pr.value, data_names=data_names)
    }
    if("index_101" %in% names(result_list)){
      result_list$index_101 = calculate101(pr = pr.value, taverage = taverage.value, data_names=data_names)
    }
    if("index_127" %in% names(result_list)){
      result_list$index_127 = calculate127(taverage = taverage.value, pr=pr.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(taverage.value) & !is.null(lat)){
    if(time.scale==YEAR){
      if("index_99" %in% names(result_list)){
        result_list$index_99 = calculate99(data = taverage.value, value = lat, data_names=data_names)
      }
      if("index_103" %in% names(result_list)){
        result_list$index_103 = calculate103(data = taverage.value, value = lat, data_names=data_names)
      }
    }
  }
  if(!is.null(w.value)){
    if("index_89" %in% names(result_list)){
      result_list$index_89 = calculate89(data = w.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_91" %in% names(result_list)){
      result_list$index_91 = calculate91(data = w.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_92" %in% names(result_list)){
      result_list$index_92 = calculate92(data = w.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_93" %in% names(result_list)){
      result_list$index_93 = calculate93(data = w.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(taverage.value) & !is.null(rh.value)){
    if("index_85" %in% names(result_list)){
      result_list$index_85 = calculate85(taverage = taverage.value, rh = rh.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_86" %in% names(result_list)){
      result_list$index_86 = calculate86(taverage = taverage.value, rh = rh.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(taverage.value) & !is.null(w.value)){
    if("index_87" %in% names(result_list)){
      result_list$index_87 = calculate87(taverage = taverage.value, w = w.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(taverage.value) & !is.null(w.value) & !is.null(vapor.value)){
    if("index_88" %in% names(result_list)){
      result_list$index_88 = calculate88(taverage = taverage.value, w = w.value, vapor = vapor.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(w_max.value)){
    if("index_90" %in% names(result_list)){
      result_list$index_90 = calculate90(data = w_max.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(eto.value)){
    if(time.scale==YEAR){
      if("index_94" %in% names(result_list)){
        result_list$index_94 = calculate94(data = eto.value, data_names=data_names)
      }
    }
  }
  if(!is.null(radiation.value) & !is.null(pr.value)){
    if("index_102" %in% names(result_list)){
      result_list$index_102 = calculate102(data = radiation.value, pr = pr.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(snow.value)){
    if("index_104" %in% names(result_list)){
      result_list$index_104 = calculate104(data = snow.value, time.scale=time.scale, data_names=data_names)
    }    
    if("index_108" %in% names(result_list)){
      result_list$index_108 = calculate108(data = snow.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(snow_depth.value)){
    if("index_105" %in% names(result_list)){
      result_list$index_105 = calculate105(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_106" %in% names(result_list)){
      result_list$index_106 = calculate106(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_107" %in% names(result_list)){
      result_list$index_107 = calculate107(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_109" %in% names(result_list)){
      result_list$index_109 = calculate109(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_110" %in% names(result_list)){
      result_list$index_110 = calculate110(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_111" %in% names(result_list)){
      result_list$index_111 = calculate111(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_112" %in% names(result_list)){
      result_list$index_112 = calculate112(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_113" %in% names(result_list)){
      result_list$index_113 = calculate113(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_114" %in% names(result_list)){
      result_list$index_114 = calculate114(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_115" %in% names(result_list)){
      result_list$index_115 = calculate115(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_116" %in% names(result_list)){
      result_list$index_116 = calculate116(data = snow_depth.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(cloud_cover.value)){
    if("index_118" %in% names(result_list)){
      result_list$index_118 = calculate118(data = cloud_cover.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_120" %in% names(result_list)){
      result_list$index_120 = calculate120(data = cloud_cover.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(cloud_cover_less_100.value)){
    if("index_119" %in% names(result_list)){
      result_list$index_119 = calculate119(data = cloud_cover_less_100.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(insolation.value)){
    if("index_117" %in% names(result_list)){
      result_list$index_117 = calculate117(data = insolation.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_121" %in% names(result_list)){
      result_list$index_121 = calculate121(data = insolation.value, time.scale=time.scale, data_names=data_names)
    }
    if("index_122" %in% names(result_list)){
      result_list$index_122 = calculate122(data = insolation.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(radiation.value) & !is.null(toa.value)){
    if("index_123" %in% names(result_list)){
      result_list$index_123 = calculate123(data = radiation.value, toa=toa.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(taverage.value) & !is.null(rh.value) & !is.null(w.value) & !is.null(pr.value) & !is.null(lat)){
    if("index126" %in% names(result_list)){
      result_list$index126 = calculate126(taverage=taverage.value, rh = rh.value, w = w.value, pr = pr.value, lat = lat, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(taverage.value) & !is.null(rh.value) & !is.null(w.value) & !is.null(pr.value)){
    if("index_128" %in% names(result_list)){
      result_list$index_128 = calculate128(taverage = taverage.value, pr=pr.value, rh=rh.value, w=w.value, time.scale=time.scale, data_names=data_names)
    }
  }
  if(!is.null(dew_point.value) & !is.null(taverage.value) & !is.null(pr.value)){
    if("index_129" %in% names(result_list)){
      result_list$index_129 = calculate129(dew_point=dew_point.value, taverage=taverage.value, pr=pr.value, time.scale=time.scale, data_names=data_names)
    }
  }
  # if(!is.null(radiation.value) & !is.null(toa.value)){
  #   if("index_130" %in% names(result_list)){
  #     result_list$index_130 = calculate130(data = radiation.value, toa=toa.value, time.scale=time.scale, data_names=data_names)
  #   }
  # }
  # if(!is.null(pr.value) & !is.null(w.value)){
  #   if("index_131" %in% names(result_list)){
  #    result_list$index_131 = calculate131(pr = pr.value, w=w.value, time.scale=time.scale, data_names=data_names)
  #   }
  # }
  # if(!is.null(pr.value) & !is.null(radiation.value) & !is.null(w.value)){
  #     if("index_132" %in% names(result_list)){
  #     result_list$index_132 = tci.value = calculate132(pr=pr.value, sunshine=radiation.value, w=w.value, time.scale=time.scale, data_names=data_names)  
  #     }
  # }
  # if(!is.null(tci.value)){
  #   if("index_133" %in% names(result_list)){
  #     result_list$index_133 = calculate133(data = tci.value, time.scale=time.scale, data_names=data_names)
  #       }
  #   if("index_134" %in% names(result_list)){
  #     result_list$index_134 = calculate134(data = tci.value, time.scale=time.scale, data_names=data_names)
  #     }
  # }

  # Eliminar índices no calculados
  result_list = result_list[-c(which(sapply(result_list, is.null)), length(result_list))]

  return(result_list)

}

# Datos diarios

####Temperature-based
#' 1. Average temperature (maximum, minimum and mean)
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' calculate1(data=tmax.value)
average_temp = calculate1 = function(data, data_names=NULL, time.scale=YEAR){
  return(calcf_data(data, extract_names=select_time_function(time.scale), data_names=data_names, operation=meanf))
}

#' 1. Average maximum temperature
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' mean_tx(data=tmax.value)
mean_tx = calculate1 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data, data_names=data_names, time.scale=time.scale))

}

#' 1. Average minimum temperature
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' mean_tn(data=tmin.value)
mean_tn = calculate1 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data, data_names=data_names, time.scale=time.scale))
}

#' 1. Average mean temperature
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' mean_tg(data=tmean.value)
mean_tg = calculate1 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data, data_names=data_names, time.scale=time.scale))

}

#' 3. Maximum temperature (for maximum, minimum and mean)
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year  
#' @return maximum temperature
#' @export
#' @examples
#' calculate3(data=tmax.value)
maximum_temp = calculate3 = function(data, data_names=NULL, time.scale=YEAR){
  return(calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=maxf))
}

#' 4. Minimum temperature (for maximum, minimum and mean)
#' 
#' @param data maximum, minimum or medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return minimum temperature
#' @export
#' @examples
#' calculate4(data=tmax.value)
minimum_temp = calculate4 = function(data, data_names=NULL, time.scale=YEAR){
  return(calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=minf))
}

#' 8. Cold days
#' Percentages of days with maximum temperatures lower than the 10th percentile.
#' 
#' @param data maximum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return Cold days
#' @export
#' @examples
#' calculate8(data=tmax.value)
col_ddays = calculate8 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.10), na.rm = TRUE)
  function_ = function(data, value){    
    return(100*sumf(data<value)/length(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 9. Cold nights
#' Percentages of days with minimum temperatures lower than the 10th percentile.
#' 
#' @param data minimum temperature
#' @param date date
#' @param time.scale month, season or year
#' @return Cold nights
#' @export
#' @examples
#' calculate9(data=tmin.value)
cold_nights = calculate9 = function(data, date=NA, time.scale=YEAR){
  data.q = quantile(ref_data(data, date), c(.10), na.rm = TRUE)
  function_ = function(data, value){    
    return(100*sumf(data<value)/length(data))
  }
  byYears = calcf_data(data=data, date=date, extract_names=select_time_function(time.scale), operation=function_, value=data.q)
  return(byYears)
}

# library(compiler)
# cold_nights = cmpfun(cold_nights)

#' 10. Cold spell duration index
#' Annual count of days with at least 6 consecutive days when TN < 10th percentile #Tn = tmin
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Cold spell duration index
#' @export
#' @examples
#' calculate10(data=tmin.value)
cold_spell_duration_index = calculate10 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.10), na.rm = TRUE)
  # Count of days with at least 6 consecutive days when TN < 10th percentile #Tn = tmin
  # Ej. days_least_10(data=unique(years(names(tmin.value)))[1], names=years(names(tmin.value)), value=data.q)
  function_ = function(data, value){
    data.10 = data <= value
    data.rle = rle(as.numeric(data.10))
    aux = data.rle$values==1 & data.rle$lengths>=6
    count = sum(data.rle$lengths[aux])
    return(count)
  }

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)

  return(byYears)
}

#' 11. Coldest night (TNn)
#' Lowest daily minimum temperature at the annual and monthly scales
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return TNn
#' @export
#' @examples
#' calculate11(data=tmin.value)
TNn = calculate11 = function(data, data_names=NULL, time.scale=YEAR){
  return(calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=minf))
}

#' 12. Diurnal temperature range
#' Mean difference between TX and TN.
#'
#' @param max maximum temperature 
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Diurnal temperature range
#' @export
#' @examples
#' calculate12(max=tmax.value, min=tmin.value)
diurnal_temperature_range = calculate12 = function(max, min, data_names=NULL, time.scale=YEAR){
  data = max - min
  byMonths = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=days_mean)
  return(byMonths)
}

#' 13. vDTR
#' Mean absolute day-to-day difference in DTR (annual and monthly)
#'
#' @param max maximum temperature 
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return vDTR
#' @export
#' @examples
#' calculate13(max=tmax.value, min=tmin.value)
vDTR = calculate13 = function(max, min, data_names=NULL, time.scale=YEAR){
  data = diurnal_temperature_range(max, min, data_names, time.scale)
  data = abs(data[1:(length(data)-1)]-data[2:length(data)])

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=days_mean)
  return(byYears)
}

#' 14. Frost days
#' Number of days with minimum temperature <0ºC per year.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return frost days
#' @export
#' @examples
#' calculate14(data=tmin.value)
frost_days = calculate14 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sum(data<0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 15. Growing season length
#' Annual count of days between the first span of at least 6 days with Tmean >5ºC and first span after 1 July of 6 days with Tmean <5 ºC.
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return growing season length
#' @export
#' @examples
#' calculate15(data=taverage.value)
growing_season_length = calculate15 = function(data, data_names=NULL, time.scale=YEAR){
  # Days between the first span of at least 6 days with Tmean >5ºC and first span after 1 July of 6 days with Tmean <5 ºC
  # Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    BREAKVALUE = 5
    times.data = chron(names(data))
    aux = which(times.data == chron(paste0("7/1/", unique(years(times.data)))))
    data.max = which(data>BREAKVALUE)[1] #Mayores que 5
    data.min = data<BREAKVALUE #Menores que 5
    data.min[1:length(data.min)<=aux] = FALSE# Menores que 5 después del 1 de julio
    data.min.rle = rle(as.numeric(data.min))
    aux = which(data.min.rle$length>6 & data.min.rle$values==1)[1]
    if(is.na(aux) | length(aux)<=0 | length(data.max)<=0){
      count = 0
    }else{
      data.min = sum(data.min.rle$length[1:aux])
      count = data.min-data.max
    }
    return(count)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 16. Ice days
#' Number of days with maximum temperature <0ºC per year.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return ice days
#' @export
#' @examples
#' calculate16(data=tmax.value)
ice_days = calculate16 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sum(data<0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 17. Maximum number of consecutive frost days (CFD)
#' Annual and monthly
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return maximum consecutive frost
#' @export
#' @examples
#' calculate17(data=tmin.value)
maximum_consecutive_frost = calculate17 = function(data, data_names=NULL, time.scale=YEAR){
  # Ej. function_(data=tmin.value[unique(years(names(tmin.value)))[12]==years(names(tmin.value))])
  function_ = function(data){
    VALUE = 0
    data.rle = rle(as.numeric(data<VALUE))  
    calculate = 0  
    if(sumf(data.rle$values>0)>0){
      calculate = maxf(data.rle$lengths[data.rle$values>0])
    }
    return(calculate)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 18. Intra-annual extreme temperature range
#' Difference between the highest TX and the lowest TN in the year.
#' 
#' @param max maximum temperature
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return extreme temperature range
#' @export
#' @examples
#' calculate18(max=tmax.value, min=tmin.value)
extreme_temperature_range = calculate18 = function(max, min, data_names=NULL, time.scale=YEAR){
  # Ej. function_(max=max[unique(years(names(max)))[12]==years(names(max))], min=min[unique(years(names(min)))[12]==years(names(min))])
  function_ = function(max, min){
    calculate = maxf(max)-minf(min) 
    return(calculate)
  }
  data = cbind(max, min)
  byYears = calcf_data(data=max, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, min=min)
  return(byYears)
}

#' 19. Summer days
#' Number of days with maximum temperature >25ºC per year.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Summer days
#' @export
#' @examples
#' calculate19(data=tmax.value)
summer_days = calculate19 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sum(data>25))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 20. Maximum number of consecutive summer days (TX > 25º)
#' Monthly and annual
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return consecutive summer days
#' @export
#' @examples
#' calculate20(data=tmax.value)
consecutive_summer_days = calculate20 = function(data, data_names=NULL, time.scale=YEAR){
  # Ej. function_(data=tmax.value[unique(years(names(tmax.value)))[12]==years(names(tmax.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data>25))
    calculate = maxf(data.rle$lengths[data.rle$values>0])
    if(calculate<0){
      calculate=0
    }
    return(calculate)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 21. Temperature sums
#' Sum of Tmax days >17ºC–days Tmax <17ºC ºC
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Temperature sums
#' @export
#' @examples
#' calculate21(data=tmax.value)
temperature_sums = calculate21 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sum(data>17)-sum(data<17))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 22. Tropical nights
#' Number of days with minimum temperature >20ºC per year.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Tropical nights
#' @export
#' @examples
#' calculate22(data=tmin.value)
tropical_nights = calculate22 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){  
    return(sum(data>20))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 23. Heating degree days (HD17)
#' Monthly and annual (sum(Cº-17))
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return HD17
#' @export
#' @examples
#' calculate23(data=tmin.value)
HD17 = calculate23 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sum(data > 17))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 24. Very cold days
#' Number of days with minimum temperature <1st percentile per year.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Very cold days
#' @export
#' @examples
#' calculate24(data=tmin.value)
very_cold_days = calculate24 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sum(data<value))
  }
  value = quantile(data, c(.1), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 25. Very warm days
#' Number of days with maximum temperature >99th percentile per year.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Very warm days
#' @export
#' @examples
#' calculate25(data=tmax.value)
very_warm_days = calculate25 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sum(data>value))
  }
  value = quantile(data, c(.99), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 26. Warm days
#' Percentages of days with maximum temperatures higher than the 90th percentile.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Warm days
#' @export
#' @examples
#' calculate26(data=tmax.value)
warm_days = calculate26 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sum(data>value))
  }
  value = quantile(data, c(.90), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 27. Warm nights
#' Percentages of days with minimum temperatures higher than the 90th percentile.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Warm nights
#' @export
#' @examples
#' calculate27(data=tmin.value)
warm_nights = calculate27 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sum(data>value))
  }
  value = quantile(data, c(.90), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 28. Warm spell duration index
#' Annual count of days with at least 6 consecutive days when TX > 90th percentile
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Warm spell duration index
#' @export
#' @examples
#' calculate28(data=tmax.value)
warm_spell_duration_index = calculate28 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    data.rle = rle(as.numeric(data>value))
    aux = data.rle$values==1 & data.rle$lengths>=6
    count = sum(data.rle$lengths[aux])
    return(count)
  }
  value = quantile(data, c(.90), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 29. Warmest day (TXn)
#' Highest daily maximum temperature at the annual and monthly scales
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return TXn
#' @export
#' @examples
#' calculate29(data=tmax.value)
TXn = calculate29 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 30. zero crossing days
#' Number of days with Tmax > 0 ºC and Tmin < 0 ºC.
#' 
#' @param max maximum temperature
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return zero crossing days
#' @export
#' @examples
#' calculate30(max=tmax.value, min=tmin.value)
zero_crossing = calculate30 = function(max, min, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sum(data))
  }
  data = max>0 & min<0
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 31. Onset of growing season 1
#' The start of the first span with at least 6 days with Tmean >5ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Onset of growing season 1
#' @export
#' @examples
#' calculate31(data=taverage.value)
onset_growing_1 = calculate31 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data>5))
    aux = which(data.rle$values==1 & data.rle$lengths>=6)[1]
    count = sum(data.rle$lengths[1:aux])-data.rle$lengths[aux]+1
    return(count)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 32. Onset of growing season 2
#' The start of the first span with at least 10 days with Tmean >5ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Onset of growing season 2
#' @export
#' @examples
#' calculate32(data=taverage.value)
onset_growing_2 = calculate32 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data>5))
    aux = which(data.rle$values==1 & data.rle$lengths>=10)[1]
    count = sum(data.rle$lengths[1:aux])-data.rle$lengths[aux]+1
    return(count)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 33. Growing season temperature 1
#' @param data_names names of each period of time
#' Growing season (april to october) mean temperature
#' 
#' @param data medium temperature
#' @return Growing season temperature 1
#' @export
#' @examples
#' calculate33(data=taverage.value)
growing_temp_1 = calculate33 = function(data, data_names=NULL){
  #Ej. maxq(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(APR,MAY, JUN, JUL, AUG, SEP, OCT)]
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 34. Growing season temperature 2
#' Growing season (may to september) mean temperature
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Growing season temperature 2
#' @export
#' @examples
#' calculate34(data=taverage.value)
growing_temp_2 = calculate34 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(MAY, JUN, JUL, AUG, SEP)]
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 35. Growing degree days (GD4)
#' Sum of degree days over 4ºC (annual and monthly) (sum Cº-4)
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return GD4
#' @export
#' @examples
#' calculate35(data=taverage.value)
GD4 = calculate35 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    return(sumf(data>4))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 36. Winkler index
#' Sum of degree days over 10°C from April 1 until October 31 = Sum max [(avg. daily temp. – 10), 0]
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return Winkler index
#' @export
#' @examples
#' calculate36(data = tmax.value)
 winkler_index = calculate36 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){  
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]
    return(sumf(data>10))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 37. Winter Severity index
#' Mean temperature of the coldest month of the year
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return Winter Severity index
#' @export
#' @examples
#' calculate37(data = tmax.value)
winter_severity_index = calculate37 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){  
    byMonths = calcf_data(data=data, extract_names=months, operation=meanf)
    return(byMonths[byMonths==minf(byMonths)])
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 38.1 Temperature sums 1
#' Sums of maximum temperatures >= 32ºC in June, July, August or from June to August
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param selectTime Jun, Jul, Aug or from Jun to Aug (summer)
#' @return temperature sums 1
#' @export
#' @examples
#' calculate38.1_time(data = tmax.value, selectTime=JUN)
temperature_sums_1.1_time = calculate38.1.time = function(data, data_names=NULL, selectTime){
  #Ej. function_1(data=tmax.value[unique(years(names(tmax.value)))[12]==years(names(tmax.value))], month=JUN)
  function_1 = function(data, month){
    data = data[months(chron(names(data)))%in%c(month)]
    return(sum(data[data>=32]))
  }
  if(selectTime=="summer"){
    selectTime=c(JUN, JUL, AUG)
  }
  sum.jun = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_1, month=selectTime)  
  return(sum.jun)
}

#' 38.1 Temperature sums 1
#' Sums of maximum temperatures >= 32ºC in June, July, August and from June to August
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return temperature sums 1
#' @export
#' @examples
#' calculate38.1(data = tmax.value)
temperature_sums_1.1 = calculate38.1 = function(data, data_names=NULL){
  #Ej. function_1(data=tmax.value[unique(years(names(tmax.value)))[12]==years(names(tmax.value))], month=JUN)
  function_1 = function(data, month){
    data = data[months(chron(names(data)))%in%c(month)]
    return(sum(data[data>=32]))
  }
  sum.jun = temperature_sums_1.1_time(data, data_names=data_names, selectTime=JUN)
  sum.jul = temperature_sums_1.1_time(data, data_names=data_names, selectTime=JUL)
  sum.aug = temperature_sums_1.1_time(data, data_names=data_names, selectTime=AUG)
  sum.jja = temperature_sums_1.1_time(data, data_names=data_names, selectTime=SUMMER)
  
  return(data.frame("Jun"=sum.jun, "Jul"=sum.jul, "Aug"=sum.aug, "JunToAug"=sum.jja))
}

#' 38.2. Temperature average 1
#' Average number of days whith maximum temperature >= 32ºC in June, July, August and from June to August
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param selectTime Jun, Jul, Aug or from Jun to Aug (summer)
#' @return temperature sums 1
#' @export
#' @examples
#' calculate38.2_time(data = tmax.value, selectTime=JUN)
temperature_average_1.2_time = calculate38.2_time = function(data, data_names=NULL, selectTime){
  #Ej. function_2(data=tmax.value[unique(years(names(tmax.value)))[12]==years(names(tmax.value))], month=JUN)
  function_2 = function(data, month){
    data = data[months(chron(names(data)))%in%c(month)]
    return(sum(data>=32)) #/length(data)
  }
  if(selectTime==SUMMER){
    selectTime=c(JUN, JUL, AUG)
  }
  average.jun = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_2, month=selectTime)
  return(average.jun)
}

#' 38.2. Temperature average 1
#' Average number of days whith maximum temperature >= 32ºC in June, July, August and from June to August
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return temperature sums 1
#' @export
#' @examples
#' calculate38.2(data = tmax.value)
temperature_average_1.2 = calculate38.2 = function(data, data_names=NULL){
  #Ej. function_2(data=tmax.value[unique(years(names(tmax.value)))[12]==years(names(tmax.value))], month=JUN)
  function_2 = function(data, month){
    data = data[months(chron(names(data)))%in%c(month)]
    return(sum(data>=32)) #/length(data)
  }
  average.jun = temperature_average_1.2_time(data, data_names=data_names, selectTime=JUN)
  average.jul = temperature_average_1.2_time(data, data_names=data_names, selectTime=JUL)
  average.aug = temperature_average_1.2_time(data, data_names=data_names, selectTime=AUG)
  average.jja = temperature_average_1.2_time(data, data_names=data_names, selectTime=SUMMER)
  return(data.frame("Jun"=average.jun, "Jul"=average.jul, "Aug"=average.aug, "JunToAug"=average.jja))
}

#' 38. Temperature sums 1
#' Sums of maximum temperatures >= 32ºC and average number of days whith maximum temperature >= 32ºC in June, July, August and from June to August
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return temperature sums 1
#' @export
#' @examples
#' calculate38(data = tmax.value)
temperature_sums_1 = calculate38 = function(data, data_names=NULL){  
  return(data.frame(max=calculate38.1(data, data_names=data_names), average=calculate38.2(data, data_names=data_names)))
}

#' 39. Temperature sums 2
#' Sums of minimum air temperatures <= -15ºC recorded in December-February interval
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @return temperature sums 2
#' @export
#' @examples
#' calculate39(data = tmin.value)
temperature_sums_2 = calculate39 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sumf(data <= -15))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}


#' 40. Temperature sums 3
#' Sums of minimum air temperatures <=-10ºC recorded in December-February interval
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @return temperature sums 3
#' @export
#' @examples
#' calculate40(data = tmin.value)
temperature_sums_3 = calculate40 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sumf(data <= -10))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 41. Temperature sums 4
#' The average daily amount of negative air temperature (sum of Tmed <= 0ºC/ cold units) recorded in the November to March period 
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return temperature sums 4
#' @export
#' @examples
#' calculate41(data = taverage.value)
temperature_sums_4 = calculate41 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(NOV, DEC, JAN, FEB, MAR)]
    return(sumf(data <= 0))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 42. Temperature sums 5
#' Sums of positive average temperatures calculated for the 1st of February to the 10th April interval
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return temperature sums 5
#' @export
#' @examples
#' calculate42(data = taverage.value)
temperature_sums_5 = calculate42 = function(data, data_names=NULL){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    time = chron(names(data))
    t.ini = chron(paste("2", "1", unique(years(time)), sep="/"))
    t.end = chron(paste("4", "10", unique(years(time)), sep="/"))
    data = data[chron(names(data))>=t.ini & chron(names(data))<=t.end]
    return(sumf(data[data >= 0]))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 43. Temperature sums 6
#' Number of consecutive days whith maximum temperature >= 32ºC from June to August
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return temperature sums 6
#' @export
#' @examples
#' calculate43(data = tmax.value)
temperature_sums_6 = calculate43 = function(data, data_names=NULL){
  #Ej. function_(data=tmax.value[unique(years(names(tmax.value)))[12]==years(names(tmax.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(JUN, JUL, AUG)]
    data = data>=32
    data.rle = rle(as.numeric(data))
    return(maxf(data.rle$lengths[data.rle$values == 1]))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

####Precipitation-based
#' 44. Total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return total precipitation
#' @export
#' @examples
#' calculate44(data = pr.value, time.scale=YEAR)
total_precipitation = calculate44 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 45. Maximum precipitation of the month, season and year
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return maximum precipitation
#' @export
#' @examples
#' calculate45(data = pr.value)
 maximum_precipitation = calculate45 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 46. R10mm
#' Annual count of days when daily precipitation amount >= 10mm
#' 
#' @param data precipitation
#' @param date date
#' @param time.scale month, season or year
#' @return R10mm
#' @export
#' @examples
#' calculate46(data = pr.value)
r10mm = calculate46 = function(data, date, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=10)) #mm
  }
  byYears = calcf_data(data=data, date=date, extract_names=select_time_function(time.scale), operation=function_)
  return(byYears)
}

#' 47. R20mm
#' Annual count of days when daily precipitation amount >= 20mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return R20mm
#' @export
#' @examples
#' calculate47(data = pr.value)
R20mm = calculate47 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=20)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 48. R95pTOT
#' Annual total PRCP when RR > 95p
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return R95pTOT
#' @export
#' @examples
#' calculate48(data = pr.value)
R95pTOT = calculate48 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.95), na.rm = TRUE)
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], value=data.q)
  function_ = function(data, value){
    return(sumf(data[data>value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 49. R99pTOT
#' Annual total PRCP when RR > 99p
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return R99pTOT
#' @export
#' @examples
#' calculate49(data = pr.value)
R99pTOT = calculate49 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.99), na.rm = TRUE)
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], value=data.q)
  function_ = function(data, value){
    return(sumf(data[data>value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 50. Rx1day
#' Monthly maximum 1-day precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return Rx1day
#' @export
#' @examples
#' calculate50(data = pr.value)
Rx1day = calculate50 = function(data, data_names=NULL){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data, value){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=months_years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 51. Rx5day
#' Monthly maximum consecutive 5-day precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Rx5day
#' @export
#' @examples
#' calculate51(data = pr.value)
Rx5day = calculate51 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    data2 = c(data[c(2:length(data))], 0)
    data3 = c(data[c(3:length(data))], 0, 0)
    data4 = c(data[c(4:length(data))], 0, 0, 0)
    data5 = c(data[c(5:length(data))], 0, 0, 0, 0)
    data.sum = data + data2 + data3 + data4 + data5
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 52. SDII
#' Simple precipitation intensity index
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SDII
#' @export
#' @examples
#' calculate52(data = pr.value)
SDII = calculate52 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data)/length(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 53. Dry days
#' The annual number of days with less than 1 mm/day
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return dry days
#' @export
#' @examples
#' calculate53(data = pr.value)
dry_days = calculate53 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data < 1))  #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 54. Freezing rain
#' The annual number of days with freezing rain
#' 
#' @param data freezing rain
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return freezing rain days
#' @export
#' @examples
#' calculate54(data = freezing.rain.value)
freezing_rain = calculate54 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=freezing.rain.value[unique(years(names(freezing.rain.value)))[12]==years(names(freezing.rain.value))])
  function_ = function(data){
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


#' 55. Effective precipitation
#' precipitation minus evapotranspiration
#' 
#' @param pr precipitation
#' @param eto et0
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return effective precipitation
#' @export
#' @examples
#' calculate55(pr = pr.value, eto = eto.value)
effective_precipitation = calculate55 = function(pr, eto, data_names=NULL, time.scale=YEAR){
  #Ej. function_(pr=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], eto=eto.value)
  function_ = function(pr, eto){
    eto = eto[names(eto)%in%names(pr)]
    return(sum(pr-eto))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, eto=eto)
  return(byYears)
}

#' 56. Longest dry period
#' maximum length of consecutive dry days (pp<1)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return longest dry period
#' @export
#' @examples
#' calculate56(data = pr.value)
longest_dry_period = calculate56 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data<1)) #mm
    count = 0
    if(sumf(data.rle$values==1)>0){
      count = maxf(data.rle$lengths[data.rle$values==1])
    }
    return(count)  
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 57. Longest wet period
#' @param data_names names of each period of time
#' maximum length of consecutive wet days (pp>=1)
#' 
#' @param data precipitation
#' @param time.scale month, season or year
#' @return longest wet period
#' @export
#' @examples
#' calculate57(data = pr.value)
longest_wet_period = calculate57 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data>=1)) #mm
    count = 0
    if(sumf(data.rle$values==1)>0){
      count = maxf(data.rle$lengths[data.rle$values==1])
    }
    return(count)  
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 58. Precipitation fraction due to very wet days
#' Precipitation at days exceeding the 95percentile divided by total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return PFDVWD
#' @export
#' @examples
#' calculate58(data = pr.value)
PFDVWD = calculate58 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.95), na.rm = TRUE)
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], value=data.q)
  function_ = function(data, value){    
    return(sumf(data[data>value])/sumf(data[data<=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 59. Precipitation fraction due to extremely wet days
#' Precipitation at days exceeding the 99percentile divided by total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return PFDEWD
#' @export
#' @examples
#' calculate59(data = pr.value)
PFDEWD = calculate59 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.99), na.rm = TRUE)
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], value=data.q)
  function_ = function(data, value){    
    return(sumf(data[data>value])/sumf(data[data<=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}


#' 60. Heavy precipitation days
#' number of days with precipitation above 50mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return heavy precipitation days
#' @export
#' @examples
#' calculate60(data = pr.value)
heavy_precipitation_days = calculate60 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sumf(data>value)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=50)
  return(byYears)
}


#' 61. R95p
#' number of days with precipitation above 95 percentile
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return R95p
#' @export
#' @examples
#' calculate61(data = pr.value)
R95p = calculate61 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.95), na.rm = TRUE)
  function_ = function(data, value){    
    return(sumf(data>value)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 62. iAnnual/seasonal Precipitation Concentration Index
#' PCI=100*sum(P)/P^2 (foto en el xlsx)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Precipitation Concentration Index
#' @export
#' @examples
#' calculate62(data = pr.value)
precipitation_concentration_index = calculate62 = function(data, data_names=NULL, time.scale=YEAR){
  function_year = function(data){      
    return(100*sumf(data))
  }
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))], value=byYears)
  function_ = function(data, value){
    value = value[as.character(unique(years(chron(names(data)))))]
    return(value/((sumf(data))^2))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_year)
  return(byYears)
}

#' 63. iModified Fournier Index
#' a precipitation concentration index
#' https://es.scribd.com/document/76414093/modified-fournier-index
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return iModified Fournier Index
#' @export
#' @examples
#' calculate63(data = pr.value)
MFI = iModified_fournier_index = calculate63 = function(data, data_names=NULL){
  #Ej. result = function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    function_ = function(data){
      return(sumf(data))
    }
    byMonths = calcf_data(data=data, extract_names=months_years, operation=function_)
    data2Sum = sumf(byMonths^2)
    dataSum2 = sumf(byMonths)^2
    return(data2Sum/dataSum2)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 64. Growing season precipitation
#' Growing season (april to october) mean precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return growing precipitation
#' @export
#' @examples
#' calculate64(data = pr.value)
growing_precipitation = calculate64 = function(data, data_names=NULL){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(APR,MAY, JUN, JUL, AUG, SEP, OCT)]    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 65. Non-growing season precipitation
#' October to april total precipitation, can inform on the resource available for low potential evaporation conditions
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return non growing precipitation
#' @export
#' @examples
#' calculate65(data = pr.value)
non_growing_precipitation = calculate65 = function(data, data_names=NULL){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))])
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(OCT,NOV, DEC, FEB, MAR, APR)]
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 66. Total precipitation in wet days
#' Precipitation amount on days with RR >= 1 mm in a choosen period (e.g. year)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return precipitation in wet days
#' @export
#' @examples
#' calculate66(data = pr.value)
precipitation_wet_days = calculate66 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))])
  function_ = function(data){
    return(sumf(data[data>=1])) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 67. RR1: 
#' Wet days >= 1 mm (days), ECA&D standard
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return RR1
#' @export
#' @examples
#' calculate67(data = pr.value)
RR1 = calculate67 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=1)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 68. RR3: 
#' Wet days >= 3mm (days), ECA&D standard
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return RR3
#' @export
#' @examples
#' calculate68(data = pr.value)
RR3 = calculate68 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=3)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

####Bioclimatic
#' 69. BIO10
#' Mean Temperature of Warmest Quarter
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO10
#' @export
#' @examples
#' calculate69(data = taverage.value)
BIO10 = calculate69 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.75), na.rm = TRUE)
  #Ej. result = function_(data=taverage.value[unique(months_years(names(taverage.value)))[12]==months_years(names(taverage.value))], value=data.q)
  function_ = function(data, value){
    return(meanf(data[data>=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names, value=data.q)
  return(byYears)
}

#' 70. BIO11
#' Mean Temperature of Coldest Quarter
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO11
#' @export
#' @examples
#' calculate70(data = taverage.value)
BIO11 = calculate70 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.25), na.rm = TRUE)
  #Ej. result = function_(data=taverage.value[unique(months_years(names(taverage.value)))[12]==months_years(names(taverage.value))], value=data.q)
  function_ = function(data, value){
    return(meanf(data[data<=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names, value=data.q)
  return(byYears)
}

#' 71. BIO13
#' Precipitation of Wettest Month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO13
#' @export
#' @examples
#' calculate71(data = pr.value)
BIO13 = calculate71 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=months, operation=sumf)
    return(maxf(byMonths))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 72. BIO14
#' Precipitation of Driest Month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO14
#' @export
#' @examples
#' calculate72(data = pr.value)
BIO14 = calculate72 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=months, operation=sumf)
    return(minf(byMonths))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 73. BIO15
#' Precipitation Seasonality (Coefficient of Variation)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO15
#' @export
#' @examples
#' calculate73(data = pr.value)
BIO15 = calculate73 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sd(data, na.rm = TRUE)/meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 74. BIO16
#' Precipitation of Wettest Quarter
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO16
#' @export
#' @examples
#' calculate74(data = pr.value)
BIO16 = calculate74 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.75), na.rm = TRUE)
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))], value=data.q)
  function_ = function(data, value){
    return(sumf(data[data>=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 75. BIO17
#' Precipitation of Driest Quarter
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO17
#' @export
#' @examples
#' calculate75(data = pr.value)
BIO17 = calculate75 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(data), c(.25), na.rm = TRUE)
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))], value=data.q)
  function_ = function(data, value){
    return(sumf(data[data<=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 76. BIO18
#' Precipitation of Warmest Quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO18
#' @export
#' @examples
#' calculate76(pr=pr.value, taverage=taverage.value)
BIO18 = calculate76 = function(pr, taverage, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(taverage), c(.75), na.rm = TRUE)
  data = pr[taverage>=data.q]
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 77. BIO19
#' Precipitation of Coldest Quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO19
#' @export
#' @examples
#' calculate77(pr=pr.value, taverage=taverage.value)
BIO19 = calculate77 = function(pr, taverage, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(taverage), c(.25), na.rm = TRUE)
  data = pr[taverage<=data.q]
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 78. BIO4
#' Temperature Seasonality (standard deviation *100)
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO4
#' @export
#' @examples
#' calculate78(data = taverage.value)
BIO4 = calculate78 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))], value=data.q)
  function_ = function(data){
    return(100*sd(data, na.rm = TRUE))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 79. BIO5
#' max Temperature of Warmest Month
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO5
#' @export
#' @examples
#' calculate79(data = taverage.value)
BIO5 = calculate79 = function(data, data_names=NULL){
  #Ej. result = function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=months, operation=meanf)
    month = names(byMonths[which(byMonths==maxf(byMonths))[1]])
    data = data[months(chron(names(data)))%in%c(month)]
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 80. BIO6
#' min Temperature of Coldest Month
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO6
#' @export
#' @examples
#' calculate80(data = taverage.value)
BIO6 = calculate80 = function(data, data_names=NULL){
  #Ej. result = function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=months, operation=meanf)
    month = names(byMonths[which(byMonths==minf(byMonths))[1]])
    data = data[months(chron(names(data)))%in%c(month)]
    return(maxf(data))
  }
  byYears = calcf_data(data=data, data_names=data_names, extract_names=years, operation=function_)
  return(byYears)
}

#' 81. BIO7
#' Temperature Annual Range (BIO5-BIO6)
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO7
#' @export
#' @examples
#' calculate81(data = taverage.value)
BIO7 = calculate81 = function(data, data_names=NULL){
  return(BIO5(data, data_names=data_names)-BIO6(data, data_names=data_names))
}

#' 82. BIO8
#' Mean Temperature of Wettest Quarter
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO8
#' @export
#' @examples
#' calculate82(taverage = taverage.value, pr = pr.value)
BIO8 = calculate82 = function(taverage, pr, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(pr), c(.75), na.rm = TRUE)
  data = taverage[pr>=data.q]
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 83. BIO9
#' Mean Temperature of Driest Quarter
#' 
#' @param taverage medium temperature 
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO9
#' @export
#' @examples
#' calculate83(taverage = taverage.value, pr = pr.value)
BIO9 = calculate83 = function(taverage, pr, data_names=NULL, time.scale=YEAR){
  data.q = quantile(ref_data(pr), c(.25), na.rm = TRUE)
  data = taverage[pr<=data.q]
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 84. BIO20
#' UTCI (Universal Thermal Climate Index)  (Blazejczyk et all, 2012) (Air temperature, Humidity, Wind) fully described at https://goo.gl/by4hH9 http://www.utci.org
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO20
#' @export
#' @examples
#' calculate84(taverage = taverage.value, pr = pr.value)
BIO20 = UTCI = calculate84 = function(taverage, pr, data_names=NULL, time.scale=YEAR){ #fergus: no entiendo
  return(FALSE)
}

#' 85. Mould index
#' Number of days with  a relative humidity over 90\% in combination with temperatures above 10°C 
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Mould index
#' @export
#' @examples
#' calculate85(taverage = taverage.value, rh = rh.value)
mould_index = calculate85 = function(taverage, rh, data_names=NULL, time.scale=YEAR){
  data = taverage>10 & rh>90
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 86. Heat Index: temperature + humidity 
#'  http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml ; available from the R package weathermetrics
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Heat Index
#' @export
#' @examples
#' calculate86(taverage = taverage.value, rh = rh.value)
heat_index = calculate86 = function(taverage, rh, data_names=NULL, time.scale=YEAR){
  data = heat.index(t = taverage, rh = rh, temperature.metric = "celsius")
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 87. Wind Chill Index: temperature + wind 
#' Osczevski, R. & Bleustein, M. 2005, Bull. Amer. Meteor. Soc., 86, 1453, doi:10.1175/BAMS-86-10-1453 https://journals.ametsoc.org/doi/abs/10.1175/BAMS-86-10-1453
#' Ta = air temperature in fahrenheit; in ºC ; v = wind speed in km/h
#' http://www.calculator.net/wind-chill-calculator.html
#' WCI = 35.74 + 0.6215*T - 35.75*V0.16 + 0.4275*T*V0.16
#' Old WCI = 0.081*(3.71*V1/2 + 5.81 - 0.25*V) × (T - 91.4) + 91.4
#' 
#' @param taverage medium temperature
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return WCI
#' @export
#' @examples
#' calculate87(taverage = taverage.value, w = w.value)
WCI = wind_chill_index = calculate87 = function(taverage, w, data_names=NULL, time.scale=YEAR){
  taverage = 1.8 * taverage + 32 #to fahrenheit
  data = 35.74 + 0.6215*taverage - 35.75*w + 0.4275*taverage*w
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 88. Apparent Temperature
#' AT = Ta + 0.33e -0.70v -4.00; Ta = air temperature in ºC ; v = wind speed in m/s; e= water vapour pressure in hPa  
#' 
#' @param taverage medium temperature
#' @param w average wind
#' @param vapor water vapour pressure
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return AT
#' @export
#' @examples
#' calculate88(taverage = taverage.value, w = w.value, vapor = vapor.value)
AT = calculate88 = function(taverage, w, vapor, data_names=NULL, time.scale=YEAR){
  data = taverage + 0.33 * vapor - 0.70 * w - 4.00
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

####wind-based
#' 89. Gustmax
#' number of days with wind gusts above 21 m/s
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Gustmax
#' @export
#' @examples
#' calculate89(data = w.value)
Gustmax = calculate89 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>21))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 90. FXx:
#' Maximun value of daily maximum wind gust (m/s), ECA&D standard
#' 
#' @param data maximum wind gust
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FXx
#' @export
#' @examples
#' calculate90(data = w_max.value)
FXx = calculate90 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 91. FG:
#' Mean of daily mean wind strength (m/s), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FG
#' @export
#' @examples
#' calculate91(data = w.value)
FG = calculate91 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 92. FGcalm: 
#' Calm days (FG <= 2 m/s) (days), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FGcalm
#' @export
#' @examples
#' calculate92(data = w.value)
FGcalm = calculate92 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data<=2))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 93. FG6Bft: 
#' Days with daily averaged wind >= 6 Bft (10.8 m/s) (days), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FG6Bft
#' @export
#' @examples
#' calculate93(data = w.value)
FG6Bft = calculate93 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=10.8))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


####aridity/continentality-indices
#' 94. Annual, seasonal, and monthly Reference Evapotranspiration (Eto)
#' If data available using Fao-56 Penman-Monteith, if not using the Hargreaves & Samani method.
#' 
#' @param data Eto
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Eto
#' @export
#' @examples
#' calculate94(data = eto.value)
Eto = calculate94 = function(data, data_names=NULL, time.scale=YEAR){

  # data = hsPET(date, lat=42, t.min, t.max)

  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


#' 95. iUNEP Aridity Index
#' P/Eto
#' 
#' @param eto et0
#' @param pr precipitation
#' @param taverage taverage
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return iUNEP
#' @export
#' @examples
#' calculate95(eto = eto.value, pr = pr.value)
iUNEP = calculate95 = function(eto, pr, taverage, data_names=NULL, time.scale=YEAR){
  data = pr/eto
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


#' 96. Climatic Moisture Deficit (CMD)
#' ETo - Effective Precipitation
#' 
#' @param eto et0
#' @param pr precipitation
#' @param taverage taverage 
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return CMD
#' @export
#' @examples
#' calculate96(eto = eto.value, pr = pr.value)
CMD = calculate96 = function(eto, pr, taverage, data_names=NULL, time.scale=YEAR){
  data = eto-pr
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 97. De Martonne Aridity Index i
#'  De Martonne = P / (T + 10); P is the annual total amount of precipitation (mm) and T is the mean annual air temperature (°C)
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Martonne Aridity Index
#' @export
#' @examples
#' calculate97(pr = pr.value, taverage = taverage.value)
martonne = martonne_aridity_index = calculate97 = function(pr, taverage, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], taverage=taverage)
  function_ = function(data, taverage){
    taverage = taverage[names(taverage) %in% names(data)]
    data = sumf(data)
    taverage = meanf(taverage)
    return(data/(taverage+10))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}

#' 98. Emberger Aridity Index
#'  100*P / (M^2 -  m^2)
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return Emberger Aridity Index
#' @export
#' @examples
#' calculate98(taverage = taverage.value, pr = pr.value)
emberger_aridity_index = calculate98 = function(taverage, pr, data_names=NULL){
  function_ = function(data, pr){
    function_ = function(data){    
      return(meanf(data))
    }
    byMonths = calcf_data(data=data, extract_names=months_years, operation=function_)
    p = sumf(pr) #meanf(pr) ¿media anual de precipitación?
    return(100*p / (maxf(byMonths)^2 - minf(byMonths)^2))
  }
  byYears = calcf_data(data=taverage, extract_names=years, operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}

#' 99. Johansson Continentality Index
#'  JCI = ( 1.7 * the annual range of monthly mean air temperatures grados / sin(geographic latitude grados) ) - 20.4
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param value lat
#' @return JCI
#' @export
#' @examples
#' calculate99(data = taverage.value, value = lat)
JCI = johansson_continentality_index = calculate99 = function(data, data_names=NULL, value){
  function_mean = function(data){
    return(meanf(data))
  }
  byMonths = calcf_data(data=data, extract_names=months_years, operation=function_mean)
  names(byMonths) = byMonths_chron(byMonths)

  function_ = function(data, value){  
    return((1.7 * (maxf(data)-minf(data)) / sin(value)) - 20.4)
  }
  byYears = calcf_data(data=byMonths, extract_names=years, operation=function_, data_names=data_names, value=value)
  return(byYears)
}

#' 100. Kerner Oceanity Index
#'  k = 100 * (temperatura media octubre en grados - temperatura media abril en grados) / the annual range of monthly mean air temperatures grados
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Kerner Oceanity Index
#' @export
#' @examples
#' calculate100(data = taverage.value)
kerner_oceanity_index = calculate100 = function(data, data_names=NULL){
  function_ = function(data){
    return(meanf(data))
  }
  byMonths = calcf_data(data=data, extract_names=months_years, operation=function_)
  names(byMonths) = byMonths_chron(byMonths)

  # function_(data=byMonths[unique(years(names(byMonths)))[68]==years(names(byMonths))])
  function_ = function(data){
    data = 100 * (data[months(names(data))==OCT]-data[months(names(data))==APR]) / (maxf(data)-minf(data))
    if(length(data)<1){
      data = NA
    }
    return(data)
  }
  byYears = calcf_data(data=byMonths, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}


#' 101. Pinna Combinative index
#'  Pinna = 1/2* ((P/(T+10)) + (12*Pd / (Td+10))) where P and T are the multi-annual mean values of precipitation and air temperature, respectively, and P’d and T’d are the mean values of precipitation and air temperature of the driest month, respectively
#' 
#' @param pr precipitation 
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @return Pinna Combinative index
#' @export
#' @examples
#' calculate101(pr = pr.value, taverage = taverage.value)
pinna = pinna_combinative_index = calculate101 = function(pr, taverage, data_names=NULL){
  #Ej. result = function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))], taverage=taverage.value)
  function_ = function(data, taverage){
    taverage = taverage[names(taverage) %in% names(data)]
    P = sumf(data)
    T = meanf(taverage)

    function_sum = function(data){
      return(sumf(data))
    }
    Pd = calcf_data(data=data, extract_names=months_years, operation=function_sum)
    function_mean = function(data){
      return(meanf(data))
    }
    Td = calcf_data(data=taverage, extract_names=months_years, operation=function_mean)
    d = whichf(Td==maxf(Td))[1]
    Pd = Pd[d]
    Td = Td[d]
    return(1/2* ((P/(T+10)) + (12*Pd / (Td+10))))
  }
  byYears = calcf_data(data=pr, extract_names=years, , data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}

#' 102. Budyko Index
#' (PP/LP)*100, where R is the mean annual net radiation (also known as the net radiation balance), PP is the mean annual precipitation, and L is the latent heat of vaporization for water
#' https://es.wikipedia.org/wiki/Clasificaci%C3%B3n_clim%C3%A1tica_de_Budyko
#' de vaporización: 2257 kJ/kg (539,4 cal/g) a 97 °C.
#' 
#' @param data net radiation 
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Budyko Index
#' @export
#' @examples
#' calculate102(data = radiation.value, pr = pr.value)
budyko_index = calculate102 = function(data, pr, data_names=NULL, time.scale=YEAR){
  function_ = function(data, pr){
    r = 0.8 * meanf(data)
    pp = meanf(pr)
    l = 2257
    return(pp/(l*r))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}

#' 103. Marsz Oceanity Index
#'  MOI = ( 0.731 * geographic latitude grados + 1.767 ) / the annual range of monthly mean air temperatures grados
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param value lat 
#' @return n0to10
#' @export
#' @examples
#' calculate103(data = taverage.value, value = lat)
MOI = marsz_oceanity_index = calculate103 = function(data, data_names=NULL, value){
  function_mean = function(data){
    return(meanf(data))
  }
  byMonths = calcf_data(data=data, extract_names=months_years, operation=function_mean)
  names(byMonths) = byMonths_chron(byMonths)

  function_ = function(data, value){  
    return(( 0.731 * value + 1.767 ) / (maxf(data)-minf(data)) )
  }
  byYears = calcf_data(data=byMonths, extract_names=years, operation=function_, data_names=data_names, value=value)
  return(byYears)
}

####snow-based
#' 104. Snowfall sum
#' The annual, seasonal and monthly sum of snowfall
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return n0to10
#' @export
#' @examples
#' calculate104(data = snow.value)
snowfall_sum = calculate104 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 105. Snow depth n0to10
#' The number of days with snow depth in the range 1-10 cm
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return n0to10
#' @export
#' @examples
#' calculate105(data = snow_depth.value)
n0to10 = calculate105 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=1 & data<=10))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 106. Snow depth n10to20
#' The number of days with snow depth of 10-20 cm
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return n10to20
#' @export
#' @examples
#' calculate106(data = snow_depth.value)
n10to20 = calculate106 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=10 & data<=20))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 107. snow depth
#' mean of daily snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return snow depth
#' @export
#' @examples
#' calculate107(data = snow_depth.value)
snow_depth = calculate107 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 108. Freq. of snow days
#' annual number of snow days
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return freq. of snow days
#' @export
#' @examples
#' calculate108(data = snow.value)
freq_snow_days = calculate108 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 109. mild snowy days
#' @param data_names names of each period of time
#' annual number of days with snow depth more than 5 cm.
#' 
#' @param data snow depth
#' @param time.scale month, season or year
#' @return mild snowy days
#' @export
#' @examples
#' calculate109(data = snow_depth.value)
mild_snowy_days = calculate109 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>5))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 110. heavy snowy days
#' annual number of days with snow depth more than 50 cm.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return heavy snowy days
#' @export
#' @examples
#' calculate110(data = snow_depth.value)
heavy_snowy_days = calculate110 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>50))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 111. The arrival date of first snowcover
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return first snowcover
#' @export
#' @examples
#' calculate111(data = snow_depth.value)
first_snowcover = calculate111 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(which(data>0)[1])
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 112. The arrival date of first permanent snowcover
#' The first date of a snow spell (consecutive snow cover days) in the year.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return first permanent snowcover
#' @export
#' @examples
#' calculate112(data = snow_depth.value)
first_permanent_snowcover = calculate112 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(snow_depth.value)))[5]==years(names(snow_depth.value))])
  function_ = function(data){
    data = as.numeric(data>0)
    data.rle = rle(as.numeric(data))
    data.i = which(data.rle$lengths>1 & data.rle$values>0)[1]
    if(is.na(data.i) | data.i==1){
      return(data.i)
    }else{
      return(sum(1, data.rle$lengths[1:(data.i-1)]))
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 113. The departure date of last permanent snowcover
#' The last date of a snow spell (consecutive snow cover days) in the year.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return last permanent snowcover
#' @export
#' @examples
#' calculate113(data = snow_depth.value)
last_permanent_snowcover = calculate113 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(snow_depth.value)))[5]==years(names(snow_depth.value))])
  function_ = function(data){
    data = as.numeric(data>0)
    data.rle = rle(as.numeric(data))
    data.i = which(data.rle$lengths>1 & data.rle$values>0)
    data.i = data.i[length(data.i)]
    if(length(data.i)==0 | is.na(data.i)){
      return(NA)
    }else{
      return(sum(data.rle$lengths[1:(data.i)]))
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}


#' 114. Average snow depth during the reference period (annual, seasonal and monthly)
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return average snow depth
#' @export
#' @examples
#' calculate114(data = snow_depth.value)
average_snow_depth = calculate114 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 115. Average amount of snow covered days in period
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return snow covered days
#' @export
#' @examples
#' calculate115(data = snow_depth.value)
snow_covered_days = calculate115 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(meanf(data>0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}


#' 116. Maximum snow depth of the year / season / month
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return maximum snow depth
#' @export
#' @examples
#' calculate116(data = snow_depth.value)
maximum_snow_depth = calculate116 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

####Cloud/radiation-based
#' 117. Sum of sunshine duration
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return sunshine duration
#' @export
#' @examples
#' calculate117(data = insolation.value)
sunshine_duration = calculate117 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}


#' 118. sunny days
#' days with mean cloud cover less than 10\%.
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return sunny days
#' @export
#' @examples
#' calculate118(data = cloud_cover.value)
sunny_days = calculate118 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(sumf(data<10))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 119. cloudy days
#' @param data_names names of each period of time
#' Number of days with cloud base below 100 meter.
#' 
#' @param data cloud base below 100 meter
#' @param time.scale month, season or year
#' @return cloudy days
#' @export
#' @examples
#' calculate119(data = cloud_cover_less_100.value)
cloudy_days = calculate119 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(sumf(data>0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 120. mean CC
#' mean daily cloud cover
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return mean CC
#' @export
#' @examples
#' calculate120(data = cloud_cover.value)
mean_CC = calculate120 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 121. SSp:
#' Sunshine duration fraction with respect to day length (\%), standard ECA&D
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SSp
#' @export
#' @examples
#' calculate121(data = insolation.value)
SSp = calculate121 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(100*meanf(data)/24)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}


#' 122. SS: 
#' Sunshine duration (hours), standard ECA&D
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SS
#' @export
#' @examples
#' calculate122(data = insolation.value)
SS = calculate122 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 123. Atmospheric Clarity Index
#' ratio between solar radiation at surface and solar radiation at TOA (alt top of the atmosphere empirically obtained, see https://goo.gl/Wzs1Zk)
#' http://www.greenrhinoenergy.com/solar/radiation/atmosphere.php
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return ACI
#' @export
#' @examples
#' data(radiation); data(toa)
#' ACI(data = radiation.value, toa=toa.value)
ACI = calculate123 = function(data, toa, data_names=NULL, time.scale=YEAR){
  function_ = function(data, toa){
    # toa  = toa[]
    return(data/toa)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, toa=toa, data_names=data_names)
  return(byYears)
}

####Drought indices
#' 124. Standardized Precipitation Index (SPI)
#' 1, 3, 6 and 12 month SPI
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param scale scale
#' @return SPI
#' @export
#' @examples
#' calculate124(data = pr.value)
SPI = calculate124 = function(data, data_names=NULL, scale=3){
  function_ = function(data, value){
    return(sumf(data))
  }
  byMonths = calcf_data(data=data, extract_names=months_years, operation=function_)  
  data3 = array(spi(byMonths, scale=scale, na.rm = TRUE)$fitted[, 1])
  names(data3) = names(byMonths)
  return(data3)
}

#' 125. Standardized Precipitation Evapotranspiration Index (SPEI)
#' 1, 3, 6 and 12 month SPEI
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param scale scale
#' @return SPEI
#' @export
#' @examples
#' calculate125(eto = eto.value, pr = pr.value)
SPEI = calculate125 = function(eto, pr, data_names=NULL, scale=3){
  data = pr - eto 
  function_ = function(data, value){
    return(sumf(data))
  }
  byMonths = calcf_data(data=data, extract_names=months_years, operation=function_)
  data3 = array(spei(byMonths, scale=scale, na.rm = TRUE)$fitted[, 1])
  names(data3) = names(byMonths)  
  return(data3)
}

####Fire-based
#' 126. Canadian Fire Weather Index (FWI)
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R
#' https://github.com/SantanderMetGroup/fireDanger
#' combination of daily values of temperature, relative humidity, surface wind and precipitation
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param w average wind
#' @param pr precipitation
#' @param dew_point dew_point
#' @param lat latitude
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FWI
#' @export
#' @examples
#' calculate126(taverage=taverage.value, rh = rh.value, w = w.value, 
#' pr = pr.value, lat = lat, dew_point=dew_point.value)
FWI = calculate126 = function(taverage, rh, w, pr, dew_point, lat, data_names=NULL, time.scale=YEAR){
  # fergus: Comparar las 2 funciones
  dayLength = DayLengths(lat)
  data = index_CFWI(Month=as.numeric(months(names(taverage))),Days=as.POSIXlt(chron(names(taverage)))$yday+1,Temp=taverage, Dew = dew_point, WS = w, Rain = pr, daylist=dayLength)

  data = fwi1D(months=as.numeric(months(names(taverage))), Tm=taverage, H=rh, r=pr, W=w, lat = lat)
  data = data[names(data)!=""]
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 127. Keetch-Byran Drought Index (KBDI)
#' combination of daily maximum in temperature and precipitation
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R#Keech-Byran_Drought_Index
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param rh relative humidity
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return KBDI
#' @export
#' @examples
#' calculate127(taverage = taverage.value, rh=rh.value, w = w.value, pr=pr.value)
KBDI = calculate127 = function(taverage, pr, rh, w, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), operation=function_)
  map = meanf(byYears[as.numeric(names(byYears))>=1961 & as.numeric(names(byYears))<=1990])
  # fergus: Comparar las 2 funciones
  data = index_KBDI(Temperature=taverage, Rain=pr, MAP=map)
  data = kbdindex(date=chron(names(taverage)), t=taverage, p=pr, h=rh, w=w/1000)
  names(data) = names(taverage)
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 128. McArthur Forest Fire Danger Index (FFDI)
#' combination of temperature, relative humidity, surface wind speed and KBDI
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R#Keech-Byran_Drought_Index
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param rh relative humidity
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FFDI
#' @export
#' @examples
#' calculate128(taverage = taverage.value, pr=pr.value, rh=rh.value, w=w.value)
FFDI = calculate128 = function(taverage, pr, rh, w, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), operation=function_)
  map = meanf(byYears[as.numeric(names(byYears))>=1961 & as.numeric(names(byYears))<=1990])
  kdbiData = index_KBDI(Temperature=taverage, Rain=pr, MAP=map)
  data = index_MA(Temperature=taverage, Rain=pr, DewPoint=rh, MAP=map, Wind=w, KBDI=kdbiData)
  names(data) = names(taverage)
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 129. Modified Nesterov Index (MNI)
#' cummulative function of temperature and dew point deficit
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R
#' https://github.com/jbedia/fireDanger/wiki/nesterovIndex
#' @param dew_point dew point
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return MNI
#' @export
#' @examples
#' calculate129(dew_point=dew_point.value, rh=rh.value, taverage=taverage.value, pr=pr.value)
MNI = calculate129 = function(dew_point, taverage, rh, pr, data_names=NULL, time.scale=YEAR){
  # Reescritura de las función index_MMI
  # lookK_ <- function(data){
  #   data.return = data
  #   data.return[data>= 0.0 & data< 0.1] = 1.0
  #   data.return[data>= 0.1 & data< 1.0] = 0.8
  #   data.return[data>= 1.0 & data< 3.0] = 0.6
  #   data.return[data>= 3.0 & data< 6.0] = 0.4
  #   data.return[data>= 6.0 & data< 15.0] = 0.2
  #   data.return[data>= 15.0 & data< 19.0] = 0.1
  #   data.return[data< 0 | data>=19.0] = 0
  # }
  # deficit = taverage - dew_point
  # data = taverage * deficit * lookK_(pr)
  # data[pr>3.0] = 0.0

  # fergus: Comparar las 2 funciones
  data = index_MMI(DewPoint=dew_point, Temperature=taverage, Rain=pr)

  data = nesterovIndex(t=taverage, rh=rh, p=pr)

  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 130. Finnish Forest Fire Index (FFFI)
#' combination of temperature, relative humidity, wind speed, radiation and precipitation
#' https://link.springer.com/chapter/10.1007%2F978-3-642-55903-7_88
#' P-eto+-f1
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FFFI
#' @export
#' @examples
#' calculate130(data = radiation.value, toa=toa.value)
FFFI = calculate130 = function(data, toa, data_names=NULL, time.scale=YEAR){ #fergus: no entiendo

  function_ = function(data, toa){
    toa  = toa[]
    return(data/toa)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, toa=toa, data_names=data_names)
  return(byYears)
}

####Tourism
#' 131. HCI:Urban
#' Holliday Climate Index for Urban destinations (Scott et all, 2016) (Tmax,wind,cloudiness,RH, precipitation) Scott, D., Rutty, M., Amelung, B. and Tang, M. (2016): An inter-comparison of the Holiday Climate Index (HCI) and the Tourism Climate Index (TCI), Atmosphere, 7, 80, doi:10.3390/atmos7060080
#' 
#' @param pr precipitation
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return HCI
#' @export
#' @examples
#' calculate131(pr = pr.value, w=w.value)
HCI = calculate131 = function(pr, w, data_names=NULL, time.scale=YEAR){ #fergus: no entiendo
  #tc: thermal comfort 
  # data = 4*tc + 2*cloud_cover + 3*pr + w
  # function_ = function(data){
  #   return(meanf(data))
  # }
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(NULL)
}

#' 132. TCI: Tourism Climatic Index
#' Standard index computed by ECA&D; Described at Miezkowski (1985), conceptual formula: TCI = 4cid + cia + 2R + 2S + W, where CId is a daytime comfort index, CIa a daily comfort index, R is cumulated rainfall, S the daily sunshine hours and W wind speed
#' 
#' @param data precipitation
#' @param sunshine net radiation 
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return TCI
#' @export
#' @examples
#' calculate132(data=pr.value, sunshine=radiation.value, w=w.value)
TCI = calculate132 = function(data, sunshine, w, data_names=NULL, time.scale=YEAR){ #fergus: no entiendo
  # #cia: Daily Comfort Index
  # #cid: Daytime Comfort Index 
  # data = 2 * ( 4*cid + cia + 2*pr + 2*sunshine + w )
  # function_ = function(data){
  #   return(meanf(data))
  # }
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(NULL)
}

#' 133. TCI>60: 
#' Number of days TCI>60 , standard ECA&D
#' 
#' @param data Tourism Climatic Index
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return TCI60
#' @export
#' @examples
#' calculate133(data = tci.value)
TCI60 = calculate133 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>60))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 134. TCI>80:
#' Number of days TCI>80, standard ECA&D
#' 
#' @param data Tourism Climatic Index
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return TCI80
#' @export
#' @examples
#' calculate134(data = tci.value)
TCI80 = calculate134 = function(data, data_names=NULL, time.scale=YEAR){  
  function_ = function(data){
    return(sumf(data>80))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
