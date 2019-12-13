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

#' @include indecis_indices.R
NULL

# source("R/penman_fao_dia.R")
# source("R/indecis_indices.R")
# source("R/indecis_indices_functions.R")
# source("R/fwi1D.R")
# source("R/kbdindex.R")
# source("R/nesterovIndex.R")

#' Function allow scale
#'
#' @param name function name
#' @return allow or not allow
#' @keywords internal
allow_scale = function(name){
  return("time.scale"%in%names(formals(name)))
}

#' Scales allow
#'
#' @param name function name
#' @return scales allow
#' @keywords internal
scale_name = function(name){
  if(allow_scale(name)){
    scales = c(YEAR, MONTH, SEASON)
  }else{
    scales = c(YEAR)
  }
  if(grepl("spei", name) | grepl("spi", name)){
    scales = c(MONTH)
  }
  return(scales)
}

#' Select no empty parameters
#'
#' @param data data list
#' @return no empty parameter
#' @keywords internal
no_null = function(data){
  return(data[[which(!sapply(data, is.null))[1]]])
}

#' @title Calculate all indexes
#' @description Calculate all indexes for a point 
#'
#' @param data data list
#' @param lat latitude, degree
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param index_result indexes to calculate
#' @param na.rm logical. Should missing values (including NaN) be removed? (value or array by index)
#' @return all indexes
#' @export
## @examples
## data(data_all)
## calculate_all(data = data_all, lat = data_all$lat)
calculate_all = function(data, lat=NULL, time.scale=YEAR, data_names=NULL, index_result = c(1:138), na.rm=FALSE){

  data[[LAT]] = lat
  if(!is.null(data[[SNOWFALL]])){
    data[[SNOWFALL]][data[[SNOWFALL]]<0.0001] = 0
  }
  if(is.null(data[[SNOWFALLMM]]) & !is.null(data[[SNOWFALL]])){
    data[[SNOWFALLMM]] = data[[SNOWFALL]]*1000
  }
  if(is.null(data[[SNOWDEPTH]]) & !is.null(data[[SWE]]) & !is.null(data[[SNOWDENSITY]])){
    # data[[SNOWDEPTH]] = data[[SWE]]/312
    data[[SNOWDEPTH]] = 100000 * data[[SWE]] / data[[SNOWDENSITY]] # 100000 * m of water equivalent / kg m-3
  }
  if(is.null(data[[VAPOUR]]) &  !is.null(data[[DEWPOINT]])){
    data[[VAPOUR]] = td_to_vapor(data[[DEWPOINT]])
  }
  # Checking values
  if(sum(data[[TMAX]]<data[[TMIN]], na.rm = TRUE)>0){
    warning("TMAX < TMIN")
  }
  if(is.null(data[[TMEAN]]) & !is.null(data[[TMAX]]) & !is.null(data[[TMIN]])){
    data[[TMEAN]] = (data[[TMAX]]+data[[TMIN]])/2
  }
  if(sum(data[[TMAX]]<data[[TMEAN]], na.rm = TRUE)>0){
    warning("TMAX < TMEAN")
  }
  if(sum(data[[TMEAN]]<data[[TMIN]], na.rm = TRUE)>0){
    warning("TMEAN < TMIN")
  }
  if(is.null(data[[HUMIDITY]]) & !is.null(data[[TMAX]]) & !is.null(data[[TMIN]]) & !is.null(data[[DEWPOINT]])){
    data[[HUMIDITY]] = td_to_rh(tmax=data[[TMAX]], tmin=data[[TMIN]], td=data[[DEWPOINT]])
  }
  if(sum(data[[PRECIPITATION]]<0, na.rm = TRUE)>0){
    warning(paste("PRECIPITATION < 0", sum(data[[PRECIPITATION]]<0, na.rm = TRUE)))
    data[[PRECIPITATION]][data[[PRECIPITATION]]<0] = 0 
  }
  if(is.null(data[[RADIATION]]) & !is.null(data[[RADIATION_W]])){
    # W/M2 -> J/m2 
    data[[RADIATION]] = data[[RADIATION_W]] * (24*60*60)
  }
  if(is.null(data[[RADIATION_W]]) & !is.null(data[[RADIATION]])){
    # J/m2 -> W/M2
    data[[RADIATION_W]] = data[[RADIATION]] / (24*60*60)
  }
  if(sum(data[[RADIATION]]<0, na.rm = TRUE)>0){
    data[[RADIATION]][data[[RADIATION]]<0] = 0
    warning("RADIATION < 0")
  }
  if(sum(data[[RADIATIONTOA]]<0, na.rm = TRUE)>0){
    data[[RADIATIONTOA]][data[[RADIATIONTOA]]<0] = 0
    warning("RADIATIONTOA < 0")
  }
  if(sum(data[[RADIATION_W]]<0, na.rm = TRUE)>0){
    data[[RADIATION_W]][data[[RADIATION_W]]<0] = 0
    warning("RADIATION_W < 0")
  }
  if(sum(data[[WIND]]<0, na.rm = TRUE)>0){
    data[[WIND]][data[[WIND]]<0] = 0
    warning("WIND < 0")
  }  
  if(sum(data[[WINDGUST]]<0, na.rm = TRUE)>0){
    data[[WINDGUST]][data[[WINDGUST]]<0] = 0
    warning("WINDGUST < 0")
  }
  if(sum(data[[HUMIDITY]]>100, na.rm = TRUE)>0){
    data[[HUMIDITY]][data[[HUMIDITY]]>100] = 100
    data[[HUMIDITY]][data[[HUMIDITY]]<0] = 0
    warning("HUMIDITY > 100")
  }
  if(sum(data[[CLOUD]]>100, na.rm = TRUE)>0){
    data[[CLOUD]][data[[CLOUD]]>100] = 100
    data[[CLOUD]][data[[CLOUD]]<0] = 0
    warning("CLOUD > 100")
  }
  if(sum(data[[CLOUD100]]>100, na.rm = TRUE)>0){
    data[[CLOUD100]][data[[CLOUD100]]>100] = 100
    data[[CLOUD100]][data[[CLOUD100]]<0] = 0
    warning("CLOUD100 > 100")
  }
  if(is.null(data[[INSOLATION]]) & !is.null(data[[RADIATION]]) & !is.null(data[[LAT]]) & !is.null(data[[MDE]])){
    data[[INSOLATION]] = r_to_in(radiation=data[[RADIATION]], lat=data[[LAT]], mde=data[[MDE]])
  }
  if(is.null(data[[ETO]]) & !is.null(data[[TMIN]]) & !is.null(data[[MDE]]) & !is.null(data[[TMAX]]) & (!is.null(data[[RADIATION]]) | !is.null(data[[INSOLATION]])) & !is.null(data[[WIND]]) & !is.null(data[LAT]) & !is.null(data[[DEWPOINT]])){
    # tmin = data[[TMIN]]; tmax = data[[TMAX]]; radiation = data[[RADIATION]]; toa = data[[RADIATIONTOA]]; w = data[[WIND]]; lat=data[[LAT]]; tdew = data[[DEWPOINT]]; mde=data[[MDE]]; rh=data[[HUMIDITY]]
    data[[ETO]] = calc_eto(tmin = data[[TMIN]], tmax = data[[TMAX]], radiation = data[[RADIATION]], toa = data[[RADIATIONTOA]], w = data[[WIND]], lat=data[[LAT]], tdew = data[[DEWPOINT]], mde=data[[MDE]], rh=data[[HUMIDITY]], insolation=data[[INSOLATION]])
  }
  # return(list("eto_all"=data[[ETO]])) #fergus:quitar
  # data_all=data

  if(is.null(data_names)){
    date = chron(names(no_null(data)))
    extract_names=select_time_function(time.scale)
    data_names = extract_names(date)
  }

  # Eliminamos funciones que no existen
  # index_result = index_result[sapply(paste0("calculate_", index_result), exists)]
  index_result = index_result[!is.na(index_names[index_result])]

  index_spei = c(122:129)
  index_result.spei = index_spei[index_spei%in%index_result]
  # We eliminate functions that do not allow the time.scale parameter if the scale is not annual; since those functions only work for the annual time.scale
  if(time.scale!=YEAR){
    index_result = index_result[sapply(paste0("calculate_", index_result), allow_scale)]
    if(time.scale==MONTH){
      index_result = c(index_result, index_result.spei)
    }
  }else{
    index_result = index_result[!index_result %in% index_spei]
  }
  ## source("indecis_indices.R")
  ## Calculate index
  ##
  ## @param n id index
  ## @param data all parameters
  ## @param na.rm logical. Should missing values (including NaN) be removed?
  ## @return calculate index
  ## @keywords internal
  calculate_n_index = function(n, data, na.rm){
    # f = get(paste0("calculate_", n)) #, pos = -1
    # print(index_names[n])   
    f = index_functions[[index_names[n]]]

    if(is.null(attr(f, "data"))){
      stop(paste("calculate_index in calculate", n))
    }
    if(!is.na(attr(f, "data")[1])){
      # print(length(attr(f, "data")))
      # print(class(data))
      if(length(attr(f, "data"))==1){
        return(f(data=data[[attr(f, "data")[1]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==2){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==3){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==4){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==5){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==6){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==7){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==8){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==9){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data[[attr(f, "data")[9]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==10){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data[[attr(f, "data")[9]]], data[[attr(f, "data")[10]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else if(length(attr(f, "data"))==11){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data[[attr(f, "data")[9]]], data[[attr(f, "data")[10]]], data[[attr(f, "data")[11]]], data_names=data_names, time.scale=time.scale, na.rm = na.rm))
      }else{
        print("More input values than allowed.")
      }
    }else{
      warning(paste("calculate_index in calculate", n))
    }
    return(NULL)
  }

  result_list <- list()
  if(length(index_result)>0){
    length(result_list) <- length(index_result)
    names(result_list) <- index_names[index_result]
    start <- 1
    i <- 1 
    for (i in start:length(index_result)){
      n <- index_result[i]
      # print(paste("Calculate", "function", n, "i", i))
      if(length(na.rm)==1){
        na.rm.n = na.rm
      }else{
        na.rm.n = na.rm[n]
      }
      result_list[[index_names[n]]] <- calculate_n_index(n, data=data, na.rm=na.rm.n)
    }
    # index_result_yes = which(index_names%in%names(result_list))
    # index_result_no = which(!(index_names%in%names(result_list)))
  }
  return(result_list)
}

#' @title Calculate all indexes for all time scales
#' @description Calculate all indexes for a point and all time scales
#'
#' @param data data list
#' @param lat latitude, degree
#' @return all indexes
#' @export
## @examples
## data(data_all)
## calculate_all_scales(data = data_all, lat = data_all$lat)
calculate_all_scales = function(data, lat=NULL){
  data_year=calculate_all(data, lat, time.scale=YEAR)
  data_month=calculate_all(data, lat, time.scale=MONTH)
  data_season=calculate_all(data, lat, time.scale=SEASON)
  return(list(year=data_year, month=data_month, season=data_season))
}
