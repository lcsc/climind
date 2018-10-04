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

# source("Fire_Danger_Index_Functions.R")
# source("penman_fao_dia.R")
# source("indecis_indices.R")
# source("indecis_indices_functions.R")
# source("fwi1D.R")
# source("kbdindex.R")
# source("nesterovIndex.R")

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

#' Function allow scale
#'
#' @param name function name
#' @return allow or not allow
#' @examples
#' allow_scale(name="spei_1")
allow_scale = function(name){
  return("time.scale"%in%names(formals(name)))
}

#' Scales allow
#'
#' @param name function name
#' @return scales allow
#' @examples
#' allow_scale(name="spei_1")
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

# tmin.value=NULL; tmax.value=NULL; taverage.value=NULL; pr.value=NULL
# insolation.value=NULL; w.value=NULL; w_max.value=NULL; snow.value=NULL; snow_depth.value=NULL; cloud_cover.value=NULL; cloud_cover_less_100.value=NULL; radiation.value=NULL; dew_point.value=NULL; toa.value=NULL
calculate_all = function(tmin.value=NULL, tmax.value=NULL, taverage.value=NULL, insolation.value=NULL, w.value=NULL, w_max.value=NULL, pr.value=NULL, snow.value=NULL, snow_depth.value=NULL, cloud_cover.value=NULL, cloud_cover_less_100.value=NULL, radiation.value=NULL, dew_point.value=NULL, toa.value=NULL, lat=NULL, time.scale=YEAR, data_names=NULL, index_result = c(1:138)){
  
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

 parameters = list(tn=tmin.value, tx=tmax.value, tg=taverage.value, insolation.value, w.value, w_max.value, rr=pr.value, snow.value, snow_depth.value, cloud_cover.value, cloud_cover_less_100.value, radiation.value, dew_point.value, toa.value, vapor.value, rh.value, tci.value)

  no_null = function(){
    return(parameters[[which(!sapply(parameters, is.null))[1]]])
  }

  if(is.null(data_names)){
    data = no_null()
    date = chron(names(data))
    extract_names=select_time_function(time.scale)
    data_names = extract_names(date)
  }

  # Eliminamos funciones que no existen
  index_result = index_result[sapply(paste0("calculate_", index_result), exists)]
  # Eliminamos funciones que no permiten el parámetro time.scale si la escala no es anual; ya que esas funciones solo funcionan para el time.scale anual
  if(time.scale!=YEAR){    
    index_result = index_result[sapply(paste0("calculate_", index_result), allow_scale)]
    if(time.scale==MONTH){
      index_result = c(index_result, c(122:129))
    }
  }else{
    index_result = index_result[!index_result %in%  c(122:129)]
  }

  # source("indecis_indices.R")
  calculate_index = function(n, parameters){
    f = get(paste0("calculate_", n))
    if(is.null(attr(f, "data"))){
      stop(paste("calculate_index in calculate", n))
    }
    if(n%in%c(13, 14, 19, 30)){
      return(f(max=tmax.value, min=tmin.value, data_names=data_names, time.scale=time.scale))
    }
    if(n%in%c(74, 75, 80, 81, 96, 97, 100)){
      return(f(pr=pr.value, taverage=taverage.value, data_names=data_names, time.scale=time.scale))
    }
    if(n%in%c(77, 78, 79)){
      return(f(data=taverage.value,max=tmax.value, min=tmin.value, data_names=data_names, time.scale=time.scale))
    }
    if(n%in%c(98, 102)){
      return(f(data=parameters[[attr(f, "data")[1]]], data_names=data_names, time.scale=time.scale, value = lat))
    }
    if(n%in%c(126, 127, 128, 129)){ # SPEI
      if(!is.null(eto.value)){
        return(f(eto=eto.value, pr=pr.value, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(n%in%c(130)){
      if(!is.null(dew_point.value)){
        return(f(rh=rh.value, w=w.value, dew_point=dew_point.value, taverage=taverage.value, pr=pr.value, lat=lat, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(n%in%c(131, 132)){
      if(!is.null(rh.value)){
        return(f(rh=rh.value, w=w.value, taverage=taverage.value, pr=pr.value, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(n%in%c(133)){
      if(!is.null(dew_point.value)){
        return(f(rh=rh.value, dew_point=dew_point.value, taverage=taverage.value, pr=pr.value, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(n%in%c(134)){
      if(!is.null(toa.value)){
        return(f(data = radiation.value, toa=toa.value, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(n%in%c(135)){
      if(!is.null(toa.value)){
        return(f(w = w.value, pr = pr.value, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(n%in%c(136)){
      if(!is.null(radiation.value)){
        return(f(data=pr.value, sunshine=radiation.value, w=w.value, data_names=data_names, time.scale= time.scale))
      }else{
        return(NULL)
      }
    }
    if(!is.na(attr(f, "data"))){
      return(f(data=parameters[[attr(f, "data")[1]]], data_names=data_names, time.scale=time.scale))
    }else{
      warning(paste("calculate_index in calculate", n))            
    }
    return(NULL)
  }

  result_list = list()
  length(result_list)<-length(index_result)
  names(result_list) = index_names[index_result]
  for (i in 1:length(index_result)){
    n = index_result[i]
    result_list[[index_names[n]]] = calculate_index(n, parameters=parameters)
  }
  # index_result = which(index_names%in%names(result_list))

  return(result_list)
}
