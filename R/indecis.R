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
#' @param data lista de datos de entrada para los cálculos
#' @param lat latitude
#' @return all indexes
#' @export
#' @examples
calculate_all_scales = function(data, lat=NULL){
  data_year=calculate_all(data, lat, time.scale=YEAR)
  data_month=calculate_all(data, lat, time.scale=MONTH)
  data_season=calculate_all(data, lat, time.scale=SEASON)
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
#' scale_name(name="spei_1")
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
#' @param data lista de datos de entrada para los cálculos
#' @param lat latitude
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return all indexes
#' @export
#' @examples
calculate_all = function(data, lat=NULL, time.scale=YEAR, data_names=NULL, index_result = c(1:138)){

  data[[LAT]] = lat

  if(is.null(data[[SNOWDEPTHTHICKNESS]]) & !is.null(data[[SNOWDEPTH]])){
    data[[SNOWDEPTHTHICKNESS]] = data[[SNOWDEPTH]]*0.312
  }
  if(is.null(data[[VAPOUR]]) &  !is.null(data[[DEWPOINT]])){
    data[[VAPOUR]] = td_to_vapor(data[[DEWPOINT]])
  }
  if(is.null(data[[HUMIDITY]]) & !is.null(data[[TMAX]]) & !is.null(data[[TMIN]]) & !is.null(data[[DEWPOINT]])){
    data[[HUMIDITY]] = td_to_rh(tmax=data[[TMAX]], tmin=data[[TMIN]], td=data[[DEWPOINT]])
  }
  if(is.null(data[[TMEAN]]) & !is.null(data[[TMAX]]) & !is.null(data[[TMIN]])){
    data[[TMEAN]] = (data[[TMAX]]+data[[TMIN]])/2
  }
  if(is.null(data[[INSOLATION]]) & !is.null(data[[RADIATION]]) & !is.null(data[[LAT]]) & !is.null(data[[MDE]])){
    data[[INSOLATION]] = r_to_in(radiation=data[[RADIATION]], lat=data[[LAT]], mde=data[[MDE]])
  }
  if(is.null(data[[EVAPOTRANSPIRATION]]) & !is.null(data[[TMIN]]) & !is.null(data[[MDE]]) & !is.null(data[[TMAX]]) & (!is.null(data[[RADIATION]]) | !is.null(data[[INSOLATION]])) & !is.null(data[[WIND]]) & !is.null(data[LAT]) & !is.null(data[[DEWPOINT]])){
    # tmin = data[[TMIN]]; tmax = data[[TMAX]]; radiation = data[[RADIATION]]; toa = data[[RADIATIONTOA]]; w = data[[WIND]]; lat=data[[LAT]]; tdew = data[[DEWPOINT]]; mde=data[[MDE]]; rh=data[[HUMIDITY]]
    # data[[EVAPOTRANSPIRATION]] = calc_eto(tmin = data[[TMIN]], tmax = data[[TMAX]], radiation = data[[RADIATION]], insolation=data[[INSOLATION]], toa = data[[RADIATIONTOA]], w = data[[WIND]], lat=data[[LAT]], tdew = data[[DEWPOINT]], mde=data[[MDE]], rh=data[[HUMIDITY]])
    data[[EVAPOTRANSPIRATION]] = calc_eto(tmin = data[[TMIN]], tmax = data[[TMAX]], radiation = data[[RADIATION]], toa = data[[RADIATIONTOA]], w = data[[WIND]], lat=data[[LAT]], tdew = data[[DEWPOINT]], mde=data[[MDE]], rh=data[[HUMIDITY]], insolation=data[[INSOLATION]])
  }

  # Comprobación de valores
  if(sum(data[[TMAX]]<data[[TMIN]], na.rm = TRUE)>0){
    warning("TMAX < TMIN")
  }
  if(sum(data[[TMAX]]<data[[TMEAN]], na.rm = TRUE)>0){
    warning("TMAX < TMEAN")
  }
  if(sum(data[[TMEAN]]<data[[TMIN]], na.rm = TRUE)>0){
    warning("TMEAN < TMIN")
  }
  if(sum(data[[PRECIPITATION]]<0, na.rm = TRUE)>0){
    warning(paste("PRECIPITATION < 0", sum(data[[PRECIPITATION]]<0, na.rm = TRUE)))
    data[[PRECIPITATION]][data[[PRECIPITATION]]<0] = 0
  }
  if(sum(data[[WIND]]<0, na.rm = TRUE)>0){
    warning("WIND < 0")
  }
  if(sum(data[[WINDGUST]]<0, na.rm = TRUE)>0){
    warning("WINDGUST < 0")
  }
  if(sum(data[[HUMIDITY]]>100, na.rm = TRUE)>0){
    warning("HUMIDITY > 100")
  }
  if(sum(data[[CLOUD]]>100, na.rm = TRUE)>0){
    warning("CLOUD > 100")
  }
  if(sum(data[[CLOUD100]]>100, na.rm = TRUE)>0){
    warning("CLOUD100 > 100")
  }
  # data_all=data

  #' Select no null parameters
  #'
  #' @return no null parameters
  #' @examples
  no_null = function(){
    return(data[[which(!sapply(data, is.null))[1]]])
  }

  if(is.null(data_names)){
    data = no_null()
    date = chron(names(data))
    extract_names=select_time_function(time.scale)
    data_names = extract_names(date)
  }

  # Eliminamos funciones que no existen
  index_result = index_result[sapply(paste0("calculate_", index_result), exists)]

  index_spei = c(122:129)
  index_result.spei = index_spei[index_spei%in%index_result]
  # Eliminamos funciones que no permiten el parámetro time.scale si la escala no es anual; ya que esas funciones solo funcionan para el time.scale anual
  if(time.scale!=YEAR){
    index_result = index_result[sapply(paste0("calculate_", index_result), allow_scale)]
    if(time.scale==MONTH){
      index_result = c(index_result, index_result.spei)
    }
  }else{
    index_result = index_result[!index_result %in% index_spei]
  }

  # source("indecis_indices.R")
  #' Calculate index
  #'
  #' @param n id index
  #' @param data all parameters
  #' @return calculate index
  #' @examples
  calculate_n_index = function(n, data){
    f = get(paste0("calculate_", n))
    if(is.null(attr(f, "data"))){
      stop(paste("calculate_index in calculate", n))
    }
    if(!is.na(attr(f, "data")[1])){
      if(length(attr(f, "data"))==1){
        return(f(data=data[[attr(f, "data")[1]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==2){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==3){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==4){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==5){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==6){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==7){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==8){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==9){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data[[attr(f, "data")[9]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==10){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data[[attr(f, "data")[9]]], data[[attr(f, "data")[10]]], data_names=data_names, time.scale=time.scale))
      }else if(length(attr(f, "data"))==11){
        return(f(data[[attr(f, "data")[1]]], data[[attr(f, "data")[2]]], data[[attr(f, "data")[3]]], data[[attr(f, "data")[4]]], data[[attr(f, "data")[5]]], data[[attr(f, "data")[6]]], data[[attr(f, "data")[7]]], data[[attr(f, "data")[8]]], data[[attr(f, "data")[9]]], data[[attr(f, "data")[10]]], data[[attr(f, "data")[11]]], data_names=data_names, time.scale=time.scale))
      }else{
        print("Más valores de entrada de los permitidos.")
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
      print(paste("Calcular", "función", n, "i", i))
      result_list[[index_names[n]]] <- calculate_n_index(n, data=data)
    }
    # Eliminados: 47 48
    # Devuelven NULL porque no sabemos como funcionan 135 136 137 138
    # index_result_yes = which(index_names%in%names(result_list))
    # index_result_no = which(!(index_names%in%names(result_list)))
  }
  return(result_list)
}
