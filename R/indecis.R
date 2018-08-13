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
