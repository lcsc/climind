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

#' @include custom_functions.R
#' @include ffdi.R
#' @include fwi1D.R
#' @include indecis_indices_functions.R
#' @include kbdindex.R
#' @include macArthurFFDI.R
#' @include nesterovIndex.R
#' @include penman_fao_dia.R
NULL

# Datos diarios

C_degrees = "celsius"
C_days = "days"
C_date = "date"
C_precipitation = "mm"
C_index = "index"
C_radiation = "radiation"
C_wind = "wind"
C_snow = "snow"
C_cloud = C_sunshine = "sunshine"

# index_tipes = array(NA, dim=c(138))
# index_tipes[1:42] = "Temperature-based"
# index_tipes[43:66] = "Precipitation-based"
# index_tipes[67:87] = "Bioclimatic"
# index_tipes[88:92] = "wind-based"
# index_tipes[93:102] = "Aridity/continentality-indices"
# index_tipes[103:115] = "Snow-based"
# index_tipes[116:121] = "Cloud/radiation-based"
# index_tipes[122:129] = "Drought"
# index_tipes[130:134] = "Fire"
# index_tipes[135:138] = "Tourism"

index_tipes = list("Temperature-based"=c(1:42), "Precipitation-based" = c(43:66), "Bioclimatic" = c(67:87), "wind-based" = c(88:92), "Aridity/continentality-indices" = c(93:102), "Snow-based" = c(103:115), "Cloud/radiation-based" = c(116:121), "Drought" = c(122:129), "Fire" = c(130:134), "Tourism" = c(135:138))

index_units = array(NA, dim=c(138))
index_titles = array(NA, dim=c(138))
index_names = array(NA, dim=c(138))
index_functions = list()

#' @title Mean TX
#' @description Mean of daily maximum air temperature
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' gtx(data=data_all$tg)
gtx = calculate_1 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[1] = C_degrees
index_titles[1] = "Mean TX"
index_names[1] = "gtx"
attr(calculate_1, "data") <- c(TMAX)

#' @title Maximum TX
#' @description Maximum of daily maximum air temperature
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5. \url{https://www.ecad.eu/documents/WCDMP_72_TD_1500_en_1.pdf}
## @importance Important application in agriculture, tourism, water, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' xtx(data=data_all$tx)
xtx = calculate_2 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[2] = C_degrees
index_titles[2] = "Maximum TX"
index_names[2] = "xtx"
attr(calculate_2, "data") <- c(TMAX)

#' @title Minimum TX
#' @description Minimum of daily maximum air temperature
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5. \url{https://www.ecad.eu/documents/WCDMP_72_TD_1500_en_1.pdf}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' ntx(data=data_all$tx)
ntx = calculate_3 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[3] = C_degrees
index_titles[3] = "Minimum TX"
index_names[3] = "ntx"
attr(calculate_3, "data") <- c(TMAX)

#' @title Mean TN
#' @description Mean of daily minimum air temperature
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' gtn(data=data_all$tn)
gtn = calculate_4 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[4] = C_degrees
index_titles[4] = "Mean TN"
index_names[4] = "gtn"
attr(calculate_4, "data") <- c(TMIN)

#' @title Maximum TN
#' @description Maximum of daily minimum air temperature
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5. \url{https://www.ecad.eu/documents/WCDMP_72_TD_1500_en_1.pdf}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' xtn(data=data_all$tn)
xtn = calculate_5 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[5] = C_degrees
index_titles[5] = "Maximum TN"
index_names[5] = "xtn"
attr(calculate_5, "data") <- c(TMIN)

#' @title Minimum TN
#' @description Minimum of daily minimum air temperature
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5. \url{https://www.ecad.eu/documents/WCDMP_72_TD_1500_en_1.pdf}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' ntn(data=data_all$tn)
ntn = calculate_6 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[6] = C_degrees
index_titles[6] = "Minimum TN"
index_names[6] = "ntn"
attr(calculate_6, "data") <- c(TMIN)

#' @title Mean TG
#' @description Mean of daily mean air temperature
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' gtg(data=data_all$tg)
gtg = calculate_7 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[7] = C_degrees
index_titles[7] = "Mean TG"
index_names[7] = "gtg"
attr(calculate_7, "data") <- c(TMEAN)

#' @title Maximum TG
#' @description Maximum of daily mean air temperature
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' xtg(data=data_all$tg)
xtg = calculate_8 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[8] = C_degrees
index_titles[8] = "Maximum TG"
index_names[8] = "xtg"
attr(calculate_8, "data") <- c(TMEAN)

#' @title Minimum TG
#' @description Minimum value of daily mean air temperature
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Average temperature
#' @export
#' @examples
#' data(data_all)
#' ntg(data=data_all$tg)
ntg = calculate_9 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[9] = C_degrees
index_titles[9] = "Minimum TG"
index_names[9] = "ntg"
attr(calculate_9, "data") <- c(TMEAN)

#' @title Percentage of cold days
#' @description Percentages of days with TX lower than the 10th percentile.
#' @section Formula: \deqn{cd = \frac{No. days TX < 10p} {No. days} * 100}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return percentage of cold days
#' @export
#' @examples
#' data(data_all)
#' cd(data=data_all$tx)
cd = calculate_10 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.10))

  function_ = function(data, value){    
    value = select_value_for_data(data, value, time.scale)
    return(100*sum(data<value, na.rm = na.rm)/length(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[10] = C_days
index_titles[10] = "Percentage of cold days"
index_names[10] = "cd"
attr(calculate_10, "data") <- c(TMAX)

#' @title Percentage of cold nights
#' @description Percentages of days with TN lower than the 10th percentile.
#' @section Formula: \deqn{cn = \frac{No. days TN <  10p} {No. days} * 100} 
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return percentage of cold nights
#' @export
#' @examples
#' data(data_all)
#' cn(data=data_all$tn)
cn = calculate_11 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calculate_10(data, data_names, time.scale, na.rm = na.rm))
}
index_units[11] = C_days
index_titles[11] = "Percentage of cold nights"
index_names[11] = "cn"
attr(calculate_11, "data") <- c(TMIN)

#' @title Cold spell duration
#' @description Count of days with at least 6 consecutive days when TN < 10th percentile
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Cold spell duration index
#' @export
#' @examples
#' data(data_all)
#' cdd(data=data_all$tn)
cdd = calculate_12 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
   value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.10))

  function_ = function(data, value){
    if(na.rm | sum(is.na(data))==0){
      value = select_value_for_data(data, value, time.scale)
      data.10 = data <= value
      data.rle = rle(as.numeric(data.10))
      aux = data.rle$values==1 & data.rle$lengths>=6
      count = sum(data.rle$lengths[aux], na.rm = na.rm)
      return(count)
    }else{
      return(NA)
    }
  }

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)

  return(byYears)
}
index_units[12] = C_days
index_titles[12] = "Cold spell duration"
index_names[12] = "cdd"
attr(calculate_12, "data") <- c(TMIN)

#' @title Diurnal temperature range
#' @description Mean difference between TX and TN.
#' @section Formula: \deqn{DTR_j = \frac{ \sum_{ i = 1 } ^ { I } ( TX_{ ij } - TN_{ ij })} {I}}
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5. \url{https://www.ecad.eu/documents/WCDMP_72_TD_1500_en_1.pdf}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param tmax maximum temperature 
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Diurnal temperature range
#' @export
#' @examples
#' data(data_all)
#' dtr(tmax=data_all$tx, tmin=data_all$tn)
dtr = calculate_13 = function(tmax, tmin, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tmax - tmin
  byMonths = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=mean, na.rm = na.rm)
  return(byMonths)
}
index_units[13] = C_degrees
index_titles[13] = "Diurnal temperature range"
index_names[13] = "dtr"
attr(calculate_13, "data") <- c(TMAX, TMIN)

#' @title Mean daily difference DTR
#' @description Mean absolute day-to-day difference in DTR
#' @section Formula: \deqn{vDTR_j = \frac{ \sum_{ i = 1 } ^ { I } \mid ( TX_{ ij } - TN_{ ij } ) - ( TX_{i-1,j} -TN_{ i - 1,j }) \mid } {I}}
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param tmax maximum temperature 
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return vDTR
#' @export
#' @examples
#' data(data_all)
#' vdtr(tmax=data_all$tx, tmin=data_all$tn)
vdtr = calculate_14 = function(tmax, tmin, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tmax - tmin
  if(length(data)>0){
    data = abs(data[1:(length(data)-1)]-data[2:length(data)])
    data_names = data_names[-1]
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=mean, na.rm = na.rm)
  return(byYears)
}
index_units[14] = C_degrees
index_titles[14] = "Mean daily difference DTR"
index_names[14] = "vdtr"
attr(calculate_14, "data") <- c(TMAX, TMIN)

#' @title Frost days
#' @description Number of days with TN < 0 Celsius.
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return frost days
#' @export
#' @examples
#' data(data_all)
#' fd(data=data_all$tn)
fd = calculate_15 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data<0, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[15] = C_days
index_titles[15] = "Frost days"
index_names[15] = "fd"
attr(calculate_15, "data") <- c(TMIN)

#' @title Growing season length
#' @description Annual count of days between the first span of at least 6 days with TG > 5 Celsius and first span after 1 July of 6 days with TG < 5 Celsius.
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return growing season length
#' @export
#' @examples
#' data(data_all)
#' gsl(data=data_all$tg)
gsl = calculate_16 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    if(na.rm | sum(is.na(data))==0){
      BREAKVALUE = 5 #Grados de ruptura
      times.data = chron(names(data)) #Fechas a tratar
      aux = which(times.data == chron(paste0("7/1/", unique(years(times.data)))), arr.ind = TRUE, useNames = TRUE) # Fecha en la que se pasa de buscar mayores de 5 a menores de 5

      ### Fecha desde la que contar
      data.max = data>BREAKVALUE #Mayores que 5
      data.max[1:length(data.max)>aux] = FALSE
      data.max.rle = rle(as.numeric(data.max))
      aux.max = which(data.max.rle$length>6 & data.max.rle$values==1, arr.ind = TRUE, useNames = TRUE)[1]-1

      # Fecha hasta que la contar
      data.min = data<BREAKVALUE #Menores que 5
      data.min[1:length(data.min)<=aux] = FALSE #Less than 5 after July 1
      data.min.rle = rle(as.numeric(data.min))      
      aux.min = which(data.min.rle$length>6 & data.min.rle$values==1, arr.ind = TRUE, useNames = TRUE)[1]-1

      if(is.na(aux.min) | length(aux.min)<=0 | length(data.min)<=0 | is.na(data.min[1])){
        data.min = length(data) + 2
      }else{
        data.min = sum(data.min.rle$length[1:aux.min], arr.ind = TRUE, useNames = TRUE)
      }
      if(is.na(aux.max) | length(aux.max)<=0 | length(data.max)<=0 | is.na(data.max[1])){
        data.max = 1000
      }else{
        if(aux.max>0){
            data.max = sum(data.max.rle$length[1:aux.max], arr.ind = TRUE, useNames = TRUE) + 1
          }else{
            data.max = 1 + 2
          }
      }
      count = data.min - data.max + 1
      if(count<0) { 
        count = 0 
      }
      return(count)
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[16] = C_days
index_titles[16] = "Growing season length"
index_names[16] = "gsl"
attr(calculate_16, "data") <- c(TMEAN)

#' @title Ice days
#' @description Number of days with TX < 0 Celsius.
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return ice days
#' @export
#' @examples
#' data(data_all)
#' id(data=data_all$tx)
id = calculate_17 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data<0, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[17] = C_days
index_titles[17] = "Ice days"
index_names[17] = "id"
attr(calculate_17, "data") <- c(TMAX)

#' @title Maximum consecutive frost days
#' @description Maximum number of consecutive days with TN < 0 Celsius
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return maximum consecutive frost
#' @export
#' @examples
#' data(data_all)
#' cfd(data=data_all$tn)
cfd = calculate_18 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    VALUE = 0
    data.rle = rle(as.numeric(data<VALUE))  
    calculate = sum(data.rle$values>0, na.rm = na.rm)  
    if(!is.na(calculate) & calculate>0){
      calculate = max(data.rle$lengths[data.rle$values>0], na.rm = na.rm)
    }
    return(calculate)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[18] = C_days
index_titles[18] = "Maximum consecutive frost days"
index_names[18] = "cfd"
attr(calculate_18, "data") <- c(TMIN)

#' @title Extreme temperature range
#' @description Difference between the maximum TX and the minimum TN.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param tmax maximum temperature
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return extreme temperature range
#' @export
#' @examples
#' data(data_all)
#' etr(tmax=data_all$tx, tmin=data_all$tn)
etr = calculate_19 = function(tmax, tmin, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(tmax, tmin){
    calculate = max(tmax, na.rm = na.rm)-min(tmin[names(tmax)], na.rm = na.rm) 
    return(calculate)
  }
  byYears = calcf_data(data=tmax, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, tmin=tmin)
  return(byYears)
}
index_units[19] = C_degrees
index_titles[19] = "Extreme temperature range"
index_names[19] = "etr"
attr(calculate_19, "data") <- c(TMAX, TMIN)

#' @title Summer days
#' @description Number of days with maximum temperature > 25 Celsius.
#' Number of days with TX >25 Celsius.
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Summer days
#' @export
#' @examples
#' data(data_all)
#' sud(data=data_all$tx)
sud = calculate_20 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>25, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[20] = C_days
index_titles[20] = "Summer days"
index_names[20] = "sud"
attr(calculate_20, "data") <- c(TMAX)

#' @title Maximum consecutive summer days
#' @description Maximum number of consecutive summer days (TX > 25 Celsius)
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return consecutive summer days
#' @export
#' @examples
#' data(data_all)
#' csd(data=data_all$tx)
csd = calculate_21 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    data.rle = rle(as.numeric(data>25))
    if(sum(is.na(data))>0){
      calculate = NA
    }else{
      calculate = 0
    }
    if(length(data.rle)>0 & !is.na(data.rle$lengths[data.rle$values>0][1])){
      calculate = max(data.rle$lengths[data.rle$values>0], na.rm = na.rm)
      if(!is.na(calculate) & calculate<0){
        calculate = 0
      }
    }
    return(calculate)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[21] = C_days
index_titles[21] = "Maximum consecutive summer days"
index_names[21] = "csd"
attr(calculate_21, "data") <- c(TMAX)

#' @title Difference days above/below Tx17
#' @description (days tx > 17 Celsius)-(days TX < 17 Celsius)
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Temperature sums
#' @export
#' @examples
#' data(data_all)
#' dd17(data=data_all$tx)
dd17 = calculate_22 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>17, na.rm = na.rm)-sum(data<17, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[22] = C_days
index_titles[22] = "Difference days above/below Tx17"
index_names[22] = "dd17"
attr(calculate_22, "data") <- c(TMAX)

#' @title Tropical nights
#' @description Number of days with TN > 20 Celsius.
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Tropical nights
#' @export
#' @examples
#' data(data_all)
#' tn(data=data_all$tn)
tn = calculate_23 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){  
    return(sum(data>20, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[23] = C_days
index_titles[23] = "Tropical nights"
index_names[23] = "tn"
attr(calculate_23, "data") <- c(TMIN)

#' @title Heating degree days
#' @description accumulated degree when TG is below 17 Celsius
#' @section Formula:\deqn{HD17_j = \sum_{j-1}^{I} (17 ^ oC - TG_ij)}
#' @references Quayle, R. G., & Diaz, H. F. (1980). Heating degree day data applied to residential heating energy consumption. Journal of Applied Meteorology, 19(3), 241-246. https://doi.org/10.1175/1520-0450(1980)019<0241:HDDDAT>2.0.CO;2
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return HD17
#' @export
#' @examples
#' data(data_all)
#' hd17(data=data_all$tg)
hd17 = calculate_24 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(17-data[!is.na(data) & data < 17], na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[24] = C_degrees
index_titles[24] = "Heating degree days"
index_names[24] = "hd17"
attr(calculate_24, "data") <- c(TMEAN)

#' @title Very cold days
#' @description Days with TN <1st percentile.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Very cold days
#' @export
#' @examples
#' data(data_all)
#' vcd(data=data_all$tn)
vcd = calculate_25 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.01))

  function_ = function(data, value){
    value = select_value_for_data(data, value, time.scale)
    return(sum(data<value, na.rm = na.rm))
  }

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[25] = C_days
index_titles[25] = "Very cold days"
index_names[25] = "vcd"
attr(calculate_25, "data") <- c(TMIN)

#' @title Very warm days
#' @description Days with TX >99th percentile per year.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Very warm days
#' @export
#' @examples
#' data(data_all)
#' vwd(data=data_all$tx)
vwd = calculate_26 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.99))
  function_ = function(data, value){
    value = select_value_for_data(data, value, time.scale)
    return(sum(data>value, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[26] = C_days
index_titles[26] = "Very warm days"
index_names[26] = "vwd"
attr(calculate_26, "data") <- c(TMAX)

#' @title Warm days
#' @description Total numbers of days with TX higher than the 90th percentile.
#' 
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Warm days
#' @export
#' @examples
#' data(data_all)
#' wd(data=data_all$tx)
wd = calculate_27 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.90))
  function_ = function(data, value){
    value = select_value_for_data(data, value, time.scale)
    return(100*sum(data>value, na.rm = na.rm)/length(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[27] = C_days
index_titles[27] = "Warm days"
index_names[27] = "wd"
attr(calculate_27, "data") <- c(TMAX)

#' @title Warm nights
#' @description Percentages of days with TN higher than the 90th percentile.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Warm nights
#' @export
#' @examples
#' data(data_all)
#' wn(data=data_all$tn)
wn = calculate_28 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.90))
  function_ = function(data, value){
    value = select_value_for_data(data, value, time.scale)
    return(100*sum(data>value, na.rm = na.rm)/length(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[28] = C_days
index_titles[28] = "Warm nights"
index_names[28] = "wn"
attr(calculate_28, "data") <- c(TMIN)

#' @title Warm spell duration
#' @description Count of days with at least 6 consecutive days when TX > 90th percentile.
## @importance Important application in agriculture, tourism, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Warm spell duration index
#' @export
#' @examples
#' data(data_all)
#' wsd(data=data_all$tx)
wsd = calculate_29 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.90))  
  function_ = function(data, value){
    if(na.rm | sum(is.na(data))==0){
      value = select_value_for_data(data, value, time.scale)
      data.rle = rle(as.numeric(data>value))
      aux = data.rle$values==1 & data.rle$lengths>=6
      count = sum(data.rle$lengths[aux], na.rm = na.rm)
      return(count)
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[29] = C_days
index_titles[29] = "Warm spell duration"
index_names[29] = "wsd"
attr(calculate_29, "data") <- c(TMAX)

#' @title Zero crossing days
#' @description Number of days with TX > 0 Celsius and TN < 0 Celsius.
#' 
## @importance Important application in agriculture, tourism, human health
#' 
#' @param tmax maximum temperature
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return zero crossing days
#' @export
#' @examples
#' data(data_all)
#' zcd(tmax=data_all$tx, tmin=data_all$tn)
zcd = calculate_30 = function(tmax, tmin, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tmax>0 & tmin<0
  function_ = function(data){
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[30] = C_days
index_titles[30] = "Zero crossing days"
index_names[30] = "zcd"
attr(calculate_30, "data") <- c(TMAX, TMIN)

#' @title Onset of growing season 6 days
#' @description Date of the start of the first span with at least 6 days with TG >5 Celsius
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Onset of growing season 1
#' @export
#' @examples
#' data(data_all)
#' ogs6(data=data_all$tg)
ogs6 = calculate_31 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    if(na.rm | sum(is.na(data))==0){
      data.rle = rle(as.numeric(data>5))
      aux = which(data.rle$values==1 & data.rle$lengths>=6, arr.ind = TRUE, useNames = TRUE)[1]
      if(is.na(aux)){
        return(0)
      }else{
        return(sum(data.rle$lengths[1:aux], na.rm = na.rm)-data.rle$lengths[aux]+1)
      }
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[31] = C_date
index_titles[31] = "Onset of growing season 6 days"
index_names[31] = "ogs6"
attr(calculate_31, "data") <- c(TMEAN)

#' @title Onset of growing season 10 days
#' @description Date of the start of the first span with at least 10 days with TG > 5 Celsius
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Onset of growing season 2
#' @export
#' @examples
#' data(data_all)
#' ogs10(data=data_all$tg)
ogs10 = calculate_32 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    if(na.rm | sum(is.na(data))==0){
      data.rle = rle(as.numeric(data>5))
      aux = which(data.rle$values==1 & data.rle$lengths>=10, arr.ind = TRUE, useNames = TRUE)[1]
      if(is.na(aux)){
        return(0)
      }else{
        return(sum(data.rle$lengths[1:aux], na.rm = na.rm)-data.rle$lengths[aux]+1)
      }
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[32] = C_date
index_titles[32] = "Onset of growing season 10 days"
index_names[32] = "ogs10"
attr(calculate_32, "data") <- c(TMEAN)

#' @title Growing season (Apr-Oct)
#' @description Growing season (april to october) mean TG
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return Growing season temperature 1
#' @export
#' @examples
#' data(data_all)
#' ta_o(data=data_all$tg)
ta_o = calculate_33 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]
    return(mean(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[33] = C_degrees
index_titles[33] = "Growing season (Apr-Oct)"
index_names[33] = "ta_o"
attr(calculate_33, "data") <- c(TMEAN)

#' @title Growing season(May-Sep)
#' @description Growing season (may to september) mean TG
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return Growing season temperature 2
#' @export
#' @examples
#' data(data_all)
#' tm_s(data=data_all$tg)
tm_s = calculate_34 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(MAY, JUN, JUL, AUG, SEP)]
    return(mean(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[34] = C_degrees
index_titles[34] = "Growing season(May-Sep)"
index_names[34] = "tm_s"
attr(calculate_34, "data") <- c(TMEAN)

#' @title Growing degree days
#' @description Sum of degree days of TG over 4 Celsius (the daily mean temperature is less than 4 celsius, it is set equal to 4 celsius)
#' @references McMaster, G. S., & Wilhelm, W. W. (1997). Growing degree-days: One equation, two interpretations. Agricultural and Forest Meteorology, 87(4), 291-300
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return GD4
#' @export
#' @examples
#' data(data_all)
#' gd4(data=data_all$tg)
gd4 = calculate_35 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data[data>4]-4, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[35] = C_degrees
index_titles[35] = "Growing degree days"
index_names[35] = "gd4"
attr(calculate_35, "data") <- c(TMEAN)

#' @title Winkler index
#' @description Sum of degree days over 10 celsius of TG from April 1 until October 31
#' @references Winkler, A.J., J.A. Cook, W.M. Kliewer, and L.A. Lider. 1974. General Viticulture. 4th ed. University of California Press, Berkeley.
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return Winkler index
#' @export
#' @examples
#' data(data_all)
#' wki(data = data_all$tg)
wki = calculate_36 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){  
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]
    return(sum(data[data>10]-10, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[36] = C_degrees
index_titles[36] = "Winkler index"
index_names[36] = "wki"
attr(calculate_36, "data") <- c(TMEAN)

#' @title Winter Severity
#' @description Mean TG of the coldest month of the year
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return Winter Severity index
#' @export
#' @examples
#' data(data_all)
#' ws(data = data_all$tg)
ws = calculate_37 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){  
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), data_names=NULL, operation=mean, na.rm = na.rm)
    if(sum(!is.na(byMonths))>0){
      return(byMonths[!is.na(byMonths) & byMonths==min(byMonths, na.rm = na.rm)][1])
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[37] = C_degrees
index_titles[37] = "Winter Severity"
index_names[37] = "ws"
attr(calculate_37, "data") <- c(TMEAN)

#' @title Sums TX32
#' @description Sum of degree days when TX >= 32 Celsius on the interval June-August. The 32 celsius limit is the critical biological threshold for the maximum air temperature from which the physiological optimal growth and development of wheat and maize plants.
## @importance Important application in agriculture
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return temperature sums 1
#' @export
#' @examples
#' data(data_all)
#' stx32(data = data_all$tx)
stx32 = calculate_38 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(JUN, JUL, AUG)]
    return(sum(abs(data[data>=32]-32), na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[38] = C_degrees
index_titles[38] = "Sums TX32"
index_names[38] = "stx32"
attr(calculate_38, "data") <- c(TMAX)

#' @title Days TX32
#' @description Number of days whith TX >= 32 Celsius on the interval June-August.
## @importance Important application in agriculture,water, human health
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return temperature sums 1
#' @export
#' @examples
#' data(data_all)
#' d32(data = data_all$tx)
d32 = calculate_39 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(JUN, JUL, AUG)]
    return(sum(data>=32, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[39] = C_days
index_titles[39] = "Days TX32"
index_names[39] = "d32"
attr(calculate_39, "data") <- c(TMAX)

#' @title Sums TN-15
#' @description Sum of degree days when TN <= -15 Celsius recorded in December-February interval
## @importance Important application in agriculture,energy, human health, tourism
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return temperature sums 2
#' @export
#' @examples
#' data(data_all)
#' stn15(data = data_all$tn)
stn15 = calculate_40 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sum(abs(data[data <= -15]-15), na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[40] = C_degrees
index_titles[40] = "Sums TN-15"
index_names[40] = "stn15"
attr(calculate_40, "data") <- c(TMIN)

#' @title Sums TN-10
#' @description Sum of degree days when TN <=-10 Celsius recorded in December-February interval
## @importance Important application in agriculture,energy, human health, tourism
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return temperature sums 3
#' @export
#' @examples
#' data(data_all)
#' stn10(data = data_all$tn)
stn10 = calculate_41 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sum(abs(data[data <= -10]-10), na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[41] = C_degrees
index_titles[41] = "Sums TN-10"
index_names[41] = "stn10"
attr(calculate_41, "data") <- c(TMIN)

#' @title Sums positive
#' @description Sums of positive TG calculated for the 1st of February to the 10th April interval
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return temperature sums 5
#' @export
#' @examples
#' data(data_all)
#' ptg(data = data_all$tg)
ptg = calculate_42 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    time = chron(names(data))
    t.ini = chron(paste("2", "1", unique(years(time)), sep="/"))
    t.end = chron(paste("4", "10", unique(years(time)), sep="/"))
    data = data[chron(names(data))>=t.ini & chron(names(data))<=t.end]
    return(sum(data[data >= 0], na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[42] = C_degrees
index_titles[42] = "Sums positive"
index_names[42] = "ptg"
attr(calculate_42, "data") <- c(TMEAN)

####Precipitation-based
#' @title Total precipitation
#' @description Total amounts of precipitation
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return total precipitation
#' @export
#' @examples
#' data(data_all)
#' rti(data = data_all$rr)
rti = calculate_43 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[43] = C_precipitation
index_titles[43] = "Total precipitation"
index_names[43] = "rti"
attr(calculate_43, "data") <- c(PRECIPITATION)

#' @title Maximum precipitation
#' @description The highest amount of daily precipitation
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return maximum precipitation
#' @export
#' @examples
#' data(data_all)
#' rx(data = data_all$rr)
rx = calculate_44 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(max(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[44] = C_precipitation
index_titles[44] = "Maximum precipitation"
index_names[44] = "rx"
attr(calculate_44, "data") <- c(PRECIPITATION)

#' @title Days precipitation >= R10mm
#' @description Days with daily precipitation amount >= 10mm
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return R10mm
#' @export
#' @examples
#' data(data_all)
#' r10mm(data = data_all$rr)
r10mm = calculate_45 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  #Ej. function_(data=data[unique(years(names(data)))[12]==years(names(data))])
  function_ = function(data){
    return(sum(data>=10, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, data_names=data_names, extract_names=select_time_function(time.scale), operation=function_)
  return(byYears)
}
index_units[45] = C_days
index_titles[45] = "Days precipitation >= R10mm"
index_names[45] = "r10mm"
attr(calculate_45, "data") <- c(PRECIPITATION)

#' @title Days precipitation >= R20mm
#' @description Days with daily precipitation amount >= 20mm
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return R20mm
#' @export
#' @examples
#' data(data_all)
#' r20mm(data = data_all$rr)
r20mm = calculate_46 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=20, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[46] = C_days
index_titles[46] = "Days precipitation >= R20mm"
index_names[46] = "r20mm"
attr(calculate_46, "data") <- c(PRECIPITATION)

#' @title Maximum 5 days R
#' @description Maximum consecutive 5-day precipitation 
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Rx5day
#' @export
#' @examples
#' data(data_all)
#' rx5d(data = data_all$rr)
rx5d = calculate_50 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    data2 = c(data[c(2:length(data))], 0)
    data3 = c(data[c(3:length(data))], 0, 0)
    data4 = c(data[c(4:length(data))], 0, 0, 0)
    data5 = c(data[c(5:length(data))], 0, 0, 0, 0)
    data.sum = data + data2 + data3 + data4 + data5
    return(max(data.sum, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[50] = C_precipitation
index_titles[50] = "Maximum 5 days R"
index_names[50] = "rx5d"
attr(calculate_50, "data") <- c(PRECIPITATION)

#' @title Simple precipitation intensity index
#' @description Sum of precipitation in wet days (days with >1mm of precipitation), and dividing that by the number of wet days in the period.
#' @references Michele Brunetti, Maurizio Maugerib, Teresa Nanni, (2001) Changes in total precipitation, rainy days and extreme events in northeastern Italy, International Journal of Climatology
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return SDII
#' @export
#' @examples
#' data(data_all)
#' sdii(data = data_all$rr)
sdii = calculate_51 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    if(sum(is.na(data))>0 | sum(data>=1, na.rm = na.rm)>0){
      data_calc = sum(data[data>=1], na.rm = na.rm)/sum(data>=1, na.rm = na.rm)
    }else{
      data_calc = 0
    }
    return(data_calc)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[51] = C_precipitation
index_titles[51] = "Simple precipitation intensity index"
index_names[51] = "sdii"
attr(calculate_51, "data") <- c(PRECIPITATION)

#' @title Dry days
#' @description Number of days with less than 1 mm
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return dry days
#' @export
#' @examples
#' data(data_all)
#' dd(data = data_all$rr)
dd = calculate_52 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data < 1, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[52] = C_days
index_titles[52] = "Dry days"
index_names[52] = "dd"
attr(calculate_52, "data") <- c(PRECIPITATION)

#' @title Effective precipitation
#' @description Precipitation minus evapotranspiration
## @importance Important application in agriculture and water
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return effective precipitation
#' @export
#' @examples
#' data(data_all)
#' ep(pr = data_all$rr, eto = data_all$evapotranspiration)
ep = calculate_53 = function(eto, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(pr, eto){
    return(sum(pr-eto[names(pr)], na.rm = na.rm))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, eto=eto)
  return(byYears)
}
index_units[53] = C_precipitation
index_titles[53] = "Effective precipitation"
index_names[53] = "ep"
attr(calculate_53, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' @title Longest dry period
#' @description Maximum length of consecutive dry days (RR<1)
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return longest dry period
#' @export
#' @examples
#' data(data_all)
#' ldp(data = data_all$rr)
ldp = calculate_54 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    data.rle = rle(as.numeric(data<1))
    count = sum(data.rle$values==1, na.rm = na.rm)
    if(!is.na(count) & count>0){
      count = max(data.rle$lengths[data.rle$values==1], na.rm = na.rm)
    }
    return(count)  
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[54] = C_days
index_titles[54] = "Longest dry period"
index_names[54] = "ldp"
attr(calculate_54, "data") <- c(PRECIPITATION)

#' @title Longest wet period
#' @description Maximum length of consecutive wet days (RR>=1)
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return longest wet period
#' @export
#' @examples
#' data(data_all)
#' lwp(data = data_all$rr)
lwp = calculate_55 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    data.rle = rle(as.numeric(data>=1))
    count = sum(data.rle$values==1, na.rm = na.rm)
    if(!is.na(count) & count>0){
      count = max(data.rle$lengths[data.rle$values==1], na.rm = na.rm)
    }
    return(count)  
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[55] = C_days
index_titles[55] = "Longest wet period"
index_names[55] = "lwp"
attr(calculate_55, "data") <- c(PRECIPITATION)

#' @title Percentage precipitation of very wet days
#' @description Precipitation at days exceeding the 95percentile divided by total precipitation expressed in percentage
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return PVWD
#' @export
#' @examples
#' data(data_all)
#' r95tot(data = data_all$rr)
r95tot = calculate_56 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data[data>0], extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.95))
  function_ = function(data, value){
    if(sum(is.na(data))>0){ return(NA) }
    value = select_value_for_data(data, value, time.scale)
    data = 100*sum(data[data>value], na.rm = na.rm)/sum(data, na.rm = na.rm)
    if(is.na(data) | data==Inf | data==-Inf){ data = 0 }
    return(data)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[56] = C_precipitation
index_titles[56] = "Precipitation fraction very wet days"
index_names[56] = "r95tot"
attr(calculate_56, "data") <- c(PRECIPITATION)

#' @title Precipitation fraction extremely wet days
#' @description Precipitation at days exceeding the 99percentile divided by total precipitation expressed in percentage
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return PEWD
#' @export
#' @examples
#' data(data_all)
#' r99tot(data = data_all$rr)
r99tot = calculate_57 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data[data>0], extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.99))
  function_ = function(data, value){
    if(sum(is.na(data))>0){ return(NA) }
    value = select_value_for_data(data, value, time.scale)
    return(100*sum(data[data>value], na.rm = na.rm)/sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  byYears[is.nan(byYears) | byYears==Inf | byYears==-Inf] = 0
  return(byYears)
}
index_units[57] = C_precipitation
index_titles[57] = "Precipitation fraction extremely wet days"
index_names[57] = "r99tot"
attr(calculate_57, "data") <- c(PRECIPITATION)

#' @title Heavy precipitation days
#' @description Number of days with precipitation above 50mm
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return heavy precipitation days
#' @export
#' @examples
#' data(data_all)
#' d50mm(data = data_all$rr)
d50mm = calculate_58 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data, value){
    return(sum(data>value, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=50)
  return(byYears)
}
index_units[58] = C_days
index_titles[58] = "Heavy precipitation days"
index_names[58] = "d50mm"
attr(calculate_58, "data") <- c(PRECIPITATION)

#' @title Very wet days
#' @description Days with precipitation > 95p
#' @references Klein Tank AMG, Zwiers FW, Zhang X. 2009. Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation, climate data and monitoring WCDMP-No 72, WMO-TD No 1500, p 5.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return R95p
#' @export
#' @examples
#' data(data_all)
#' d95p(data = data_all$rr)
d95p = calculate_59 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.95))
  function_ = function(data, value){
    value = select_value_for_data(data, value, time.scale)
    return(sum(data>value, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[59] = C_days
index_titles[59] = "Very wet days"
index_names[59] = "d95p"
attr(calculate_59, "data") <- c(PRECIPITATION)

#' @title Precipitation Concentration Index
#' @description Index to evaluate precipitation heterogeneity at a monthly scale. Values <10 (uniform monthly rainfall distribution); values 11-15 (moderate concentration of precipitation); values 16-20 (irregular distribution); and >20 ((high precipitation concentration)
#' @section Formula: \deqn{PCI = \frac{\sum_{i=1}^{12} P_i ^ 2} {(P_t) ^ 2} * 100}
## @importance Important application in agriculture and water
#' @references Oliver, J.E. (1980) Monthly precipitation distribution: a comparative index. Professional Geographer, 32, 300–309

#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return PCI
#' @export
#' @examples
#' data(data_all)
#' pci(data = data_all$rr)
pci = calculate_60 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sum, na.rm = na.rm)
    data2Sum = sum(byMonths^2, na.rm = na.rm)
    dataSum2 = sum(byMonths, na.rm = na.rm)^2
    return(100*data2Sum/dataSum2)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[60] = C_precipitation
index_titles[60] = "Precipitation Concentration Index"
index_names[60] = "pci"
attr(calculate_60, "data") <- c(PRECIPITATION)

#' @title Modified Fournier Index
#' @description The precipitation concentration index is frequently associated to erosion risk.  Values: 0-60 very low; 60-90 Low; 90-120 moderate; 120-160 high; > 160 very high.
#' @section Formula: \deqn{MFI = \sum_{i=1}^{12} \frac{P_i ^ 2} {P_t}}
#' @references Fournier F. 1960. Climat et Erosion. PUF: Paris. Arnoldus HM. 1980. An approximation of the rainfall factor in the Uni-versal Soil Loss Equation. In Assessments of Erosion, de Boodts M,Gabriels D (eds). John Wiley and Sons Ltd, Chichester 127–132. De Luis M., González-Hidalgo J.C., Longares L.A. Is rainfal erosivity increasing in the Mediterranean Iberian Peninsula?. Land Degradation & Development, 21: 139-144. 
#' 
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return MFI
#' @export
#' @examples
#' data(data_all)
#' mfi(data = data_all$rr)
mfi = calculate_61 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sum, na.rm = na.rm)
    data2Sum = sum(byMonths^2, na.rm = na.rm)
    dataSum2 = sum(byMonths, na.rm = na.rm)
    return(data2Sum/dataSum2)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[61] = C_precipitation
index_titles[61] = "Modified Fournier Index"
index_names[61] = "mfi"
attr(calculate_61, "data") <- c(PRECIPITATION)

#' @title Growing season precipitation
#' @description Growing season (april to october) total precipitation
## @importance Important application in agriculture
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return GSP
#' @export
#' @examples
#' data(data_all)
#' gsr(data = data_all$rr)
gsr = calculate_62 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]    
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[62] = C_precipitation
index_titles[62] = "Growing season precipitation"
index_names[62] = "gsr"
attr(calculate_62, "data") <- c(PRECIPITATION)

#' @title Non-growing season precipitation
#' @description Total precipitation from October to April
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return non growing precipitation
#' @export
#' @examples
#' data(data_all)
#' ngsr(data = data_all$rr)
ngsr = calculate_63 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(OCT, NOV, DEC, JAN, FEB, MAR, APR)]
    return(sum(data, na.rm = na.rm))
  }
  # Ej. 1956: Oct:Dec 1956 + Jan:April 1957
  i = grep("01/01/", names(data))[2] # First day of the second year
  data2 = c(data[i:length(data)], rep(NA, i-1))
  names(data2) = names(data)

  data[months(chron(names(data)))%in%c(JAN, FEB, MAR, APR)] = data2[months(chron(names(data)))%in%c(JAN, FEB, MAR, APR)]

  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[63] = C_precipitation
index_titles[63] = "Non-growing season precipitation"
index_names[63] = "ngsr"
attr(calculate_63, "data") <- c(PRECIPITATION)

#' @title Total precipitation wet days
#' @description Precipitation amount on days with RR >= 1 mm
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, water management and tourism.
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return precipitation in wet days
#' @export
#' @examples
#' data(data_all)
#' rtwd(data = data_all$rr)
rtwd = calculate_64 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data[data>=1], na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[64] = C_precipitation
index_titles[64] = "Total precipitation wet days"
index_names[64] = "rtwd"
attr(calculate_64, "data") <- c(PRECIPITATION)

#' @title Wet days 1mm
#' @description Total number of wet days >= 1 mm
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, water management and tourism.
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return RR1
#' @export
#' @examples
#' data(data_all)
#' dr1mm(data = data_all$rr)
dr1mm = calculate_65 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=1, na.rm = na.rm)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[65] = C_precipitation
index_titles[65] = "Wet days 1mm"
index_names[65] = "dr1mm"
attr(calculate_65, "data") <- c(PRECIPITATION)

#' @title Wet days 3mm
#' @description Total number of Wet days >= 3mm 
#' 
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture, water management and tourism.
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return RR3
#' @export
#' @examples
#' data(data_all)
#' dr3mm(data = data_all$rr)
dr3mm = calculate_66 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=3, na.rm = na.rm)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[66] = C_precipitation
index_titles[66] = "Wt days 3mm"
index_names[66] = "dr3mm"
attr(calculate_66, "data") <- c(PRECIPITATION)

####Bioclimatic
#' @title TG of warmest quarter
#' @description TG of the warmest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @param ... ... 
#' @return BIO10
#' @export
#' @examples
#' data(data_all)
#' bio10(data = data_all$tg)
bio10 = calculate_67 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = months_quarter(functionValues=data, selectFunction=max, na.rm=na.rm)
    return(mean(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[67] = C_degrees
index_titles[67] = "TG of warmest quarter"
index_names[67] = "bio10"
attr(calculate_67, "data") <- c(TMEAN)

#' @title TG of coldest quarter
#' @description TG of coldest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @param ... ... 
#' @return BIO11
#' @export
#' @examples
#' data(data_all)
#' bio11(data = data_all$tg)
bio11 = calculate_68 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = months_quarter(functionValues=data, selectFunction=min, na.rm=na.rm)
    return(mean(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[68] = C_degrees
index_titles[68] = "TG of coldest quarter"
index_names[68] = "bio11"
attr(calculate_68, "data") <- c(TMEAN)

#' @title Precipitation of wettest month
#' @description Total precipitation of the wettest month of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO13
#' @export
#' @examples
#' data(data_all)
#' bio13(data = data_all$rr)
bio13 = calculate_69 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sum, na.rm = na.rm)
    return(max(byMonths, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[69] = C_precipitation
index_titles[69] = "Precipitation of wettest month"
index_names[69] = "bio13"
attr(calculate_69, "data") <- c(PRECIPITATION)

#' @title Precipitation of driest month
#' @description Total precipitation of the driest month of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO14
#' @export
#' @examples
#' data(data_all)
#' bio14(data = data_all$rr)
bio14 = calculate_70 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sum, na.rm = na.rm)
    return(min(byMonths, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[70] = C_precipitation
index_titles[70] = "Precipitation of driest month"
index_names[70] = "bio14"
attr(calculate_70, "data") <- c(PRECIPITATION)

#' @title Precipitation coefficient of variation 
#' @description The coefficient of variation is a measure of the variation in monthly precipitation totals over the course of the year. This index is the ratio of the standard deviation of the monthly total precipitation to the mean monthly total precipitation and is expressed as a percentage.
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
#' This is a measure of the variation in monthly precipitation totals over the course of the year. This index is the ratio of the standard deviation of the monthly total precipitation to the mean monthly total precipitation (also known as the coefficient of variation) and is expressed as a percentage.
## @importance Important application in agriculture
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO15
#' @export
#' @examples
#' data(data_all)
#' bio15(data = data_all$rr)
bio15 = calculate_71 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    byMonth = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sum, na.rm = na.rm)
    return(100*stats::sd(byMonth, na.rm = na.rm)/mean(byMonth, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[71] = C_index
index_titles[71] = "Coefficient of variation precipitation"
index_names[71] = "bio15"
attr(calculate_71, "data") <- c(PRECIPITATION)

#' @title Precipitation wettest quarter
#' @description Precipitation of the wettest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @param ... ... 
#' @return BIO16
#' @export
#' @examples
#' data(data_all)
#' bio16(data = data_all$rr, na.rm = TRUE)
bio16 = calculate_72 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = months_quarter(functionValues=data, selectFunction=max, na.rm=na.rm)
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[72] = C_precipitation
index_titles[72] = "Precipitation wettest quarter"
index_names[72] = "bio16"
attr(calculate_72, "data") <- c(PRECIPITATION)

#' @title Precipitation of Driest Quarter
#' @description Precipitation of the driest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO17
#' @export
#' @examples
#' data(data_all)
#' bio17(data = data_all$rr)
bio17 = calculate_73 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data_calc = months_quarter(functionValues=data, selectFunction=min, na.rm=na.rm)
    return(sum(data_calc, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[73] = C_precipitation
index_titles[73] = "Precipitation driest quarter"
index_names[73] = "bio17"
attr(calculate_73, "data") <- c(PRECIPITATION)

#' @title Precipitation warmest quarter
#' @description Precipitation of the warmest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO18
#' @export
#' @examples
#' data(data_all)
#' bio18(pr=data_all$rr, taverage=data_all$tg)
bio18 = calculate_74 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, taverage){
    data = months_quarter(functionValues=taverage[names(data)], selectFunction=max, selectValues=data, na.rm=na.rm)
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}
index_units[74] = C_precipitation
index_titles[74] = "Precipitation warmest quarter"
index_names[74] = "bio18"
attr(calculate_74, "data") <- c(PRECIPITATION, TMEAN)

#' @title Precipitation coldest quarter
#' @description Precipitation of the coldest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO19
#' @export
#' @examples
#' data(data_all)
#' bio19(pr=data_all$rr, taverage=data_all$tg)
bio19 = calculate_75 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, taverage){
    data = months_quarter(functionValues=taverage[names(data)], selectFunction=min, selectValues=data, na.rm=na.rm)
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}
index_units[75] = C_precipitation
index_titles[75] = "Precipitation coldest quarter"
index_names[75] = "bio19"
attr(calculate_75, "data") <- c(PRECIPITATION, TMEAN)

#' @title Temperature seasonality
#' @description TG standard deviation *100
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO4
#' @export
#' @examples
#' data(data_all)
#' bio4(data = data_all$tg)
bio4 = calculate_76 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    return(100*stats::sd(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[76] = C_degrees
index_titles[76] = "Temperature seasonality"
index_names[76] = "bio4"
attr(calculate_76, "data") <- c(TMEAN)

#' @title TX warmest month
#' @description TX of the warmest month of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data mean temperature
#' @param tmax maximum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO5
#' @export
#' @examples
#' data(data_all)
#' bio5(data = data_all$tg, tmax = data_all$tx)
bio5 = calculate_77 = function(data, tmax, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, tmax){
    byMonths.mean = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    byMonths.max = calcf_data(data=tmax[names(data)], extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    return(byMonths.max[max(byMonths.mean, na.rm=na.rm)==byMonths.mean][1])
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_, tmax=tmax)
  return(byYears)
}
index_units[77] = C_degrees
index_titles[77] = "TX warmest month"
index_names[77] = "bio5"
attr(calculate_77, "data") <- c(TMEAN, TMAX)

#' @title TN of coldest month
#' @description TN of the coldest month of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data mean temperature
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO6
#' @export
#' @examples
#' data(data_all)
#' bio6(data = data_all$tg, tmin = data_all$tn)
bio6 = calculate_78 = function(data, tmin, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, tmin){
    byMonths.mean = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    byMonths.min = calcf_data(data=tmin[names(data)], extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    return(byMonths.min[min(byMonths.mean, na.rm=na.rm)==byMonths.mean][1])
  }
  byYears = calcf_data(data=data, data_names=data_names, extract_names=years, operation=function_, tmin=tmin)
  return(byYears)
}
index_units[78] = C_degrees
index_titles[78] = "TN of coldest month"
index_names[78] = "bio6"
attr(calculate_78, "data") <- c(TMEAN, TMIN)

#' @title Temperature Annual Range
#' @description  TX of the warmest month minus TN of coldest month
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param tmin min temperature
#' @param tmax max temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO7
#' @export
#' @examples
#' data(data_all)
#' bio7(data = data_all$tg, tmin = data_all$tn, tmax = data_all$tx)
bio7 = calculate_79 = function(data, tmin, tmax, data_names=NULL, na.rm = FALSE, ...){
  return(bio5(data, tmax, data_names=data_names, na.rm=na.rm)-bio6(data, tmin, data_names=data_names, na.rm=na.rm))
}
index_units[79] = C_index
index_titles[79] = "Difference warmest/coldest month"
index_names[79] = "bio7"
attr(calculate_79, "data") <- c(TMEAN, TMIN, TMAX)

#' @title TG of wettest quarter
#' @description TG of the wettest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO8
#' @export
#' @examples
#' data(data_all)
#' bio8(pr = data_all$rr, taverage = data_all$tg)
bio8 = calculate_80 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, pr){
    byMonths = months_quarter(functionValues=pr[names(data)], selectFunction=max, selectValues=data, na.rm=na.rm)
    return(mean(byMonths, na.rm=na.rm))
  }
  byYears = calcf_data(data=taverage, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, pr=pr)
  return(byYears)
}
index_units[80] = C_degrees
index_titles[80] = "TG of wettest quarter"
index_names[80] = "bio8"
attr(calculate_80, "data") <- c(PRECIPITATION, TMEAN)

#' @title TG of driest quarter
#' @description TG of the driest quarter of the year
#' @references Hijmans RJ, Cameron SE, Parra JL, Jones PG, Jarvis A (2005) Very high resolution interpolated climate surfaces for global land areas. Int J Climatol 25:1965–1978. doi: 10.1002/joc.1276. \url{http://www.worldclim.org/bioclim}
## @importance Important application in agriculture
#' 
#' @param pr precipitation
#' @param taverage medium temperature 
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return BIO9
#' @export
#' @examples
#' data(data_all)
#' bio9(pr = data_all$rr, taverage =  data_all$tg)
bio9 = calculate_81 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, pr){
    byMonths = months_quarter(functionValues=pr[names(data)], selectFunction=min, selectValues=data, na.rm=na.rm)
    return(mean(byMonths, na.rm=na.rm))
  }
  byYears = calcf_data(data=taverage, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, pr=pr)
  return(byYears)
}
index_units[81] = C_degrees
index_titles[81] = "TG of driest quarter"
index_names[81] = "bio9"
attr(calculate_81, "data") <- c(PRECIPITATION, TMEAN)

#' @title Mean radiation
#' @description Mean radiation (W m-2)
#' @references Kriticos, D.J., Webber, B.L., Leriche, A., Ota, N., Macadam, I., Bathols, J. and Scott, J.K. (2012) CliMond: global high-resolution historical and future scenario climate surfaces for bioclimatic modelling. Methods in Ecology and Evolution, 3, 53-64. http://dx.doi.org/10.1111/j.2041-210X.2011.00134.x
#' 
#' @param data radiation en w/m2
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO20
#' @export
#' @examples
#' data(data_all)
#' bio20(data = data_all$radiation_w)
bio20 = calculate_82 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm=na.rm))
}
index_units[82] = C_radiation
index_titles[82] = "Mean radiation"
index_names[82] = "bio20"
attr(calculate_82, "data") <- c(RADIATION_W)

#' @title Universal Thermal Climate Index
#' @description The Universal Thermal Climate Index is expressed as an equivalent ambient temperature (Celsius) of a reference environment providing the same physiological response of a reference person as the actual environment
#' \url{http://www.utci.org/} http://www.utci.org/utci_doku.php
#' Copy \url{https://github.com/alfcrisci/rBiometeo}
#'Given air temperature (Celsius), relative humidity (\%), wind velocity (m/sec) and mean radiant temperature ( tmrt in Celsius degree) gives the Universal Thermal Climate Index in Celsius.
## @importance Important application in tourism, energy and health
#' 
#' @references Blazejczyk, K., Epstein, Y., Jendritzky, G., Staiger, H., & Tinz, B. (2012). Comparison of UTCI to selected thermal indices. International Journal of Biometeorology, 56(3), 515-535. doi:10.1007/s00484-011-0453-2
#' @param ta medium temperature
#' @param rh humidity
#' @param wind average wind
#' @param tmrt radiation temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return UTCI
#' @export
#' @examples
#' data(data_all)
#' utci(ta = data_all$tg, rh = data_all$dewpoint, wind = data_all$wind, 
#'      tmrt = data_all$"RADIATIONTEMPERATURE")
utci = calculate_83 = function(ta, rh, wind, tmrt, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
    if(is.null(ta) | is.null(rh) | is.null(wind)) {
      return(NULL) 
    }

    e = ta/10 # e = es(ta)/10; # use vapour pressure in kPa 
    pa = (e*rh/100.0)
    va = wind;
    va[va < 0.5] = 0.5
    va[va > 17] = 17

    if(!is.null(tmrt)){
      dtm = tmrt - ta
    }else{
      dtm = 0
    }

    utci = array(NA, dim=c(length(ta), 211));
    
    utci[, 211]=ta;
    utci[, 1]=6.07562052E-01;
    utci[, 2]=-2.27712343E-02*ta;
    utci[, 3]=8.06470249E-04*ta*ta;
    utci[, 4]=-1.54271372E-04*ta*ta*ta;
    utci[, 5]=-3.24651735E-06*ta*ta*ta*ta;
    utci[, 6]=7.32602852E-08*ta*ta*ta*ta*ta;
    utci[, 7]=1.35959073E-09*ta*ta*ta*ta*ta*ta;
    utci[, 8]=-2.25836520E-00*va;
    utci[, 9]=8.80326035E-02*ta*va;
    utci[, 10]=2.16844454E-03*ta*ta*va;
    utci[, 11]=-1.53347087E-05*ta*ta*ta*va;
    utci[, 12]=-5.72983704E-07*ta*ta*ta*ta*va;
    utci[, 13]=-2.55090145E-09*ta*ta*ta*ta*ta*va;
    utci[, 14]=-7.51269505E-01*va*va;
    utci[, 15]=-4.08350271E-03*ta*va*va;
    utci[, 16]=-5.21670675E-05*ta*ta*va*va;
    utci[, 17]=1.94544667E-06*ta*ta*ta*va*va;
    utci[, 18]=1.14099531E-08*ta*ta*ta*ta*va*va;
    utci[, 19]=1.58137256E-01*va*va*va;
    utci[, 20]=-6.57263143E-05*ta*va*va*va;
    utci[, 21]=2.22697524E-07*ta*ta*va*va*va;
    utci[, 22]=-4.16117031E-08*ta*ta*ta*va*va*va;
    utci[, 23]=-1.27762753E-02*va*va*va*va;
    utci[, 24]=9.66891875E-06*ta*va*va*va*va;
    utci[, 25]=2.52785852E-09*ta*ta*va*va*va*va;
    utci[, 26]=4.56306672E-04*va*va*va*va*va;
    utci[, 27]=-1.74202546E-07*ta*va*va*va*va*va;
    utci[, 28]=-5.91491269E-06*va*va*va*va*va*va;
    utci[, 29]=3.98374029E-01*dtm;
    utci[, 30]=1.83945314E-04*ta*dtm;
    utci[, 31]=-1.73754510E-04*ta*ta*dtm;
    utci[, 32]=-7.60781159E-07*ta*ta*ta*dtm;
    utci[, 33]=3.77830287E-08*ta*ta*ta*ta*dtm;
    utci[, 34]=5.43079673E-10*ta*ta*ta*ta*ta*dtm;
    utci[, 35]=-2.00518269E-02*va*dtm;
    utci[, 36]=8.92859837E-04*ta*va*dtm;
    utci[, 37]=3.45433048E-06*ta*ta*va*dtm;
    utci[, 38]=-3.77925774E-07*ta*ta*ta*va*dtm;
    utci[, 39]=-1.69699377E-09*ta*ta*ta*ta*va*dtm;
    utci[, 40]=1.69992415E-04*va*va*dtm;
    utci[, 41]=-4.99204314E-05*ta*va*va*dtm;
    utci[, 42]=2.47417178E-07*ta*ta*va*va*dtm;
    utci[, 43]=1.07596466E-08*ta*ta*ta*va*va*dtm;
    utci[, 44]=8.49242932E-05*va*va*va*dtm;
    utci[, 45]=1.35191328E-06*ta*va*va*va*dtm;
    utci[, 46]=-6.21531254E-09*ta*ta*va*va*va*dtm;
    utci[, 47]=-4.99410301E-06*va*va*va*va*dtm;
    utci[, 48]=-1.89489258E-08*ta*va*va*va*va*dtm;
    utci[, 49]=8.15300114E-08*va*va*va*va*va*dtm;
    utci[, 50]=7.55043090E-04*dtm*dtm;
    utci[, 51]=-5.65095215E-05*ta*dtm*dtm;
    utci[, 52]=-4.52166564E-07*ta*ta*dtm*dtm;
    utci[, 53]=2.46688878E-08*ta*ta*ta*dtm*dtm;
    utci[, 54]=2.42674348E-10*ta*ta*ta*ta*dtm*dtm;
    utci[, 55]=1.54547250E-04*va*dtm*dtm;
    utci[, 56]=5.24110970E-06*ta*va*dtm*dtm;
    utci[, 57]=-8.75874982E-08*ta*ta*va*dtm*dtm;
    utci[, 58]=-1.50743064E-09*ta*ta*ta*va*dtm*dtm;
    utci[, 59]=-1.56236307E-05*va*va*dtm*dtm;
    utci[, 60]=-1.33895614E-07*ta*va*va*dtm*dtm;
    utci[, 61]=2.49709824E-09*ta*ta*va*va*dtm*dtm;
    utci[, 62]=6.51711721E-07*va*va*va*dtm*dtm;
    utci[, 63]=1.94960053E-09*ta*va*va*va*dtm*dtm;
    utci[, 64]=-1.00361113E-08*va*va*va*va*dtm*dtm;
    utci[, 65]=-1.21206673E-05*dtm*dtm*dtm;
    utci[, 66]=-2.18203660E-07*ta*dtm*dtm*dtm;
    utci[, 67]=7.51269482E-09*ta*ta*dtm*dtm*dtm;
    utci[, 68]=9.79063848E-11*ta*ta*ta*dtm*dtm*dtm;
    utci[, 69]=1.25006734E-06*va*dtm*dtm*dtm;
    utci[, 70]=-1.81584736E-09*ta*va*dtm*dtm*dtm;
    utci[, 71]=-3.52197671E-10*ta*ta*va*dtm*dtm*dtm;
    utci[, 72]=-3.36514630E-08*va*va*dtm*dtm*dtm;
    utci[, 73]=1.35908359E-10*ta*va*va*dtm*dtm*dtm;
    utci[, 74]=4.17032620E-10*va*va*va*dtm*dtm*dtm;
    utci[, 75]=-1.30369025E-09*dtm*dtm*dtm*dtm;
    utci[, 76]=4.13908461E-10*ta*dtm*dtm*dtm*dtm;
    utci[, 77]=9.22652254E-12*ta*ta*dtm*dtm*dtm*dtm;
    utci[, 78]=-5.08220384E-09*va*dtm*dtm*dtm*dtm;
    utci[, 79]=-2.24730961E-11*ta*va*dtm*dtm*dtm*dtm;
    utci[, 80]=1.17139133E-10*va*va*dtm*dtm*dtm*dtm;
    utci[, 81]=6.62154879E-10*dtm*dtm*dtm*dtm*dtm;
    utci[, 82]=4.03863260E-13*ta*dtm*dtm*dtm*dtm*dtm;
    utci[, 83]=1.95087203E-12*va*dtm*dtm*dtm*dtm*dtm;
    utci[, 84]=-4.73602469E-12*dtm*dtm*dtm*dtm*dtm*dtm;
    utci[, 85]=5.12733497E-00*pa;
    utci[, 86]=-3.12788561E-01*ta*pa;
    utci[, 87]=-1.96701861E-02*ta*ta*pa;
    utci[, 88]=9.99690870E-04*ta*ta*ta*pa;
    utci[, 89]=9.51738512E-06*ta*ta*ta*ta*pa;
    utci[, 90]=-4.66426341E-07*ta*ta*ta*ta*ta*pa;
    utci[, 91]=5.48050612E-01*va*pa;
    utci[, 92]=-3.30552823E-03*ta*va*pa;
    utci[, 93]=-1.64119440E-03*ta*ta*va*pa;
    utci[, 94]=-5.16670694E-06*ta*ta*ta*va*pa;
    utci[, 95]=9.52692432E-07*ta*ta*ta*ta*va*pa;
    utci[, 96]=-4.29223622E-02*va*va*pa;
    utci[, 97]=5.00845667E-03*ta*va*va*pa;
    utci[, 98]=1.00601257E-06*ta*ta*va*va*pa;
    utci[, 99]=-1.81748644E-06*ta*ta*ta*va*va*pa;
    utci[, 100]=-1.25813502E-03*va*va*va*pa;
    utci[, 101]=-1.79330391E-04*ta*va*va*va*pa;
    utci[, 102]=2.34994441E-06*ta*ta*va*va*va*pa;
    utci[, 103]=1.29735808E-04*va*va*va*va*pa;
    utci[, 104]=1.29064870E-06*ta*va*va*va*va*pa;
    utci[, 105]=-2.28558686E-06*va*va*va*va*va*pa;
    utci[, 106]=-3.69476348E-02*dtm*pa;
    utci[, 107]=1.62325322E-03*ta*dtm*pa;
    utci[, 108]=-3.14279680E-05*ta*ta*dtm*pa;
    utci[, 109]=2.59835559E-06*ta*ta*ta*dtm*pa;
    utci[, 110]=-4.77136523E-08*ta*ta*ta*ta*dtm*pa;
    utci[, 111]=8.64203390E-03*va*dtm*pa;
    utci[, 112]=-6.87405181E-04*ta*va*dtm*pa;
    utci[, 113]=-9.13863872E-06*ta*ta*va*dtm*pa;
    utci[, 114]=5.15916806E-07*ta*ta*ta*va*dtm*pa;
    utci[, 115]=-3.59217476E-05*va*va*dtm*pa;
    utci[, 116]=3.28696511E-05*ta*va*va*dtm*pa;
    utci[, 117]=-7.10542454E-07*ta*ta*va*va*dtm*pa;
    utci[, 118]=-1.24382300E-05*va*va*va*dtm*pa;
    utci[, 119]=-7.38584400E-09*ta*va*va*va*dtm*pa;
    utci[, 120]=2.20609296E-07*va*va*va*va*dtm*pa;
    utci[, 121]=-7.32469180E-04*dtm*dtm*pa;
    utci[, 122]=-1.87381964E-05*ta*dtm*dtm*pa;
    utci[, 123]=4.80925239E-06*ta*ta*dtm*dtm*pa;
    utci[, 124]=-8.75492040E-08*ta*ta*ta*dtm*dtm*pa;
    utci[, 125]=2.77862930E-05*va*dtm*dtm*pa;
    utci[, 126]=-5.06004592E-06*ta*va*dtm*dtm*pa;
    utci[, 127]=1.14325367E-07*ta*ta*va*dtm*dtm*pa;
    utci[, 128]=2.53016723E-06*va*va*dtm*dtm*pa;
    utci[, 129]=-1.72857035E-08*ta*va*va*dtm*dtm*pa;
    utci[, 130]=-3.95079398E-08*va*va*va*dtm*dtm*pa;
    utci[, 131]=-3.59413173E-07*dtm*dtm*dtm*pa;
    utci[, 132]=7.04388046E-07*ta*dtm*dtm*dtm*pa;
    utci[, 133]=-1.89309167E-08*ta*ta*dtm*dtm*dtm*pa;
    utci[, 134]=-4.79768731E-07*va*dtm*dtm*dtm*pa;
    utci[, 135]=7.96079978E-09*ta*va*dtm*dtm*dtm*pa;
    utci[, 136]=1.62897058E-09*va*va*dtm*dtm*dtm*pa;
    utci[, 137]=3.94367674E-08*dtm*dtm*dtm*dtm*pa;
    utci[, 138]=-1.18566247E-09*ta*dtm*dtm*dtm*dtm*pa;
    utci[, 139]=3.34678041E-10*va*dtm*dtm*dtm*dtm*pa;
    utci[, 140]=-1.15606447E-10*dtm*dtm*dtm*dtm*dtm*pa;
    utci[, 141]=-2.80626406E-00*pa*pa;
    utci[, 142]=5.48712484E-01*ta*pa*pa;
    utci[, 143]=-3.99428410E-03*ta*ta*pa*pa;
    utci[, 144]=-9.54009191E-04*ta*ta*ta*pa*pa;
    utci[, 145]=1.93090978E-05*ta*ta*ta*ta*pa*pa;
    utci[, 146]=-3.08806365E-01*va*pa*pa;
    utci[, 147]=1.16952364E-02*ta*va*pa*pa;
    utci[, 148]=4.95271903E-04*ta*ta*va*pa*pa;
    utci[, 149]=-1.90710882E-05*ta*ta*ta*va*pa*pa;
    utci[, 150]=2.10787756E-03*va*va*pa*pa;
    utci[, 151]=-6.98445738E-04*ta*va*va*pa*pa;
    utci[, 152]=2.30109073E-05*ta*ta*va*va*pa*pa;
    utci[, 153]=4.17856590E-04*va*va*va*pa*pa;
    utci[, 154]=-1.27043871E-05*ta*va*va*va*pa*pa;
    utci[, 155]=-3.04620472E-06*va*va*va*va*pa*pa;
    utci[, 156]=5.14507424E-02*dtm*pa*pa;
    utci[, 157]=-4.32510997E-03*ta*dtm*pa*pa;
    utci[, 158]=8.99281156E-05*ta*ta*dtm*pa*pa;
    utci[, 159]=-7.14663943E-07*ta*ta*ta*dtm*pa*pa;
    utci[, 160]=-2.66016305E-04*va*dtm*pa*pa;
    utci[, 161]=2.63789586E-04*ta*va*dtm*pa*pa;
    utci[, 162]=-7.01199003E-06*ta*ta*va*dtm*pa*pa;
    utci[, 163]=-1.06823306E-04*va*va*dtm*pa*pa;
    utci[, 164]=3.61341136E-06*ta*va*va*dtm*pa*pa;
    utci[, 165]=2.29748967E-07*va*va*va*dtm*pa*pa;
    utci[, 166]=3.04788893E-04*dtm*dtm*pa*pa;
    utci[, 167]=-6.42070836E-05*ta*dtm*dtm*pa*pa;
    utci[, 168]=1.16257971E-06*ta*ta*dtm*dtm*pa*pa;
    utci[, 169]=7.68023384E-06*va*dtm*dtm*pa*pa;
    utci[, 170]=-5.47446896E-07*ta*va*dtm*dtm*pa*pa;
    utci[, 171]=-3.59937910E-08*va*va*dtm*dtm*pa*pa;
    utci[, 172]=-4.36497725E-06*dtm*dtm*dtm*pa*pa;
    utci[, 173]=1.68737969E-07*ta*dtm*dtm*dtm*pa*pa;
    utci[, 174]=2.67489271E-08*va*dtm*dtm*dtm*pa*pa;
    utci[, 175]=3.23926897E-09*dtm*dtm*dtm*dtm*pa*pa;
    utci[, 176]=-3.53874123E-02*pa*pa*pa;
    utci[, 177]=-2.21201190E-01*ta*pa*pa*pa;
    utci[, 178]=1.55126038E-02*ta*ta*pa*pa*pa;
    utci[, 179]=-2.63917279E-04*ta*ta*ta*pa*pa*pa;
    utci[, 180]=4.53433455E-02*va*pa*pa*pa;
    utci[, 181]=-4.32943862E-03*ta*va*pa*pa*pa;
    utci[, 182]=1.45389826E-04*ta*ta*va*pa*pa*pa;
    utci[, 183]=2.17508610E-04*va*va*pa*pa*pa;
    utci[, 184]=-6.66724702E-05*ta*va*va*pa*pa*pa;
    utci[, 185]=3.33217140E-05*va*va*va*pa*pa*pa;
    utci[, 186]=-2.26921615E-03*dtm*pa*pa*pa;
    utci[, 187]=3.80261982E-04*ta*dtm*pa*pa*pa;
    utci[, 188]=-5.45314314E-09*ta*ta*dtm*pa*pa*pa;
    utci[, 189]=-7.96355448E-04*va*dtm*pa*pa*pa;
    utci[, 190]=2.53458034E-05*ta*va*dtm*pa*pa*pa;
    utci[, 191]=-6.31223658E-06*va*va*dtm*pa*pa*pa;
    utci[, 192]=3.02122035E-04*dtm*dtm*pa*pa*pa;
    utci[, 193]=-4.77403547E-06*ta*dtm*dtm*pa*pa*pa;
    utci[, 194]=1.73825715E-06*va*dtm*dtm*pa*pa*pa;
    utci[, 195]=-4.09087898E-07*dtm*dtm*dtm*pa*pa*pa;
    utci[, 196]=6.14155345E-01*pa*pa*pa*pa;
    utci[, 197]=-6.16755931E-02*ta*pa*pa*pa*pa;
    utci[, 198]=1.33374846E-03*ta*ta*pa*pa*pa*pa;
    utci[, 199]=3.55375387E-03*va*pa*pa*pa*pa;
    utci[, 200]=-5.13027851E-04*ta*va*pa*pa*pa*pa;
    utci[, 201]=1.02449757E-04*va*va*pa*pa*pa*pa;
    utci[, 202]=-1.48526421E-03*dtm*pa*pa*pa*pa;
    utci[, 203]=-4.11469183E-05*ta*dtm*pa*pa*pa*pa;
    utci[, 204]=-6.80434415E-06*va*dtm*pa*pa*pa*pa;
    utci[, 205]=-9.77675906E-06*dtm*dtm*pa*pa*pa*pa;
    utci[, 206]=8.82773108E-02*pa*pa*pa*pa*pa;
    utci[, 207]=-3.01859306E-03*ta*pa*pa*pa*pa*pa;
    utci[, 208]=1.04452989E-03*va*pa*pa*pa*pa*pa;
    utci[, 209]=2.47090539E-04*dtm*pa*pa*pa*pa*pa;
    utci[, 210]=1.48348065E-03*pa*pa*pa*pa*pa*pa;            
    
    data = apply(utci, c(1), sum, na.rm=na.rm)
    names(data) = names(ta)
    return(average_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm=na.rm))
}
index_units[83] = C_index
index_titles[83] = "Universal thermal climate index"
index_names[83] = "utci"
attr(calculate_83, "data") <- c(TMEAN, DEWPOINT, WIND, "RADIATIONTEMPERATURE")

#' @title Mould index
#' @description Number of days with  a relative humidity over 90% in combination with TG above 10 Celsius 
## @importance Important application in tourism, energy and health
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Mould index
#' @export
#' @examples
#' data(data_all)
#' mi(taverage = data_all$tg, rh = data_all$humidity)
mi = calculate_84 = function(taverage, rh, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = taverage>10 & rh>90
  function_ = function(data){    
    return(sum(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[84] = C_days
index_titles[84] = "Mould index"
index_names[84] = "mi"
attr(calculate_84, "data") <- c(TMEAN, HUMIDITY)

#' @title Heat Index
#' @description Combines air temperature and relative humidity to determine the human-perceived equivalent temperature
#' @section Formula: \deqn{HI= -42,379+2,04901523*TG+10,14333127*rh-0,22475541*TG*rh-0.00683783*TG^2-0.05481717*rh^2+0.0122874*TG^2*rh+0.00085282*TG*rh^2-0.00000199*TG^2*rh^2}. Where TG is air temperature in ºF and rh is relative humidity in %
#' @references \url{http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml}
## @importance Important application in tourism, energy and health
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Heat Index
#' @export
#' @examples
#' data(data_all)
#' hi(taverage = data_all$tg, rh = data_all$humidity)
hi = calculate_85 = function(taverage, rh, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if( is.null(taverage) | is.null(rh) ) { return(NULL) }
  data = heat.index(t = taverage, rh = rh, temperature.metric = "celsius")
  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[85] = C_index
index_titles[85] = "Heat Index"
index_names[85] = "hi"
attr(calculate_85, "data") <- c(TMEAN, HUMIDITY)

#' @title Wind chill index
#' @description Wind chill index is the lowering of body temperature due to the passing-flow of lower-temperature air. It combines air temperature and wind speed.
#' @section Formula: \deqn{WCI = 13.12 + 0.6215 * TG - 11.37 * v ^ {+ 0.16} +  0.3965 * TG * v ^ {+ 0.16}} Where TG in celsius and v is wind speed in Km/h
#' @references Osczevski, Randall; Bluestein, Maurice (2005). The new wind chill equivalent temperature chart. Bulletin of the American Meteorological Society. 86 (10): 1453–1458
## @importance Important application in energy and tourism
#' 
#' @param taverage medium temperature
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return WCI
#' @export
#' @examples
#' data(data_all)
#' wci(taverage = data_all$tg, w = data_all$wind)
wci = calculate_86 = function(taverage, w, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  w = w*(3600/1000)
  data = 13.12 + 0.6215*taverage - 11.37*w + 0.3965*taverage*w
  data[is.na(taverage) | is.na(w)] = NA
  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[86] = C_index
index_titles[86] = "Wind chill index"
index_names[86] = "wci"
attr(calculate_86, "data") <- c(TMEAN, WIND)

#' @title Apparent temperature
#' @description Index of the percived temperature.
#' @section Formula: \deqn{AT = TG + 0.33e -0.70v -4.00}  TG = air temperature in Celsius ; v = wind speed in m/s; e= water vapour pressure in hPa 
#' 
## @importance Important application in tourism
#' 
#' @param taverage medium temperature
#' @param w average wind
#' @param vapor water vapour pressure
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return AT
#' @export
#' @examples
#' data(data_all)
#' at(taverage = data_all$tg, w = data_all$wind, vapor = data_all$VAPOUR)
at = calculate_87 = function(taverage, w, vapor, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  e = 6.015*exp(17.2*taverage/(237.7+taverage))*vapor/100
  data = taverage + 0.33 * e - 0.70 * w - 4.00
  data[is.na(taverage) | is.na(vapor) | is.na(w)] = NA
  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[87] = C_index
index_titles[87] = "Apparent temperature"
index_names[87] = "at"
attr(calculate_87, "data") <- c(TMEAN, WIND, VAPOUR)

####wind-based
#' @title Days wind gusts above 21 m/s
#' @description Number of days with wind gusts above 21 m/s
## @importance Important application in energy and tourism
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Gustmax
#' @export
#' @examples
#' data(data_all)
#' dfx21(data = data_all$windGUST)
dfx21 = calculate_88 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>21, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[88] = C_days
index_titles[88] = "Days wind gusts above 21 m/s"  
index_names[88] = "dfx21"
attr(calculate_88, "data") <- c(WINDGUST)

#' @title Daily maximum wind gust
#' @description Maximum value of daily maximum wind gust (m/s)
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in energy, agriculture and tourism
#' 
#' @param data maximum wind gust
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return FXx
#' @export
#' @examples
#' data(data_all)
#' fxx(data = data_all$windGUST)
fxx = calculate_89 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(max(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[89] = C_wind
index_titles[89] = "Daily maximum wind gust"
index_names[89] = "fxx"
attr(calculate_89, "data") <- c(WINDGUST)

#' @title Mean of daily mean wind strength
#' @description Mean of daily FG
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in energy, agriculture and tourism
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return FG
#' @export
#' @examples
#' data(data_all)
#' fg(data = data_all$wind)
fg = calculate_90 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[90] = C_wind
index_titles[90] = "Mean of daily mean wind strength"
index_names[90] = "fg"
attr(calculate_90, "data") <- c(WIND)

#' @title Calm days
#' @description Number of calm days (FG <=2 m/s)
#' @references ECA&D website: European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in energy and tourism
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FGcalm
#' @export
#' @examples
#' data(data_all)
#' fgcalm(data = data_all$wind)
fgcalm = calculate_91 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data<=2, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[91] = C_days
index_titles[91] = "Calm days"
index_names[91] = "fgcalm"
attr(calculate_91, "data") <- c(WIND)

#' @title Number of days with averaged wind above 10.8m/s
#' @description Number of days with FG >=6 Bft (10.8 m/s)
#' @references ECA&D website: European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in energy, agriculture and tourism.
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FG6Bft
#' @export
#' @examples
#' data(data_all)
#' fg6bft(data = data_all$wind)
fg6bft = calculate_92 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=10.8, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[92] = C_days
index_titles[92] = "Days daily averaged wind above 10.8m/s"
index_names[92] = "fg6bft"
attr(calculate_92, "data") <- c(WIND)

####aridity/continentality-indices
#' @title Reference evapotranspiration
#' @description If data available using Fao-56 Penman-Monteith
#' @references Chiew, F.H.S., Kamaladasa, N.N., Malano, H.M., McMahon, T.A., 1995. Penman–Monteith FAO-24 reference crop evapotranspiration and class-A pan data in Australia. Agric. Water Manage. 28, 9–21
## @importance Important application in agriculture
#' 
#' @param tmin tmin
#' @param tmax tmax
#' @param toa toa
#' @param w w
#' @param lat lat
#' @param tdew tdew
#' @param mde mde
#' @param radiation radiation
#' @param insolation insolation
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return Eto
#' @export
#' @examples
#' data(data_all)
#' eto(tmin = data_all$tn, tmax = data_all$tx, 
#'      toa = data_all$radiationtoa, w = data_all$wind,
#'      lat=data_all$lat, tdew = data_all$dewpoint, 
#'      mde=data_all$mde, radiation = data_all$radiation, 
#'      insolation=data_all$insolation, rh = data_all$humidity)
eto = calculate_93 = function(tmin, tmax, toa, w, lat, tdew, mde, radiation=NA, insolation=NA, rh=NA, data_names=NULL, time.scale=YEAR, na.rm = FALSE){

  if(is.null(w)){
    return(NULL)
  }

  data = calc_eto(tmin = tmin, tmax = tmax, radiation = radiation, insolation=insolation, toa = toa, w = w, lat=lat, tdew = tdew, mde=mde, rh = rh)

  function_ = function(data){
    return(sum(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[93] = C_index
index_titles[93] = "Reference evapotranspiration"
index_names[93] = "eto"
attr(calculate_93, "data") <- c(TMIN, TMAX, RADIATIONTOA, WIND, LAT, RADIATION, MDE, DEWPOINT, INSOLATION, HUMIDITY)

#' @title UNEP Aridity Index
#' @description P/Eto
#' @references Huiping Huang, Yuping Han, Mingming Cao, Jinxi Song, and Heng Xiao Spatial-Temporal Variation of Aridity Index of China during 1960–2013. Advances in Meteorology, vol. 2016, Article ID 1536135, 10 pages, 2016. \url{https://doi.org/10.1155/2016/1536135}
## @importance Important application in agriculture
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return iUNEP
#' @export
#' @examples
#' data(data_all)
#' uai(eto = data_all$evapotranspiration, pr = data_all$rr)
uai = calculate_94 = function(eto, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = pr/eto
  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[94] = C_index
index_titles[94] = "UNEP Aridity Index"
index_names[94] = "uai"
attr(calculate_94, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' @title Climatic moisture deficit
#' @description ETo - Effective precipitation
#' ETo - Effective Precipitation
#' @references Parks, S. A., Parisien, M. , Miller, C. , Holsinger, L. M. and Baggett, L. S. (2018), Fine-scale spatial climate variation and drought mediate the likelihood of reburning. Ecol Appl, 28: 573-586. doi:10.1002/eap.1671
## @importance Important application in agriculture
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return CMD
#' @export
#' @examples
#' data(data_all)
#' cmd(eto = data_all$evapotranspiration, pr = data_all$rr)
cmd = calculate_95 = function(eto, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = eto-pr
  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[95] = C_index
index_titles[95] = "Climatic moisture deficit"
index_names[95] = "cmd"
attr(calculate_95, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' @title De Martonne aridity index
#' @description De Martonne aridity index is the ratio between the annual amount of precipitation and anual mean of temperature plus 10 Celsius.
#' @references De Martonne E., 1926. Une nouvelle fonction climatologique: L’indice d’aridité. La Meteorologie, 449-458.
#' @section Formula: \deqn{MAI = \frac{P} {TG+10}} P = annual precipitation (mm); TG = mean annual air temperature (Celsius)
## @importance Important application in agriculture
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return Martonne Aridity Index
#' @export
#' @examples
#' data(data_all)
#' mai(pr = data_all$rr, taverage = data_all$tg)
mai = calculate_96 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, taverage){
    taverage = taverage[names(data)]
    data = sum(data, na.rm=na.rm)
    taverage = mean(taverage, na.rm=na.rm)
    return(data/(taverage+10))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}
index_units[96] = C_index
index_titles[96] = "De Martonne aridity index"
index_names[96] = "mai"
attr(calculate_96, "data") <- c(PRECIPITATION, TMEAN)

#' @title Emberger aridity index
#' @description Aridity index based on annual precipitation and temperature range
#' @section Formula: \deqn{EAI = \frac {100*P}{Thm^2 - Tcm^2}} P = annual precipitation; Thm = Average temperature of the hottest month in Kelvin; Tcm= Average temperature of the coldest month in Kelvin
#' @references Emberger L. 1930. La végétation de la région méditerranéenne: essai d'une classification des groupements végétaux Revue Générale de Botanique, 42 (641–662), pp. 705-721
## @importance Important application in agriculture
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return Emberger Aridity Index
#' @export
#' @examples
#' data(data_all)
#' eai(pr = data_all$rr, taverage = data_all$tg)
eai = calculate_97 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  taverage = taverage + 273.15
  function_ = function(data, pr){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    p = sum(pr[names(data)], na.rm=na.rm)
    return(100*p / (max(byMonths, na.rm=na.rm)^2 - min(byMonths, na.rm=na.rm)^2))
  }
  byYears = calcf_data(data=taverage, extract_names=years, operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}
index_units[97] = C_index
index_titles[97] = "Emberger aridity index"
index_names[97] = "eai"
attr(calculate_97, "data") <- c(PRECIPITATION, TMEAN)

#' @title Johansson Continentality Index
#' @description The Johansson Continentality Index is usually used for the climatic differentiation between continental and oceanic climates.
#' @section Formula: \deqn{JCI = \frac {1.7(Thm-Tcm)}{sinf}-20.4} Thm = Average temperature of the hottest month (Celsius); Tcm = Average temperature of the coldest month (Celsius); f = geographical latitude
#' @references Chronopoulou-Sereli A. 1996. Courses of Agricultural Meteorology.Publications Agricultural University of Athens: Athens, OH
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param value lat
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return JCI
#' @export
#' @examples
#' data(data_all)
#' jci(data = data_all$tg, value = data_all$lat)
jci = calculate_98 = function(data, data_names=NULL, value, na.rm = FALSE, ...){
  function_ = function(data, value){  
    data = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    return((1.7 * (max(data, na.rm=na.rm)-min(data, na.rm=na.rm)) / sin(pi*value/180)) - 20.4)
  }
  byYears = calcf_data(data=data, extract_names=years, operation=function_, data_names=data_names, value=value)
  return(byYears)
}
index_units[98] = C_index
index_titles[98] = "Johansson Continentality Index"
index_names[98] = "jci"
attr(calculate_98, "data") <- c(TMEAN, LAT)

#' @title Kerner Oceanity Index
#' @description KOI analysed the oceanity assuming that marine climates have colder spring months in comparison with the autum nmonth.
#' @section Formula: \deqn{KOI = \frac {100(TGo-TGa)}{Thm-Tcm}} TGo = Average temperature of October TGa = Average temperature of April Thm = Average temperature of the hottest month (Celsius); Tcm = Average temperature of the coldest month (Celsius)
#' @references Zambakas J. 1992.General Climatology. Department of Geology,National & Kapodistrian University of Athens, Athens. Gavilan RG. 2005. The use of climatic parameters and indices in vege-tation distribution. A case study in the Spanish System Central.Int. J.Biometeorol.50: 111–120.
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Kerner Oceanity Index
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @export
#' @examples
#' data(data_all)
#' koi(data = data_all$tg)
koi = calculate_99 = function(data, data_names=NULL, na.rm = FALSE, ...){  
  function_ = function(data){
    data = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    data = 100 * (data[grepl("Oct", names(data))]-data[grepl("Apr", names(data))]) / (max(data, na.rm=na.rm)-min(data, na.rm=na.rm))
    return(data)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[99] = C_index
index_titles[99] = "Kerner Oceanity Index"
index_names[99] = "koi"
attr(calculate_99, "data") <- c(TMEAN)

#' @title Pinna Combinative Index
#' @description Pinna combinative index is an aridity–humidity index
#' @section Formula: \deqn{PICI = \frac {1}{2} \left(\frac{P}{TG+10}+\frac{12Pdm}{TGdm+10}\right)} P = annual precipitation (mm); TG = annual mean temperature (Celsius); Pdm= precipitation of the driest month; TGdm= temperature of the driest month 
#' @references Zambakas J. 1992. General Climatology. Department of Geology, National & Kapodistrian University of Athens: Athens, Greece.
## @importance Important application in agriculture
#' 
#' @param pr precipitation 
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return Pinna Combinative index
#' @export
#' @examples
#' data(data_all)
#' pici(pr = data_all$rr, taverage = data_all$tg)
pici = calculate_100 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, taverage){
    taverage = taverage[names(data)]
    data.month = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    taverage.month = calcf_data(data=taverage, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    return(1/2* ((sum(data, na.rm=na.rm)/(mean(taverage, na.rm=na.rm)+10)) + (12*min(data.month, na.rm=na.rm) / (taverage.month[data.month==min(data.month, na.rm=na.rm)][1]+10))))
  }
  byYears = calcf_data(data=pr, extract_names=years, data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}
index_units[100] = C_index
index_titles[100] = "Pinna Combinative index"
index_names[100] = "pici"
attr(calculate_100, "data") <- c(PRECIPITATION, TMEAN)

#' @title Budyko Index
#' @description Budyko Index is based on characteristics of the surface heat and water balance.
#' @section Formula: \deqn{BI = 100\frac {Rn}{L*P}} Rn= annual net radiation, P = annual precipitation, L = latent heat of vaporization for water 
#' @references Budyko M.I. The Heat Balance of the Earth's Surface U.S. Department of Commerce, Washington D.C (1958) 259 pp., translated by N.A. Stepanova
## @importance Important application in agriculture
#' 
#' @param data net radiation, surface solar radiation downwards, J/m2
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ... 
#' @return Budyko Index
#' @export
#' @examples
#' data(data_all)
#' bi(data = data_all$radiation, pr = data_all$rr)
bi = calculate_101 = function(data, pr, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, pr){
    pr = pr[names(data)]
    r = 0.8 * mean(data, na.rm=na.rm)/1000
    pp = sum(pr, na.rm=na.rm)
    l = 2257
    # return(100*pp/(l*r))
    return(100*r/(pp*l))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}
index_units[101] = C_index
index_titles[101] = "Budyko Index"
index_names[101] = "bi"
attr(calculate_101, "data") <- c(RADIATION, PRECIPITATION)

#' @title Marsz Oceanity Index
#' @description MOI = ( 0.731 * geographic latitude grados + 1.767 ) / the annual range of monthly mean air temperatures grados
#' @section Formula: \deqn{MOI=\frac {0.731 \phi +1.767}{Thm-Tcm}} \deqn{\phi} = geographical latitude; Thm = Average temperature of the hottest month (Celsius); Tcm = Average temperature of the coldest month (Celsius)  
#' @references Marsz A, Rakusa-Suszczewskis S. 1987. Charakterystyka ekologiczna rejonu Zatoki Admiralicji (King George Island, SouthShetland Islands). 1. Klimat i obszary wolne od lodu.Kosmos36:103–127.
## @importance Important application in agriculture
#' 
#' @param data medium temperature
#' @param value lat 
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return n0to10
#' @export
#' @examples
#' data(data_all)
#' moi(data = data_all$tg, value = data_all$lat)
moi = calculate_102 = function(data, value, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, value){
    data = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    return(( 0.731 * value + 1.767 ) / (max(data, na.rm=na.rm)-min(data, na.rm=na.rm)) )
  }
  byYears = calcf_data(data=data, extract_names=years, operation=function_, data_names=data_names, value=value)
  return(byYears)
}
index_units[102] = C_index
index_titles[102] = "Marsz Oceanity Index"
index_names[102] = "moi"
attr(calculate_102, "data") <- c(TMEAN, LAT)

####snow-based
#' @title Snowfall sum
#' @description Sum of snowfall
## @importance Important application in water and tourism
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return n0to10
#' @export
#' @examples
#' data(data_all)
#' ss(data = data_all$snowfallmm)
ss = calculate_103 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=sum, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[103] = C_snow
index_titles[103] = "Snowfall sum"
index_names[103] = "ss"
attr(calculate_103, "data") <- c(SNOWFALLMM)

#' @title Snow depth 1-10
#' @description Number of days with snow depth in the range 1-10 cm
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SD0_10
#' @export
#' @examples
#' data(data_all)
#' sd0_10(data = data_all$snowdepththickness)
sd0_10 = calculate_104 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=1 & data<=10, na.rm=na.rm))
  }
  data = data/10
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[104] = C_days
index_titles[104] = "Snow depth 1-10"
index_names[104] = "sd0_10"
attr(calculate_104, "data") <- c(SNOWDEPTHTHICKNESS)

#' @title Snow depth 10-20
#' @description The number of days with snow depth of 10-20 cm
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SD10_20
#' @export
#' @examples
#' data(data_all)
#' sd10_20(data = data_all$snowdepththickness)
sd10_20 = calculate_105 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=10 & data<=20, na.rm=na.rm))
  }
  data = data/10
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[105] = C_days
index_titles[105] = "Snow depth 10-20"
index_names[105] = "sd10_20"
attr(calculate_105, "data") <- c(SNOWDEPTHTHICKNESS)

#' @title Snow depth
#' @description Mean of daily snow depth
#' mean of daily snow depth
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return snow depth
#' @export
#' @examples
#' data(data_all)
#' sdd(data = data_all$snowdepththickness)
sdd = calculate_106 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[106] = C_snow
index_titles[106] = "Snow depth"
index_names[106] = "sdd"
attr(calculate_106, "data") <- c(SNOWDEPTHTHICKNESS)

#' @title Number of snow days
#' @description Number of snow days
#' number of snow days
#' No. snow days
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in water and tourism
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return freq. of snow days
#' @export
#' @examples
#' data(data_all)
#' fsd(data = data_all$snowfall)
fsd = calculate_107 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>0, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[107] = C_days
index_titles[107] = "Frequency of snow days"
index_names[107] = "fsd"
attr(calculate_107, "data") <- c(SNOWFALL)

#' @title Mild snowy days
#' @description number of days with snow depth > 5 cm.
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return mild snowy days
#' @export
#' @examples
#' data(data_all)
#' msd(data = data_all$snowdepththickness)
msd = calculate_108 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>50/100, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[108] = C_days
index_titles[108] = "Mild snowy days"
index_names[108] = "msd"
attr(calculate_108, "data") <- c(SNOWDEPTHTHICKNESS)

#' @title Heavy snowy days
#' @description Number of days with snow depth more than 50 cm.
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return heavy snowy days
#' @export
#' @examples
#' data(data_all)
#' hsd(data = data_all$snowdepththickness)
hsd = calculate_109 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>500/100, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[109] = C_days
index_titles[109] = "Heavy snowy days"
index_names[109] = "hsd"
attr(calculate_109, "data") <- c(SNOWDEPTHTHICKNESS)

#' @title Date of first snow cover
#' @description First day when there is measurable snow cover (day of the hydrological year)
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return first snowcover
#' @export
#' @examples
#' data(data_all)
#' fsc(data = data_all$snowdepth)
fsc = calculate_110 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    if(na.rm | sum(is.na(data))==0){
      value = which(data>0)[1]
      if(is.na(value)){
        value = 0
      }
      return(value)
    }
    return(NA)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(HYDROYEAR), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[110] = C_date
index_titles[110] = "Date of first snow cover"
index_names[110] = "fsc"
attr(calculate_110, "data") <- c(SNOWDEPTH)

#' @title Date of first permanent snow cover
#' @description First day of the longest period with consecutive snow cover day (day of the hydrological year).
#' First day of the longest period with consecutive snow cover day
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return first permanent snowcover
#' @export
#' @examples
#' data(data_all)
#' fpsc(data = data_all$snowdepth)
fpsc = calculate_111 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = as.numeric(data>0)
    data.rle = rle(as.numeric(data))
    data.i = which(data.rle$lengths>1 & data.rle$values>0, arr.ind = TRUE, useNames = TRUE)
    if(na.rm | sum(is.na(data))==0){
      if(length(data.i)==0){
        return(0)
      }
      data.i = data.i[which(max(data.rle$length[data.i])==data.rle$length[data.i])[1]]
      if(is.na(data.i) | data.i==1){
        return(data.i)
      }else{
        return(sum(1, data.rle$lengths[1:(data.i-1)], na.rm = na.rm))
      }
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(HYDROYEAR), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[111] = C_date
index_titles[111] = "Date of first permanent snow cover"
index_names[111] = "fpsc"
attr(calculate_111, "data") <- c(SNOWDEPTH)

#' @title Date of last permanent snow cover
#' @description Last day of the longest period with consecutive snow cover day (day of the hydrological year).
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return last permanent snowcover
#' @export
#' @examples
#' data(data_all)
#' lpsc(data = data_all$snowdepth)
lpsc = calculate_112 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = as.numeric(data>0)
    data.rle = rle(as.numeric(data))
    data.i = which(data.rle$lengths>1 & data.rle$values>0, arr.ind = TRUE, useNames = TRUE)
    if(na.rm | sum(is.na(data))==0){
      if(length(data.i)==0){
        return(0)
      }
      data.i = data.i[which(max(data.rle$length[data.i])==data.rle$length[data.i])[1]]
      if(is.na(data.i) | data.i==1){
        return(data.i)
      }else{
        return(sum(1, data.rle$lengths[1:(data.i)], na.rm = na.rm)-1)
      }
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(HYDROYEAR), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[112] = C_date
index_titles[112] = "Date of last permanent snow cover"
index_names[112] = "lpsc"
attr(calculate_112, "data") <- c(SNOWDEPTH)

#' @title Average snow depth
#' @description Average snow depth
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return average snow depth
#' @export
#' @examples
#' data(data_all)
#' asd(data = data_all$snowdepththickness)
asd = calculate_113 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[113] = C_snow
index_titles[113] = "Average snow depth"
index_names[113] = "asd"
attr(calculate_113, "data") <- c(SNOWDEPTHTHICKNESS)

#' @title Number of snow covered days
#' @description Number of snow covered days (snow depth > 0)
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return snow covered days
#' @export
#' @examples
#' data(data_all)
#' scd(data = data_all$snowdepth)
scd = calculate_114 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){    
    return(sum(data>0, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[114] = C_days
index_titles[114] = "Amount of snow covered days"
index_names[114] = "scd"
attr(calculate_114, "data") <- c(SNOWDEPTH)

#' @title Maximum snow depth
#' @description Maximum snow depth (m)
## @importance Important application in water and tourism
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return maximum snow depth
#' @export
#' @examples
#' data(data_all)
#' ms(data = data_all$snowdepththickness)
ms = calculate_115 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=max, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[115] = C_snow
index_titles[115] = "Maximum snow depth"
index_names[115] = "ms"
attr(calculate_115, "data") <- c(SNOWDEPTHTHICKNESS)

####Cloud/radiation-based
#' @title Sum of sunshine duration
#' @description Sum of sunshine duration (hours)
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture and tourism
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SSD, h
#' @export
#' @examples
#' data(data_all)
#' ssd(data = data_all$insolation)
ssd = calculate_116 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=sum, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[116] = C_sunshine
index_titles[116] = "Sum of sunshine duration"
index_names[116] = "ssd"
attr(calculate_116, "data") <- c(INSOLATION)

#' @title Sunny days
#' @description Days with mean cloud cover less than 10%.
## @importance Important application in agriculture and tourism
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SND
#' @export
#' @examples
#' data(data_all)
#' snd(data = data_all$cloud)
snd = calculate_117 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){    
    return(sum(data<10, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[117] = C_days
index_titles[117] = "Sunny days"
index_names[117] = "snd"
attr(calculate_117, "data") <- c(CLOUD)

#' @title Foggy days
#' @description Number of days with fog.
#' @references Rastogi, B., A.P. Williams, D.T. Fischer, S.F. Iacobellis, K. McEachern, L. Carvalho, C. Jones, S.A. Baguskas, and C.J. Still, 2016: Spatial and Temporal Patterns of Cloud Cover and Fog Inundation in Coastal California: Ecological Implications. Earth Interact., 20, 1–19, \url{https://doi.org/10.1175/EI-D-15-0033.1}
## @importance Important application in agriculture and tourism
#' 
#' @param data cloud base below 100 meter
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return cloudy days
#' @export
#' @examples
#' data(data_all)
#' fod(data = data_all$cloud100)
fod = calculate_118 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){    
    return(sum(data>0, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[118] = C_days
index_titles[118] = "Foggy days"
index_names[118] = "cld"
attr(calculate_118, "data") <- c(CLOUD100)

#' @title Mean daily cloud cover
#' @description Mean daily cloud cover (%)
## @importance Important application in agriculture and tourism
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return mean CC
#' @export
#' @examples
#' data(data_all)
#' cc(data = data_all$cloud)
cc = calculate_119 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[119] = C_cloud
index_titles[119] = "Mean daily cloud cover"
index_names[119] = "cc"
attr(calculate_119, "data") <- c(CLOUD)

#' @title Sunshine duration percentage
#' @description Sunshine duration fraction with respect to day length (%) 
#' @section Formula: \deqn{SSP = \frac{SS} {SSmax} * 100} SS: sum of sunshine duration (h); SSmax: maximun daylight (h)
#' 
#' @references European Climate Assessment & Dataset. Indices dictionary. \url{https://www.ecad.eu//indicesextremes/indicesdictionary.php}
## @importance Important application in agriculture and tourism
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return ssp
#' @export
#' @examples
#' data(data_all)
#' ssp(data = data_all$insolation)
ssp = calculate_120 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){    
    return(100*mean(data, na.rm=na.rm)/24)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[120] = C_index
index_titles[120] = "Sunshine duration fraction"
index_names[120] = "ssp"
attr(calculate_120, "data") <- c(INSOLATION)

#' @title Atmospheric Clarity Index
#' @description Ratio between solar radiation at surface and solar radiation at TOA (alt top of the atmosphere)
## @importance Important application in agriculture and tourism
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return ACI
#' @export
#' @examples
#' data(data_all)
#' aci(data = data_all$radiation, toa = data_all$radiationtoa)
aci = calculate_121 = function(data, toa, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data, toa){
    toa = toa[names(data)]
    return(mean(data/toa, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names, toa=toa)
  return(byYears)
}
index_units[121] = C_index
index_titles[121] = "Atmospheric Clarity Index"
index_names[121] = "aci"
attr(calculate_121, "data") <- c(RADIATION, RADIATIONTOA)

####Drought indices
#' @title Standardized precipitation index 1
#' @description Standardized precipitation index calculated at 1-month time scale
#' @references McKee, T. B., Doesken, N. J. and Kleist, J.: The relationship of drought frequency and duration to time scales, Eighth Conf. Appl. Climatol., 179–184, 1993.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPI
#' @export
#' @examples
#' data(data_all)
#' spi1(data = data_all$rr)
spi1 = calculate_122 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=1, na.rm=na.rm))
}
index_units[122] = C_index
index_titles[122] = "Standardized precipitation index 1"
index_names[122] = "spi1"
attr(calculate_122, "data") <- c(PRECIPITATION)

#' @title Standardized precipitation index 3
#' @description Standardized precipitation index calculated at 3-month time scale
#' @references McKee, T. B., Doesken, N. J. and Kleist, J.: The relationship of drought frequency and duration to time scales, Eighth Conf. Appl. Climatol., 179–184, 1993.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPI
#' @export
#' @examples
#' data(data_all)
#' spi3(data = data_all$rr)
spi3 = calculate_123 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=3, na.rm=na.rm))
}
index_units[123] = C_index
index_titles[123] = "Standardized precipitation index 3"
index_names[123] = "spi3"
attr(calculate_123, "data") <- c(PRECIPITATION)

#' @title Standardized precipitation index 6
#' @description Standardized precipitation index calculated at 6-month time scale
#' @references McKee, T. B., Doesken, N. J. and Kleist, J.: The relationship of drought frequency and duration to time scales, Eighth Conf. Appl. Climatol., 179–184, 1993.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPI
#' @export
#' @examples
#' data(data_all)
#' spi6(data = data_all$rr)
spi6 = calculate_124 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=6, na.rm=na.rm))
}
index_units[124] = C_index
index_titles[124] = "Standardized precipitation index 6"
index_names[124] = "spi6"
attr(calculate_124, "data") <- c(PRECIPITATION)

#' @title Standardized precipitation index 12
#' @description Standardized precipitation index calculated at 12-month time scale
#' @references McKee, T. B., Doesken, N. J. and Kleist, J.: The relationship of drought frequency and duration to time scales, Eighth Conf. Appl. Climatol., 179–184, 1993.
## @importance Important application in agriculture and water
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPI
#' @export
#' @examples
#' data(data_all)
#' spi12(data = data_all$rr)
spi12 = calculate_125 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=12, na.rm=na.rm))
}
index_units[125] = C_index
index_titles[125] = "Standardized precipitation index 12"
index_names[125] = "spi12"
attr(calculate_125, "data") <- c(PRECIPITATION)

#' @title Standardised Precipitation-Evapotranspiration Index 1
#' @description Standardized precipitation-evapotranspiration index calculated at 1-month time scale
#' @references Vicente-Serrano, S. M., Beguería, S. and López-Moreno, J. I.: A multiscalar drought index sensitive to global warming: The standardized precipitation evapotranspiration index, J. Clim., 23(7), doi:10.1175/2009JCLI2909.1, 2010.
## @importance Important application in agriculture and water
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPEI
#' @export
#' @examples
#' data(data_all)
#' spei1(eto = data_all$evapotranspiration, pr = data_all$rr)
spei1 = calculate_126 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spei(eto, pr, data_names, scale=1, na.rm=na.rm))  
}
index_units[126] = C_index
index_titles[126] = "Standardised Precipitation-Evapotranspiration Index 1"
index_names[126] = "spei1"
attr(calculate_126, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' @title Standardised Precipitation-Evapotranspiration Index 3
#' @description Standardized precipitation-evapotranspiration index calculated at 3-month time scale
#' @references Vicente-Serrano, S. M., Beguería, S. and López-Moreno, J. I.: A multiscalar drought index sensitive to global warming: The standardized precipitation evapotranspiration index, J. Clim., 23(7), doi:10.1175/2009JCLI2909.1, 2010.
## @importance Important application in agriculture and water
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPEI
#' @export
#' @examples
#' data(data_all)
#' spei3(eto = data_all$evapotranspiration, pr = data_all$rr)
spei3 = calculate_127 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spei(eto, pr, data_names, scale=3, na.rm=na.rm))
}
index_units[127] = C_index
index_titles[127] = "Standardised Precipitation-Evapotranspiration Index 3"
index_names[127] = "spei3"
attr(calculate_127, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' @title Standardised Precipitation-Evapotranspiration Index 6
#' @description Standardized precipitation-evapotranspiration index calculated at 6-month time scale
#' @references Vicente-Serrano, S. M., Beguería, S. and López-Moreno, J. I.: A multiscalar drought index sensitive to global warming: The standardized precipitation evapotranspiration index, J. Clim., 23(7), doi:10.1175/2009JCLI2909.1, 2010.
## @importance Important application in agriculture and water
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPEI
#' @export
#' @examples
#' data(data_all)
#' spei6(eto = data_all$evapotranspiration, pr = data_all$rr)
spei6 = calculate_128 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
    return(calc_spei(eto, pr, data_names, scale=6, na.rm=na.rm))
}
index_units[128] = C_index
index_titles[128] = "Standardised Precipitation-Evapotranspiration Index 6"
index_names[128] = "spei6"
attr(calculate_128, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' @title Standardised Precipitation-Evapotranspiration Index 12
#' @description Standardized precipitation-evapotranspiration index calculated at 12-month time scale
#' @references Vicente-Serrano, S. M., Beguería, S. and López-Moreno, J. I.: A multiscalar drought index sensitive to global warming: The standardized precipitation evapotranspiration index, J. Clim., 23(7), doi:10.1175/2009JCLI2909.1, 2010.
## @importance Important application in agriculture
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @param ... ...
#' @return SPEI
#' @export
#' @examples
#' data(data_all)
#' spei12(eto = data_all$evapotranspiration, pr = data_all$rr)
spei12 = calculate_129 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
   return(calc_spei(eto, pr, data_names, scale=12, na.rm=na.rm))
}
index_units[129] = C_index
index_titles[129] = "Standardised Precipitation-Evapotranspiration Index 12"
index_names[129] = "spei12"
attr(calculate_129, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

####Fire-based
#' @title Canadian Fire Weather Index
#' @description The Canadian Forest Fire Weather Index is an indicator of fire weather intensity and is used to represent potential fire danger. It is computed from daily values of precipitation, temperature, near-surface wind and relative humidity
#' dimensionless, see Van Wagner (1987).
#' @references Van Wagner CE. 1987. Development and structure of the Canadian forest fire weather index system. Technical Report 35, Canadian Forestry Service: Ottawa, Ontario. Bedia, J., Herrera, S., Gutiérrez, J. M., Zavala, G., Urbieta, I. R., & Moreno, J. M. (2012). Sensitivity of fire weather index to different reanalysis products in the iberian peninsula. Natural Hazards and Earth System Science, 12(3), 699-708. doi:10.5194/nhess-12-699-2012
#' ## @importance Important application for fire prevention
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param w average wind
#' @param pr precipitation
#' @param dew_point dew_point
#' @param lat latitude
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FWI
#' @export
#' @examples
#' data(data_all)
#' fwi(taverage = data_all$tg, rh = data_all$humidity, w = data_all$wind, 
#'      pr = data_all$rr, dew_point=data_all$dewpoint, lat = data_all$lat)
fwi = calculate_130 = function(taverage, rh, w, pr, dew_point, lat, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if(is.null(taverage) | is.null(rh) | is.null(w) | is.null(pr) | is.null(dew_point) | is.null(lat)) { 
    return(NULL) 
  }

  # # fergus: Comparar las 2 funciones
  # dayLength = DayLengths(lat)
  # data = index_CFWI(Month=as.numeric(months(names(taverage))), Days=as.POSIXlt(chron(names(taverage)))$yday+1, Temp=taverage, Dew = dew_point, WS = w, Rain = pr, daylist = dayLength)
  # names(data) = names(taverage)

  # fireDanger
  # missing.values = is.na(taverage) | is.na(names(taverage)) | is.na(rh) | is.na(pr) | is.na(w)
  # data.nas = fwi1D(months=as.numeric(months(names(taverage[!missing.values]))), Tm=taverage[!missing.values], H=rh[!missing.values], r=pr[!missing.values], W=w[!missing.values], lat = lat)



# Ej. fwi1D(dates, Tm, H, r, W, lat = 46, what = "FWI", init.pars = c(85, 6, 15), spin.up = 0)
# dates=names(taverage); Tm=taverage; H=rh; r=pr=data_all$rr; W=w; lat = lat; what = "FWI"; init.pars = c(85, 6, 15); spin.up = 0
  data = fwi1D(dates=names(taverage), Tm=taverage, H=rh, r=pr, W=w, lat = lat, what = "FWI", init.pars = c(85, 6, 15), spin.up = 0)

  # data = taverage
  # data[!missing.values] = data.nas

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  return(byYears)
}
index_units[130] = C_index
index_titles[130] = "Canadian Fire Weather Index"
index_names[130] = "fwi"
attr(calculate_130, "data") <- c(TMEAN, HUMIDITY, WIND, PRECIPITATION, DEWPOINT, LAT)

#' @title Keetch-Byran Drought Index
#' @description The Keetch-Byram Drought Index (KBDI) is an indicator of drought conditions and is used to predict wildfire severity.
#' @references Keetch, J.J. and Byram, G.M. (1968). A drought index for forest fire control. Tech. Rep., USDA Forest Service Research Paper SE-38, North Carolina, USA. Alexander, M.E., 1990. Computer calculation of the Keetch-Byram Drought Index - programmers beware. Fire Management Notes 51, 23–25.
## @importance Important application for fire prevention
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return KBDI
#' @export
#' @examples
#' data(data_all)
#' kbdi(taverage = data_all$tg, pr=data_all$rr)
kbdi = calculate_131 = function(taverage, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if(is.null(taverage) | is.null(pr)) { 
    return(NULL) 
  }

# Q = Wc - W
# daily maximum temperature T
# mean annual rainfall R

  # # fergus: Comparar las 2 funciones
  # byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), operation=sum, na.rm = na.rm)
  # map = mean(byYears, na.rm=TRUE)
  # data.all = index_KBDI(Temperature=taverage, Rain=pr, MAP=map)
  # names(data.all) = names(taverage)

  # fireDanger
  missing.values = is.na(taverage) | is.na(names(taverage)) | is.na(pr)
  data.nas = kbdindex(dates=names(taverage)[!missing.values], t=taverage[!missing.values], p=pr[!missing.values], wrs = 5, start.date = NULL)
  data.all = taverage
  data.all[] = NA
  data.all[names(data.nas)] = data.nas

  byYears = calcf_data(data=data.all, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  return(byYears)
}
index_units[131] = C_index
index_titles[131] = "Keetch-Byran Drought Index"
index_names[131] = "kbdi"
attr(calculate_131, "data") <- c(TMEAN, PRECIPITATION)

#' @title McArthur Forest Fire Danger Index
#' @description The McArthur Forest Fire Danger Index (FFDI) is a good indication of the difficulty of fire suppression over a wide range of conditions. It estimates the amount of precipitation needed to bring the soil back to saturation and is computed from the Keetch-Byram Drought Index (KBDI) and Drought Factor (DF).
#' @references McArthur, A. G. (1967). Fire behaviour in eucalypt forests. Forestry and Timber Bureau Leaflet 107, 36 pp.
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param rh relative humidity
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FFDI
#' @export
#' @examples
#' data(data_all)
#' ffdi(taverage = data_all$tg, pr=data_all$rr, rh=data_all$humidity, w=data_all$wind)
ffdi = calculate_132 = function(taverage, pr, rh, w, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if(is.null(taverage) | is.null(pr) | is.null(rh) | is.null(w)) { 
    return(NULL) 
  }

  # byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), operation=sum, na.rm = na.rm)
  # map = mean(byYears, na.rm=TRUE)
  # kdbiData = index_KBDI(Temperature=taverage, Rain=pr, MAP=map)
  # data = index_MA(Temperature=taverage, Rain=pr, DewPoint=rh, MAP=map, Wind=w, KBDI=kdbiData)
  # names(data) = names(taverage)
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  
  data.all = taverage
  data.all[] = NA

  missing.values = is.na(taverage) | is.na(names(taverage)) | is.na(pr)
  data.nas = kbdindex(dates=names(taverage)[!missing.values], t=taverage[!missing.values], p=pr[!missing.values], wrs = 5, start.date = NULL)
  data.ffdi = ffdiIndex(madf = data.nas, t = taverage[names(data.nas)], h = rh[names(data.nas)], w = w[names(data.nas)])
  data.all[names(data.ffdi)] = data.ffdi

  byYears = calcf_data(data=data.all, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  return(byYears)
}
index_units[132] = C_index
index_titles[132] = "McArthur Forest Fire Danger Index"
index_names[132] = "ffdi"
attr(calculate_132, "data") <- c(TMEAN, PRECIPITATION, HUMIDITY, WIND)

#' @title Modified Nesterov Index
#' @description The Modified Nesterov Index (MNI) reflects the relationship between observed weather conditions and fire occurrence. It is a cumulative index  computed from daily temperature and dewpoint temperature, which is reset when a certain precipitation value is reached.
#' @references Groisman, P.Y., et al., 2007. Global and Planetary Change 56, 371–386.
## @importance Important application in fire prevention
#' 
#' @param dew_point dew point
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return MNI
#' @export
#' @examples
#' data(data_all)
#' mni(dew_point=data_all$dewpoint, taverage=data_all$tg, rh=data_all$humidity, pr=data_all$rr)
mni = calculate_133 = function(dew_point, taverage, rh, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if(is.null(dew_point) | is.null(taverage) | is.null(rh) | is.null(pr)) { return(NULL) 
  }

  # Rewriting the function index_MMI
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
  # data = index_MMI(DewPoint=dew_point, Temperature=taverage, Rain=pr)
  # names(data) = names(taverage)

  # fireDanger
  data = nesterovIndex(t = taverage, rh = rh, p = pr, modified = FALSE)
  names(data) = names(taverage)

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  return(byYears)
}
index_units[133] = C_index
index_titles[133] = "Modified Nesterov Index"
index_names[133] = "mni"
attr(calculate_133, "data") <- c(DEWPOINT, TMEAN, HUMIDITY, PRECIPITATION)

#' @title Finnish Forest Fire Index
#' @description Finnish forest fire index is determined from the surface moisture, by estimating the volumetric moisture of a 60 mm thick soil surface layer using potential evaporation and precipitation data.
#' @references Venäläinen A, Heikinheimo M. 2003. The Finnish forest fire index calculation system. In Early Warning Systems for Natural Disaster Reduction, Zschau J, Kuppers A (eds). Springer: Berlin; 645–648.. Vajda, A., Venalainen, A., Suomi, I., Junila, P. and Makela, H., 2014. Assessment of forest fire danger in a boreal forest environment: description and evaluation of the operational system applied in Finland. Meteorol. Appl., 21: 879-887, DOI: 10.1002/met.1425
## @importance Important application in fire prevention
#' 
#' @param data precipitation
#' @param evap potential evapotranspiration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FFFI
#' @export
#' @examples
#' data(data_all)
#' fffi(data = data_all$rr, evap=data_all$evapotranspiration)
fffi = calculate_134 = function(data, evap, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if(is.null(data) | is.null(evap)){
    return(NULL)
  }
  data = fffdi(pr=data, pet = evap, Wvol.init = 0.5, z = 60)
  data.all = evap
  data.all[] = NA
  data.all[names(data)] = data
  byYears = calcf_data(data = data.all, extract_names=select_time_function(time.scale), operation = mean, data_names = data_names, na.rm = na.rm)
  return(byYears)
}
index_units[134] = C_index
index_titles[134] = "Finnish Forest Fire Index"
index_names[134] = "fffi"
attr(calculate_134, "data") <- c(PRECIPITATION, EVAPOTRANSPIRATION)

####Tourism
#' @title Holliday Climate Index Urban
#' @description Holliday Climate Index for Urban destinations (Scott et all, 2016) (Tmax,wind,cloudiness,RH, precipitation) Scott, D., Rutty, M., Amelung, B. and Tang, M. (2016): An inter-comparison of the Holiday Climate Index (HCI) and the Tourism Climate Index (TCI), Atmosphere, 7, 80, doi:10.3390/atmos7060080
#' Holliday Climate Index for Urban destinations (Scott et all, 2016) (TX, wind, cloudiness, RH, precipitation) Scott, D., Rutty, M., Amelung, B. and Tang, M. (2016): An inter-comparison of the Holiday Climate Index (HCI) and the Tourism Climate Index (TCI), Atmosphere, 7, 80, doi:10.3390/atmos7060080
#' HCI : Urban= 4*TC +2*A+(3*precipitation+wind) where TC=thermal comfort (as a function of Tmax [C] and RH [%]), A (aesthetic facet)=cloudiness (%), precipitation [mm],wind speed (at 10m)[km/h]. HCI scores may be in the range 0 (potentially dangerous for tourists) to 100 (ideal for tourism).
#' @references Scott, D., Rutty, M., Amelung, B. and Tang, M. (2016): An inter-comparison of the Holiday Climate Index (HCI) and the Tourism Climate Index (TCI), Atmosphere, 7, 80, doi:10.3390/atmos7060080
## @importance Important application in tourism
#' 
#' @param pr precipitation
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return HCIU
#' @export
#' @examples
#' data(data_all)
#' hciu(pr = data_all$rr, w=data_all$wind)
hciu = calculate_135 = function(pr, w, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  #tc: thermal comfort 
  # data = 4*tc + 2*cloud_cover + 3*pr + w
  # function_ = function(data){
  #   return(mean(data, na.rm=na.rm))
  # }
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(NULL)
}
index_units[135] = C_index
index_titles[135] = "Holliday Climate Index Urban"
index_names[135] = "hciu"
attr(calculate_135, "data") <- c(PRECIPITATION, WIND)

#' @title Tourism Climatic Index
#' @description Standard index computed by ECA&D; Described at Miezkowski (1985), conceptual formula: TCI = 4cid + cia + 2R + 2S + W, where CId is a daytime comfort index, CIa a daily comfort index, R is cumulated rainfall, S the daily sunshine hours and W wind speed
#' Represents a quantitative evaluation of world climate for the purposes of tourism and is a composite measure of the climatic well-being of tourists.
#' TCI = 4cid + cia + 2R + 2S + W, where CId is a daytime comfort index, CIa a daily comfort index, R is cumulated rainfall, S the daily sunshine hours and W wind speed
#' @references Mieczkowski, Z. (1985). The tourism climatic index: a method of evaluating world climates for tourism. The Canadian Geographer/Le Géographe canadien, 29(3), 220-233.
## @importance Important application in tourism
#' 
#' @param data precipitation
#' @param sunshine net radiation 
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return TCI
#' @export
#' @examples
#' data(data_all)
#' tci(data=data_all$rr, sunshine=radiation.value, w=w.value)
tci = calculate_136 = function(data, sunshine, w, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  # #cia: Daily Comfort Index
  # #cid: Daytime Comfort Index 
  # data = 2 * ( 4*cid + cia + 2*pr + 2*sunshine + w )
  # function_ = function(data){
  #   return(mean(data, na.rm=na.rm))
  # }
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(NULL)
}
index_units[136] = C_index
index_titles[136] = "Tourism Climatic Index"
index_names[136] = "tci"
attr(calculate_136, "data") <- c(PRECIPITATION, RADIATION, WIND)

#' @title Good tourism days TCI>60
#' @description Number of days TCI>60, standard ECA&D
#' Number of days TCI>60 (see TCI)
#' TCI = 8 Cld + 2 Cla + 4 R + 4 S + 2 W. Let TCIij be the daily value of the Tourism Climatic Index at day i of period j. Then counted is the number of days where: TCIij>=60. Where C ld is a daytime comfort index, consisting of the mean maximum air temperature Ta, max (℃) and the mean minimum relative humidity RH (%), Cla is the daily comfort index, consisting of the mean air temperature (℃) and the mean relative humidity (%), R is the precipitation (mm), S is the daily sunshine duration (h), and W is the mean wind speed (m/s).
#' @references Mieczkowski, Z. (1985). The tourism climatic index: a method of evaluating world climates for tourism. The Canadian Geographer/Le Géographe canadien, 29(3), 220-233.
## @importance Important application in tourism
#' 
#' @param data precipitation
#' @param sunshine net radiation 
#' @param w average wind
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return TCI60
#' @export
#' @examples
#' data(data_all)
#' tci60(data=data_all$rr, sunshine=data_all$radiation, w=data_all$wind)
tci60 = calculate_137 = function(data, sunshine, w, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tci(data=data, sunshine=sunshine, w=w, data_names=data_names, time.scale=time.scale, na.rm = na.rm)
  function_ = function(data){
    return(sum(data>60, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[137] = C_days
index_titles[137] = "Good tourism days TCI>60"
index_names[137] = "tci60"
attr(calculate_137, "data") <- c(PRECIPITATION, RADIATION, WIND)

#' @title Excellent tourism days TCI>80
#' @description Number of days TCI>80, standard ECA&D
#' Number of days TCI>80 (see TCI)
#' TCI = 8 Cld + 2 Cla + 4 R + 4 S + 2 W. Let TCIij be the daily value of the Tourism Climatic Index at day i of period j. Then counted is the number of days where: TCIij>=80. Where C ld is a daytime comfort index, consisting of the mean maximum air temperature Ta, max (℃) and the mean minimum relative humidity RH (%), Cla is the daily comfort index, consisting of the mean air temperature (℃) and the mean relative humidity (%), R is the precipitation (mm), S is the daily sunshine duration (h), and W is the mean wind speed (m/s).
#' @references Mieczkowski, Z. (1985). The tourism climatic index: a method of evaluating world climates for tourism. The Canadian Geographer/Le Géographe canadien, 29(3), 220-233.
## @importance Important application in tourism
#' 
#' @param data precipitation
#' @param sunshine net radiation 
#' @param w average wind
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return TCI80
#' @export
#' @examples
#' data(data_all)
#' tci80(data=data_all$rr, sunshine=data_all$radiation, w=data_all$wind)
tci80 = calculate_138 = function(data, sunshine, w,  data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tci(data=data, sunshine=sunshine, w=w, data_names=data_names, time.scale=time.scale, na.rm = na.rm)
  function_ = function(data){
    return(sum(data>80, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[138] = C_days
index_titles[138] = "Excellent tourism days TCI>80"
index_names[138] = "tci80"
attr(calculate_138, "data") <- c(PRECIPITATION, RADIATION, WIND)

i = 1
for (i in 1:length(index_tipes)){
  index_tipes[[i]] = index_names[index_tipes[[i]]]
}
names(index_units) = names(index_titles) = names(index_names) = index_names

i = 1
for (i in 1:138){
  if(!is.na(index_names[i])){
    index_functions[[index_names[i]]] = get(paste0("calculate_", i))
  }
}

