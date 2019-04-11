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

# Datos diarios

C_degrees = "°C"
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

#' 1. GTX: Mean TX
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' gtx(data=data_all[[TMEAN]], time.scale=HYDROYEAR)
gtx = calculate_1 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[1] = C_degrees
index_titles[1] = "Mean TX"
index_names[1] = "gtx"
attr(calculate_1, "data") <- c(TMAX)

#' 2. XTX: Maximum TX
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' xtx(data=tmax.value)
xtx = calculate_2 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[2] = C_degrees
index_titles[2] = "Maximum TX"
index_names[2] = "xtx"
attr(calculate_2, "data") <- c(TMAX)

#' 3. NTX: Minimum TX
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' ntx(data=tmax.value)
ntx = calculate_3 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[3] = C_degrees
index_titles[3] = "Minimum TX"
index_names[3] = "ntx"
attr(calculate_3, "data") <- c(TMAX)

#' 4. GTN: Mean TN
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' gtn(data=tmin.value)
gtn = calculate_4 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[4] = C_degrees
index_titles[4] = "Mean TN"
index_names[4] = "gtn"
attr(calculate_4, "data") <- c(TMIN)

#' 5. XTN: Maximum TN
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' xtn(data=tmin.value)
xtn = calculate_5 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[5] = C_degrees
index_titles[5] = "Maximum TN"
index_names[5] = "xtn"
attr(calculate_5, "data") <- c(TMIN)

#' 6. NTN: Minimum TN
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' ntn(data=tmin.value)
ntn = calculate_6 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[6] = C_degrees
index_titles[6] = "Minimum TN"
index_names[6] = "ntn"
attr(calculate_6, "data") <- c(TMIN)

#' 7. GTG: Mean TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' gtg(data=taverage.value)
gtg = calculate_7 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[7] = C_degrees
index_titles[7] = "Mean TG"
index_names[7] = "gtg"
attr(calculate_7, "data") <- c(TMEAN)

#' 8. XTG: Maximum TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' xtg(data=taverage.value)
xtg = calculate_8 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[8] = C_degrees
index_titles[8] = "Maximum TG"
index_names[8] = "xtg"
attr(calculate_8, "data") <- c(TMEAN)

#' 9. NTG: Minimum TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return average temperature
#' @export
#' @examples
#' ntg(data=taverage.value)
ntg = calculate_9 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm = na.rm))
}
index_units[9] = C_degrees
index_titles[9] = "Minimum TG"
index_names[9] = "ntg"
attr(calculate_9, "data") <- c(TMEAN)

#' 10. CD: Cold days
#' Percentages of days with TX lower than the 10th percentile.
#' 
#' @param data maximum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Cold days
#' @export
#' @examples
#' cd(data=tmax.value, time.scale="season")
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
index_titles[10] = "Cold days"
index_names[10] = "cd"
attr(calculate_10, "data") <- c(TMAX)

#' 11. CN: Cold nights
#' Percentages of days with TN lower than the 10th percentile.
#' 
#' @param data minimum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Cold nights
#' @export
#' @examples
#' cn(data=tmin.value)
cn = calculate_11 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(calculate_10(data, data_names, time.scale, na.rm = na.rm))
}
index_units[11] = C_days
index_titles[11] = "Cold nights"
index_names[11] = "cn"
attr(calculate_11, "data") <- c(TMIN)

#' 12. CDD: Cold spell duration
#' Count of days with at least 6 consecutive days when TN < 10th percentile
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Cold spell duration index
#' @export
#' @examples
#' cdd(data=tmin.value, time.scale=SEASON)
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

#' 13. DTR: Diurnal temperature range
#' Mean difference between TX and TN.
#'
#' @param tmax maximum temperature 
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Diurnal temperature range
#' @export
#' @examples
#' dtr(tmax=tmax.value, tmin=tmin.value, time.scale=MONTH)
dtr = calculate_13 = function(tmax, tmin, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tmax - tmin
  byMonths = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=mean, na.rm = na.rm)
  return(byMonths)
}
index_units[13] = C_degrees
index_titles[13] = "Diurnal temperature range"
index_names[13] = "dtr"
attr(calculate_13, "data") <- c(TMAX, TMIN)

#' 14. vDTR: Mean daily difference DTR
#' Mean absolute day-to-day difference in DTR
#'
#' @param tmax maximum temperature 
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return vDTR
#' @export
#' @examples
#' vdtr(tmax=tmax.value, tmin=tmin.value, data_names=data_names, time.scale=YEAR, na.rm=FALSE)
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

#' 15. FD: Frost days
#' Number of days with minimum temperature <0ºC.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return frost days
#' @export
#' @examples
#' fd(data=tmin.value)
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

#' 16. GSL = Growing season length
#' Annual count of days between the ﬁrst span of at least 6 days with TG >5◦C and ﬁrst span after 1 July of 6 days with TG <5 ◦C.
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return growing season length
#' @export
#' @examples
#' gsl(data=taverage.value)
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
      data.min[1:length(data.min)<=aux] = FALSE #Menores que 5 después del 1 de julio
      data.min.rle = rle(as.numeric(data.min))      
      aux.min = which(data.min.rle$length>6 & data.min.rle$values==1, arr.ind = TRUE, useNames = TRUE)[1]-1

      if(is.na(aux.min) | length(aux.min)<=0 | length(data.min)<=0 | is.na(data.min[1])){
        data.min = length(data)
      }else{
        data.min = sum(data.min.rle$length[1:aux.min], arr.ind = TRUE, useNames = TRUE)
      }
      if(is.na(aux.max) | length(aux.max)<=0 | length(data.max)<=0 | is.na(data.max[1])){
        data.max = 1000
      }else{
        if(aux.max>0){
            data.max = sum(data.max.rle$length[1:aux.max], arr.ind = TRUE, useNames = TRUE) + 1
          }else{
            data.max = 1
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

#' 17. ID: Ice days
#' Number of days with maximum temperature <0ºC.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return ice days
#' @export
#' @examples
#' id(data=tmax.value)
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

#' 18. CFD: Maximum consecutive frost days
#' Maximum number of consecutive with days TN<0ºC
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return maximum consecutive frost
#' @export
#' @examples
#' cfd(data=tmin.value, time.scale=MONTH)
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

#' 19. ETR: Extreme temperature range
#' Difference between the highest TX and the lowest TN.
#' 
#' @param tmax maximum temperature
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return extreme temperature range
#' @export
#' @examples
#' etr(tmax=tmax.value, tmin=tmin.value)
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

#' 20. SUD: Summer days
#' Number of days with maximum temperature >25ºC.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Summer days
#' @export
#' @examples
#' sud(data=tmax.value)
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

#' 21. CSD: Maximum consecutive summer days
#' Maximum number of consecutive summer days (TX > 25º)
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return consecutive summer days
#' @export
#' @examples
#' csd(data=tmax.value)
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

#' 22. DD17: Difference days above/below Tx17
#' (days tx >17◦C)–(days TX <17◦C)
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Temperature sums
#' @export
#' @examples
#' dd17(data=tmax.value)
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

#' 23. TN: Tropical nights
#' Number of days with TN >20◦C.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Tropical nights
#' @export
#' @examples
#' tn(data=tmin.value)
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

#' 24. HD17: Heating degree days
#' (sum(17-TG)) only for days with TG<17ºC
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return HD17
#' @export
#' @examples
#' hd17(data=taverage.value)
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

#' 25. VCD: Very cold days
#' Number of days with minimum temperature <1st percentile.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Very cold days
#' @export
#' @examples
#' vcd(data=data_all[[TMIN]], time.scale=YEAR)
#' vcd(data=data_all[[TMIN]], time.scale=HYDROYEAR)
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

#' 26. VWD: Very warm days
#' Number of days with maximum temperature >99th percentile per year.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Very warm days
#' @export
#' @examples
#' vwd(data=tmax.value)
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

#' 27. WD: Warm days
#' Percentages of days with maximum temperatures higher than the 90th percentile.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Warm days
#' @export
#' @examples
#' wd(data=tmax.value)
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

#' 28. WN: Warm nights
#' Percentages of days with minimum temperatures higher than the 90th percentile.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Warm nights
#' @export
#' @examples
#' wn(data=tmin.value)
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

#' 29. WSD: Warm spell duration
#' Count of days with at least 6 consecutive days when TX > 90th percentile.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Warm spell duration index
#' @export
#' @examples
#' wsd(data=tmax.value)
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

#' 30. ZCD: Zero crossing days
#' Number of days with Tmax > 0 ºC and Tmin < 0 ºC.
#' 
#' @param tmax maximum temperature
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return zero crossing days
#' @export
#' @examples
#' zcd(tmax=tmax.value, tmin=tmin.value)
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

#' 31. OGS6: Onset of growing season 6 days
#' The start of the first span with at least 6 days with Tmean >5ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Onset of growing season 1
#' @export
#' @examples
#' ogs6(data=taverage.value)
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

#' 32. OGS10: Onset of growing season 10 days
#' The start of the first span with at least 10 days with Tmean >5ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Onset of growing season 2
#' @export
#' @examples
#' ogs10(data=taverage.value)
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

#' 33. Ta_o: Growing season (Apr-Oct)
#' Growing season (april to october) mean TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Growing season temperature 1
#' @export
#' @examples
#' ta_o(data=taverage.value)
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

#' 34. Tm_s: Growing season(May-Sep)
#' Growing season (may to september) mean TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Growing season temperature 2
#' @export
#' @examples
#' tm_s(data=taverage.value)
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

#' 35. GD4: Growing degree days
#' Sum of degree days over 4ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return GD4
#' @export
#' @examples
#' gd4(data=taverage.value)
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

#' 36. WKI: Winkler index
#' Sum of degree days over 10°C from April 1 until October 31 = Sum max [(avg. daily temp. – 10), 0]
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Winkler index
#' @export
#' @examples
#' wki(data = tmax.value)
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
attr(calculate_36, "data") <- c(TMAX)

#' 37. WS: Winter Severity
#' Mean temperature of the coldest month of the year
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Winter Severity index
#' @export
#' @examples
#' ws(data = tmax.value)
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
attr(calculate_37, "data") <- c(TMAX)

#' 38. STX32: Sums TX32
#' Sums of maximum temperatures >= 32ºC
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return temperature sums 1
#' @export
#' @examples
#' stx32(data = tmax.value)
stx32 = calculate_38 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data[data>=32], na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[38] = C_degrees
index_titles[38] = "Sums TX32"
index_names[38] = "stx32"
attr(calculate_38, "data") <- c(TMAX)

#' 39. D32: Days TX32
#' Number of days whith TX≥32˚C
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return temperature sums 1
#' @export
#' @examples
#' d32(data = tmax.value)
d32 = calculate_39 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=32, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[39] = C_days
index_titles[39] = "Days TX32"
index_names[39] = "d32"
attr(calculate_39, "data") <- c(TMAX)

#' 40. STN15: Sums TN-15
#' Sums of minimum air temperatures <= -15ºC recorded in December-February interval
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return temperature sums 2
#' @export
#' @examples
#' stn15(data = tmin.value)
stn15 = calculate_40 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sum(data[data <= -15], na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[40] = C_degrees
index_titles[40] = "Sums TN-15"
index_names[40] = "stn15"
attr(calculate_40, "data") <- c(TMIN)

#' 41. STN10: Sums TN-10
#' Sums of minimum air temperatures <=-10ºC recorded in December-February interval
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return temperature sums 3
#' @export
#' @examples
#' stn10(data = tmin.value)
stn10 = calculate_41 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sum(data[data <= -10], na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}
index_units[41] = C_degrees
index_titles[41] = "Sums TN-10"
index_names[41] = "stn10"
attr(calculate_41, "data") <- c(TMIN)

#' 42. PTG: Sums positive
#' Sums of positive TG  calculated for the 1st of February to the 10th April interval
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return temperature sums 5
#' @export
#' @examples
#' ptg(data = taverage.value)
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
#' 43. RT: Total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return total precipitation
#' @export
#' @examples
#' rt(data = pr.value)
rt = calculate_43 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[43] = C_precipitation
index_titles[43] = "Total precipitation"
index_names[43] = "rt"
attr(calculate_43, "data") <- c(PRECIPITATION)

#' 44. RX: Maximum precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return maximum precipitation
#' @export
#' @examples
#' rx(data = pr.value)
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

#' 45. R10mm: Days precipitation ≥R10mm 
#' Annual count of days when daily precipitation amount >= 10mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return R10mm
#' @export
#' @examples
#' r10mm(data = pr.value)
r10mm = calculate_45 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sum(data>=10, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, data_names=data_names, extract_names=select_time_function(time.scale), operation=function_)
  return(byYears)
}
index_units[45] = C_days
index_titles[45] = "Days precipitation ≥R10mm "
index_names[45] = "r10mm"
attr(calculate_45, "data") <- c(PRECIPITATION)

#' 46. R20mm: Days precipitation ≥R20mm 
#' Annual count of days when daily precipitation amount >= 20mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return R20mm
#' @export
#' @examples
#' r20mm(data = pr.value)
r20mm = calculate_46 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=20, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[46] = C_days
index_titles[46] = "Days precipitation ≥R20mm "
index_names[46] = "r20mm"
attr(calculate_46, "data") <- c(PRECIPITATION)

#' 49. Rx1d: Maximum daily R
#' Maximum 1-day precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Rx1day
#' @export
#' @examples
#' rx1d(data = pr.value)
rx1d = calculate_49 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data, value){
    return(max(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[49] = C_precipitation
index_titles[49] = "Maximum daily R"
index_names[49] = "rx1d"
attr(calculate_49, "data") <- c(PRECIPITATION)

#' 50. Rx5d: Maximum 5 days R
#' Maximum consecutive 5-day precipitation 
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Rx5day
#' @export
#' @examples
#' rx5d(data = pr.value)
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

#' 51. SDII: Simple precipitation intensity index
#' Sum of precipitation in wet days (days with >1mm of precipitation), and dividing that by the number of wet days in the period.
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return SDII
#' @export
#' @examples
#' sdii(data = pr.value, time.scale=MONTH)
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

#' 52. DD: Dry days
#' Number of days with less than 1 mm/day
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return dry days
#' @export
#' @examples
#' dd(data = pr.value)
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

#' 53. EP: Effective precipitation
#' Precipitation minus evapotranspiration
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return effective precipitation
#' @export
#' @examples
#' ep(pr = pr.value, eto = eto.value)
ep = calculate_53 = function(eto, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(pr, eto){
    return(sum(pr-eto[names(pr)], na.rm = na.rm))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, eto=eto)
  return(byYears)
}
index_units[53] = C_precipitation
index_titles[53] = "Precipitation minus evapotranspiration"
index_names[53] = "ep"
attr(calculate_53, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' 54. LDP: Longest dry period
#' Maximum length of consecutive dry days (RR<1)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return longest dry period
#' @export
#' @examples
#' ldp(data = pr.value)
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

#' 55. LWP: Longest wet period
#' Maximum length of consecutive wet days (RR>=1)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return longest wet period
#' @export
#' @examples
#' lwp(data = pr.value)
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

#' 56. R95tot: Precipitation fraction very wet days
#' Precipitation at days exceeding the 95percentile divided by total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return PVWD
#' @export
#' @examples
#' r95tot(data = pr.value)
r95tot = calculate_56 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.95))
  function_ = function(data, value){
    if(sum(is.na(data))>0){ return(NA) }
    value = select_value_for_data(data, value, time.scale)
    data = 100*sum(data[data>value], na.rm = na.rm)/sum(data, na.rm = na.rm)
    if(is.na(data)){ data = 100 }
    return(data)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}
index_units[56] = C_precipitation
index_titles[56] = "Precipitation fraction very wet days"
index_names[56] = "r95tot"
attr(calculate_56, "data") <- c(PRECIPITATION)

#' 57. R99tot: Precipitation fraction extremely wet days
#' Precipitation at days exceeding the 99percentile divided by total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return PEWD
#' @export
#' @examples
#' r99tot(data = pr.value)
r99tot = calculate_57 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  value = calcf_data(data=data, extract_names=select_all_time_function(time.scale), data_names=NULL, operation=quantile_null, probs=c(.99))
  function_ = function(data, value){
    if(sum(is.na(data))>0){ return(NA) }
    value = select_value_for_data(data, value, time.scale)
    return(100*sum(data[data>value], na.rm = na.rm)/sum(data, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  byYears[is.nan(byYears) | byYears==Inf | byYears==-Inf] = 100
  return(byYears)
}
index_units[57] = C_precipitation
index_titles[57] = "Precipitation fraction extremely wet days"
index_names[57] = "r99tot"
attr(calculate_57, "data") <- c(PRECIPITATION)

#' 58. D50mm: Heavy precipitation days
#' Number of days with precipitation above 50mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return heavy precipitation days
#' @export
#' @examples
#' d50mm(data = pr.value)
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

#' 59. D95p: Very wet days
#' Days when precipitation > 95p
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return R95p
#' @export
#' @examples
#' d95p(data = pr.value)
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

#' 60. PCI: Precipitation Concentration Index
#' PCI=100*sum(Pi^2)/P^2
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return PCI
#' @export
#' @examples
#' pci(data = pr.value)
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

#' 61. MFI: Modified Fournier Index
#' A precipitation concentration index
#' MFI=sum(Pi^2)/P
#' https://es.scribd.com/document/76414093/modified-fournier-index
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return MFI
#' @export
#' @examples
#' mfi(data = pr.value)
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

#' 62. GSR: Growing season precipitation
#' Growing season (april to october) total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return GSP
#' @export
#' @examples
#' gsr(data = pr.value)
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

#' 63. NGSR: Non-growing season precipitation
#' October to april total precipitation, can inform on the resource available for low potential evaporation conditions
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return non growing precipitation
#' @export
#' @examples
#' ngsr(data = pr.value)
ngsr = calculate_63 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(OCT, NOV, DEC, JAN, FEB, MAR, APR)]
    return(sum(data, na.rm = na.rm))
  }
  # Ej. 1956: Oct:Dec 1956 + Jan:April 1957
  i = grep("01/01/", names(data))[2] # Primer día del segundo año
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

#' 64. RTWD: Total precipitation wet days
#' Precipitation amount on days with RR >= 1 mm in a choosen period (e.g. year)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return precipitation in wet days
#' @export
#' @examples
#' rtwd(data = pr.value)
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

#' 65. DR1mm:  Wet days 1mm
#' Wet days >= 1 mm (days), ECA&D standard
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return RR1
#' @export
#' @examples
#' dr1mm(data = pr.value)
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

#' 66. DR3mm: Wt days 3mm
#' Wet days >= 3mm (days), ECA&D standard
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return RR3
#' @export
#' @examples
#' dr3mm(data = pr.value)
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
#' 67. BIO10: TG of warmest quarter
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO10
#' @export
#' @examples
#' bio10(data = taverage.value)
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

#' 68. BIO11: TG of coldest quarter
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO11
#' @export
#' @examples
#' bio11(data = taverage.value)
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

#' 69. BIO13: Precipitation of wettest month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO13
#' @export
#' @examples
#' bio13(data = pr.value)
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

#' 70. BIO14: Precipitation of driest month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO14
#' @export
#' @examples
#' bio14(data = pr.value)
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

#' 71. BIO15: Coefficient of variation precipitation
#' Precipitation seasonality (coefficient of variation)
#' This is a measure of the variation in monthly precipitation totals over the course of the year. This index is the ratio of the standard deviation of the monthly total precipitation to the mean monthly total precipitation (also known as the coefficient of variation) and is expressed as a percentage.
#'
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO15
#' @export
#' @examples
#' bio15(data = pr.value)
bio15 = calculate_71 = function(data, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data){
    byMonth = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sum, na.rm = na.rm)
    return(stats::sd(byMonth, na.rm = na.rm)/mean(byMonth, na.rm = na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[71] = C_index
index_titles[71] = "Coefficient of variation precipitation"
index_names[71] = "bio15"
attr(calculate_71, "data") <- c(PRECIPITATION)

#' 72. BIO16: Precipitation wettest quarter
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO16
#' @export
#' @examples
#' bio16(data = pr.value, na.rm = TRUE)
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

#' 73. BIO17: Precipitation driest quarter
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO17
#' @export
#' @examples
#' bio17(data = pr.value)
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

#' 74. BIO18: Precipitation warmest quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO18
#' @export
#' @examples
#' bio18(pr=pr.value, taverage=taverage.value)
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

#' 75. BIO19: Precipitation coldest quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO19
#' @export
#' @examples
#' bio19(pr=pr.value, taverage=taverage.value)
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

#' 76. BIO4: Temperature seasonality
#' Standard deviation temperature *100
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO4
#' @export
#' @examples
#' bio4(data = taverage.value)
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

#' 77. BIO5: TX warmest month
#' 
#' @param data mean temperature
#' @param tmax maximum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO5
#' @export
#' @examples
#' bio5(data = taverage.value, tmax = tmax.value)
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

#' 78. BIO6: TN of coldest month
#' 
#' @param data mean temperature
#' @param tmin minimum temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO6
#' @export
#' @examples
#' bio6(data = taverage.value, min = tmin.value)
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

#' 79. BIO7: Difference warmest/coldest month
#' Temperature Annual Range (BIO5-BIO6)
#' TX of warmest month minus TN of coldest month
#' 
#' @param data medium temperature
#' @param tmin min temperature
#' @param tmax max temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO7
#' @export
#' @examples
#' bio7(data = taverage.value, min = tmin.value, max = tmax.value)
bio7 = calculate_79 = function(data, tmin, tmax, data_names=NULL, na.rm = FALSE, ...){
  return(bio5(data, tmax, data_names=data_names, na.rm=na.rm)-bio6(data, tmin, data_names=data_names, na.rm=na.rm))
}
index_units[79] = C_index
index_titles[79] = "Difference warmest/coldest month"
index_names[79] = "bio7"
attr(calculate_79, "data") <- c(TMEAN, TMIN, TMAX)

#' 80. BIO8: TG of wettest quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO8
#' @export
#' @examples
#' bio8(pr = data_all[[PRECIPITATION]], taverage = data_all[[TMEAN]])
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

#' 81. BIO9: TG of driest quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature 
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO9
#' @export
#' @examples
#' bio9(pr = data_all[[PRECIPITATION]], taverage =  data_all[[TMEAN]])
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

#' 82. BIO20: Mean radiation 
#' https://www.edenextdata.com/?q=content/climond-bioclimatic-variables-2030 (W m-2)
#' 
#' @param data radiation en w/m2
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return BIO20
#' @export
#' @examples
#' bio20(data = data[[RADIATION_W]])
bio20 = calculate_82 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  return(average_temp(data=data, data_names=data_names, time.scale=time.scale, na.rm=na.rm))
}
index_units[82] = C_radiation
index_titles[82] = "Mean radiation"
index_names[82] = "bio20"
attr(calculate_82, "data") <- c(RADIATION_W)

#' 83. UTCI: Universal thermal climate index
#' UTCI (Blazejczyk et all, 2012) (Air temperature, Humidity, Wind)
#' https://goo.gl/by4hH9
#' http://www.utci.org/
#' https://rdrr.io/github/alfcrisci/rBiometeo/man/UTCI.html
#' Copy https://github.com/alfcrisci/rBiometeo
#' 
#'Given air temperature (Celsius), relative humidity (%), wind velocity (m/sec) and mean radiant temperature ( tmrt in Celsius degree) gives the Universal Thermal Climate Index in Celsius.
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
#' utci(ta = data[[TMEAN]], rh = data[[DEWPOINT]], wind = data[[WIND]], tmrt = data[["RADIATIONTEMPERATURE"]])
utci = calculate_83 = function(ta, rh, wind, tmrt, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
    if(is.null(ta) | is.null(rh) | is.null(wind)) {
      return(NULL) 
    }

    e = ta/10 # e = es(ta)/10; # use vapour pressure in kPa 
    pa = (e*rh/100.0)
    va = wind;
    va[va < 0.51] = 0.5
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

#' 84. MI: Mould index
#' Number of days with  a relative humidity over 90\% in combination with temperatures above 10°C 
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Mould index
#' @export
#' @examples
#' mi(taverage = data[[TMEAN]], rh = data[[HUMIDITY]])
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

#' 85. HI: Heat Index
#' Temperature + humidity 
#' http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
#' Available from the R package weathermetrics
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Heat Index
#' @export
#' @examples
#' hi(taverage = data_all[[TMEAN]], rh = data_all[[HUMIDITY]])
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

#' 86. WCI: Wind chill index
#' Temperature + wind 
#' Osczevski, R. & Bleustein, M. 2005, Bull. Amer. Meteor. Soc., 86, 1453, doi:10.1175/BAMS-86-10-1453 
#' https://journals.ametsoc.org/doi/abs/10.1175/BAMS-86-10-1453
#' Ta = air temperature; in ºC ; v = wind speed in m/s (original: km/h)
#' http://www.calculator.net/wind-chill-calculator.html
#' WCI = 13.12 + 0.6215*T - 11.37*V + 0.3965*T*V
#' 
#' @param taverage medium temperature
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return WCI
#' @export
#' @examples
#' wci(taverage = data_all[[TMEAN]], w = data_all[[WIND]])
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

#' 87. AT: Apparent Temperature
#' AT = Ta + 0.33e -0.70v -4.00; Ta = air temperature in ºC ; v = wind speed in m/s; e= water vapour pressure in hPa  
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
#' at(taverage = data_all[[TMEAN]], w = data_all[[WIND]], vapor = data_all[[VAPOUR]])
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
index_titles[87] = "Apparent Temperature"
index_names[87] = "at"
attr(calculate_87, "data") <- c(TMEAN, WIND, VAPOUR)

####wind-based
#' 88. DFx21: Days wind gusts above 21 m/s
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return Gustmax
#' @export
#' @examples
#' dfx21(data = data_all[[WINDGUST]])
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

#' 89. FXx: Daily maximum wind gust
#' Maximun value of daily maximum wind gust (m/s), ECA&D standard
#' 
#' @param data maximum wind gust
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return FXx
#' @export
#' @examples
#' fxx(data = data_all[[WINDGUST]])
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

#' 90. FG: Mean of daily mean wind strength
#' Mean of daily mean wind strength (m/s), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed? 
#' @return FG
#' @export
#' @examples
#' fg(data = data_all[[WIND]])
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

#' 91. FGcalm: Calm days
#' Calm days (average wind FG <= 2 m/s) (days), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FGcalm
#' @export
#' @examples
#' fgcalm(data = data_all[[WIND]])
fgcalm = calculate_91 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data<=2, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[91] = C_days
index_titles[91] = "Calm days2"
index_names[91] = "fgcalm"
attr(calculate_91, "data") <- c(WIND)

#' 92. FG6Bft: Days daily averaged wind above 10.8m/s
#' Days with daily averaged wind >= 6 Bft (10.8 m/s) (days), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FG6Bft
#' @export
#' @examples
#' fg6bft(data = data_all[[WIND]])
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
#' 93. Eto: Reference evapotranspiration
#' If data available using Fao-56 Penman-Monteith, if not using the Hargreaves & Samani method.
#' 
#' @param tmin tmin
#' @param tmax tmax
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
#' tmin = data_all[[TMIN]]; tmax = data_all[[TMAX]]; toa = data_all[[RADIATIONTOA]]; w = data_all[[WIND]]; lat=data_all[[LAT]]; tdew = data_all[[DEWPOINT]]; mde=data_all[[MDE]]; radiation = data_all[[RADIATION]]; insolation=data_all[[INSOLATION]]; rh = data_all[[HUMIDITY]]
#' calculate_93(tmin = data_all[[TMIN]], tmax = data_all[[TMAX]], toa = data_all[[RADIATIONTOA]], w = data_all[[WIND]], lat=data_all[[LAT]], tdew = data_all[[DEWPOINT]], mde=data_all[[MDE]], radiation = data_all[[RADIATION]], insolation=data_all[[INSOLATION]], rh = data_all[[HUMIDITY]])
eto = calculate_93 = function(tmin, tmax, toa, w, lat, tdew, mde, radiation=NA, insolation=NA, rh=NA, data_names=NULL, time.scale=YEAR, na.rm = FALSE){

  data = calc_eto(tmin = tmin, tmax = tmax, radiation = radiation, insolation=insolation, toa = toa, w = w, lat=lat, tdew = tdew, mde=mde, rh = rh)

  function_ = function(data){
    return(mean(data, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
index_units[93] = C_index
index_titles[93] = "Reference evapotranspiration"
index_names[93] = "eto"
attr(calculate_93, "data") <- c(TMIN, TMAX, RADIATIONTOA, WIND, LAT, RADIATION, MDE, DEWPOINT, INSOLATION, HUMIDITY)

#' 94. UAI: UNEP Aridity Index
#' P/Eto
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return iUNEP
#' @export
#' @examples
#' uai(eto = data[[EVAPOTRANSPIRATION]], pr = data[[PRECIPITATION]])
uai = calculate_94 = function(eto, pr, taverage, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
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

#' 95. CMD: Climatic moisture deficit
#' ETo - Effective precipitation
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return CMD
#' @export
#' @examples
#' cmd(eto = data_all[[EVAPOTRANSPIRATION]], pr = data_all[[PRECIPITATION]])
cmd = calculate_95 = function(eto, pr, taverage, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
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

#' 96. MAI: De Martonne aridity index
#' Annual rainfall/(Annual TG+10)
#' De Martonne = P / (T + 10); P is the annual total amount of precipitation (mm) and T is the mean annual air temperature (°C)
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return Martonne Aridity Index
#' @export
#' @examples
#' mai(pr = data_all[[PRECIPITATION]], taverage = data_all[[TMEAN]])
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

#' 97. EAI: Emberger aridity index
#' (100*annual rainfall)/(TGhottest month2-TG coldest month2)
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return Emberger Aridity Index
#' @export
#' @examples
#' eai(pr = data[[PRECIPITATION]], taverage = data[[TMEAN]])
eai = calculate_97 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
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

#' 98. JCI: Johansson Continentality Index
#' (1.7E/sinf)-20.4 where E (in8C) is the annual range of mean monthly air temperatures and f is the geographical latitude of the station
#' ( 1.7 * the annual range of monthly mean air temperatures grados / sin(geographic latitude grados) ) - 20.4
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param value lat
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return JCI
#' @export
#' @examples
#' jci(data = data_all[[TMEAN]], value = data_all[[LAT]])
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

#' 99. KOI: Kerner Oceanity Index
#' (100*(To-Ta))/E where To and Ta are the October and April mean values of TG respectively and E is the annual range of monthly mean air temperatures, in°C.
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Kerner Oceanity Index
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @export
#' @examples
#' koi(data = taverage.value)
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

#' 100. PiCI: Pinna Combinative index
#' 1/2((P/(T+10))+(12Pd/(Td+10))) where P and T are the annual mean values of precipitation and air temperature and P′d, T′dare the mean values of precipitation and air temperature of the driest month
#' 
#' @param pr precipitation 
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return Pinna Combinative index
#' @export
#' @examples
#' pici(pr = pr.value, taverage = taverage.value)
pici = calculate_100 = function(pr, taverage, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, taverage){
    taverage = taverage[names(data)]
    data.month = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    taverage.month = calcf_data(data=taverage, extract_names=select_time_function(MONTH), operation=mean, na.rm=na.rm)
    return(1/2* ((mean(data, na.rm=na.rm)/(mean(taverage, na.rm=na.rm)+10)) + (12*min(data.month, na.rm=na.rm) / (taverage.month[data.month==min(data.month, na.rm=na.rm)][1]+10))))
  }
  byYears = calcf_data(data=pr, extract_names=years, data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}
index_units[100] = C_index
index_titles[100] = "Pinna Combinative index"
index_names[100] = "pici"
attr(calculate_100, "data") <- c(PRECIPITATION, TMEAN)

#' 101. BI: Budyko Index
#' (Rn/L*P)*100, where Rn is the mean annual net radiation (also known as the net radiation balance), P is the mean annual precipitation, and L is the latent heat of vaporization for water
#' https://es.wikipedia.org/wiki/Clasificaci%C3%B3n_clim%C3%A1tica_de_Budyko
#' de vaporización: 2257 kJ/kg (539,4 cal/g) a 97 °C.
#' 
#' @param data net radiation 
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return Budyko Index
#' @export
#' @examples
#' bi(data = data_all[[RADIATION]], pr = data_all[[PRECIPITATION]])
bi = calculate_101 = function(data, pr, data_names=NULL, na.rm = FALSE, ...){
  function_ = function(data, pr){
    pr = pr[names(data)]
    r = 0.8 * mean(data, na.rm=na.rm)/1000
    pp = mean(pr, na.rm=na.rm)
    l = 2257
    # return(100*pp/(l*r))
    return(r/(pp*l))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}
index_units[101] = C_index
index_titles[101] = "Budyko Index"
index_names[101] = "bi"
attr(calculate_101, "data") <- c(RADIATION, PRECIPITATION)

#' 102. MOI: Marsz Oceanity Index
#'  MOI = ( 0.731 * geographic latitude grados + 1.767 ) / the annual range of monthly mean air temperatures grados
#' 
#' @param data medium temperature
#' @param value lat 
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return n0to10
#' @export
#' @examples
#' moi(data = taverage.value, value = lat)
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
#' 103. SS: Snowfall sum
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return n0to10
#' @export
#' @examples
#' ss(data = data_all[[SNOWFALLMM]])
ss = calculate_103 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=sum, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[103] = C_snow
index_titles[103] = "Snowfall sum"
index_names[103] = "ss"
attr(calculate_103, "data") <- c(SNOWFALLMM)

#' 104. SD0_10: Snow depth 1-10
#' The number of days with snow depth in the range 1-10 cm
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SD0_10
#' @export
#' @examples
#' sd0_10(data = data_all[[SNOWDEPTHTHICKNESS]])
sd0_10 = calculate_104 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>=1 & data<=10, na.rm=na.rm))
  }
  data = data/10
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[104] = C_days
index_titles[104] = "Snow depth 0-10"
index_names[104] = "sd0_10"
attr(calculate_104, "data") <- c(SNOWDEPTHTHICKNESS)

#' 105. SD10_20: Snow depth 10-20
#' The number of days with snow depth of 10-20 cm
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SD10_20
#' @export
#' @examples
#' sd10_20(data = data_all[[SNOWDEPTHTHICKNESS]])
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

#' 106. SnD: Snow depth
#' Mean of daily snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return snow depth
#' @export
#' @examples
#' snd(data = data_all[[SNOWDEPTHTHICKNESS]])
snd = calculate_106 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[106] = C_snow
index_titles[106] = "Snow depth"
index_names[106] = "snd"
attr(calculate_106, "data") <- c(SNOWDEPTHTHICKNESS)

#' 107. FSD: Frequency of snow days
#' number of snow days
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return freq. of snow days
#' @export
#' @examples
#' fsd(data = data_all[[SNOWFALL]])
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

#' 108. MSD: Mild snowy days
#' Annual number of days with snow depth more than 5 cm.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return mild snowy days
#' @export
#' @examples
#' msd(data = data_all[[SNOWDEPTHTHICKNESS]])
msd = calculate_108 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>5/100, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[108] = C_days
index_titles[108] = "Mild snowy days"
index_names[108] = "msd"
attr(calculate_108, "data") <- c(SNOWDEPTHTHICKNESS)

#' 109. HSD: Heavy snowy days
#' Annual number of days with snow depth more than 50 cm.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return heavy snowy days
#' @export
#' @examples
#' hsd(data = data_all[[SNOWDEPTHTHICKNESS]])
hsd = calculate_109 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){
    return(sum(data>50/100, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[109] = C_days
index_titles[109] = "Heavy snowy days"
index_names[109] = "hsd"
attr(calculate_109, "data") <- c(SNOWDEPTHTHICKNESS)

#' 110. FSC: Date of first snow cover
#' First day when there is measurable snow cover
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return first snowcover
#' @export
#' @examples
#' fsc(data = data_all[[SNOWDEPTH]])
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

#' 111. FPSC: Date of first permanent snow cover 
#' First day of the longest period with consecutive snow cover day.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return first permanent snowcover
#' @export
#' @examples
#' fpsc(data = data_all[[SNOWDEPTH]])
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
index_titles[111] = "Date of first permanent snow cover "
index_names[111] = "fpsc"
attr(calculate_111, "data") <- c(SNOWDEPTH)

#' 112. LPSC: Date of last permanent snow cover
#' Last day of the longest period with consecutive snow cover day.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return last permanent snowcover
#' @export
#' @examples
#' lpsc(data = data_all[[SNOWDEPTH]])
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

#' 113. ASD: Average snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return average snow depth
#' @export
#' @examples
#' asd(data = data_all[[SNOWDEPTHTHICKNESS]])
asd = calculate_113 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[113] = C_snow
index_titles[113] = "Average snow depth"
index_names[113] = "asd"
attr(calculate_113, "data") <- c(SNOWDEPTHTHICKNESS)

#' 114. SCD: Amount of snow covered days
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return snow covered days
#' @export
#' @examples
#' scd(data = data_all[[SNOWDEPTH]])
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

#' 115. MS: Maximum snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return maximum snow depth
#' @export
#' @examples
#' ms(data = data_all[[SNOWDEPTHTHICKNESS]])
ms = calculate_115 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=max, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[115] = C_snow
index_titles[115] = "Maximum snow depth"
index_names[115] = "ms"
attr(calculate_115, "data") <- c(SNOWDEPTHTHICKNESS)

####Cloud/radiation-based
#' 116. SSD: Sum of sunshine duration
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SSD, h
#' @export
#' @examples
#' ssd(data = data_all[[INSOLATION]])
ssd = calculate_116 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=sum, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[116] = C_sunshine
index_titles[116] = "Sum of sunshine duration"
index_names[116] = "ssd"
attr(calculate_116, "data") <- c(INSOLATION)

#' 117. SND: Sunny days
#' Days with mean cloud cover less than 10\%.
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SND
#' @export
#' @examples
#' snd(data = data_all[[CLOUD]])
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

#' 118. ClD: Cloudy days
#' Number of days with cloud base below 100 meter.
#' 
#' @param data cloud base below 100 meter
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return cloudy days
#' @export
#' @examples
#' cld(data = data_all[[CLOUD100]])
cld = calculate_118 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data){    
    return(sum(data>0, na.rm=na.rm))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}
index_units[118] = C_days
index_titles[118] = "Cloudy days"
index_names[118] = "cld"
attr(calculate_118, "data") <- c(CLOUD100)

#' 119. CC: Mean daily cloud cover
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return mean CC
#' @export
#' @examples
#' cc(data = data_all[[CLOUD]])
cc = calculate_119 = function(data, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm=na.rm)
  return(byYears)
}
index_units[119] = C_cloud
index_titles[119] = "Mean daily cloud cover"
index_names[119] = "cc"
attr(calculate_119, "data") <- c(CLOUD)

#' 120. SSp: Sunshine duration fraction
#' Sunshine duration fraction with respect to day length (%), standard ECA&D: 100*(SS/SSmax) SS: sum of sunshine duration, SSmax: maximun daylight hours 
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SSp
#' @export
#' @examples
#' ssp(data = data_all[[INSOLATION]])
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

#' 121. ACI: Atmospheric Clarity Index
#' Ratio between solar radiation at surface and solar radiation at TOA (alt top of the atmosphere empirically obtained)
#' https://goo.gl/Wzs1Zk
#' http://www.greenrhinoenergy.com/solar/radiation/atmosphere.php
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return ACI
#' @export
#' @examples
#' aci(data = data_all[[RADIATION]], toa = data_all[[RADIATIONTOA]])
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
#' 122. SPI1: Standardized precipitation index 1
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPI
#' @export
#' @examples
#' spi1(data = data_all[[PRECIPITATION]])
spi1 = calculate_122 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=1, na.rm=na.rm))
}
index_units[122] = C_index
index_titles[122] = "Standardized precipitation index 1"
index_names[122] = "spi1"
attr(calculate_122, "data") <- c(PRECIPITATION)

#' 123. SPI3: Standardized precipitation index 3
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPI
#' @export
#' @examples
#' spi3(data = data_all[[PRECIPITATION]])
spi3 = calculate_123 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=3, na.rm=na.rm))
}
index_units[123] = C_index
index_titles[123] = "Standardized precipitation index 3"
index_names[123] = "spi3"
attr(calculate_123, "data") <- c(PRECIPITATION)

#' 124. SPI6: Standardized precipitation index 6
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPI
#' @export
#' @examples
#' spi6(data = data_all[[PRECIPITATION]])
spi6 = calculate_124 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=6, na.rm=na.rm))
}
index_units[124] = C_index
index_titles[124] = "Standardized precipitation index 6"
index_names[124] = "spi6"
attr(calculate_124, "data") <- c(PRECIPITATION)

#' 125. SPI12: Standardized precipitation index 12
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPI
#' @export
#' @examples
#' spi12(data = data_all[[PRECIPITATION]])
spi12 = calculate_125 = function(data, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spi(data, data_names, scale=12, na.rm=na.rm))
}
index_units[125] = C_index
index_titles[125] = "Standardized precipitation index 12"
index_names[125] = "spi12"
attr(calculate_125, "data") <- c(PRECIPITATION)

#' 126. SPEI1: Standardised Precipitation-Evapotranspiration Index 1
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPEI
#' @export
#' @examples
#' spei1(eto = data_all[[EVAPOTRANSPIRATION]], pr = data_all[[PRECIPITATION]])
spei1 = calculate_126 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spei(eto, pr, data_names, scale=1, na.rm=na.rm))  
}
index_units[126] = C_index
index_titles[126] = "Standardised Precipitation-Evapotranspiration Index 1"
index_names[126] = "spei1"
attr(calculate_126, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' 127. SPEI3: Standardised Precipitation-Evapotranspiration Index 3
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPEI
#' @export
#' @examples
#' spei3(eto = data_all[[EVAPOTRANSPIRATION]], pr = data_all[[PRECIPITATION]])
spei3 = calculate_127 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
  return(calc_spei(eto, pr, data_names, scale=3, na.rm=na.rm))
}
index_units[127] = C_index
index_titles[127] = "Standardised Precipitation-Evapotranspiration Index 3"
index_names[127] = "spei3"
attr(calculate_127, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' 128. SPEI6: Standardised Precipitation-Evapotranspiration Index 6
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPEI
#' @export
#' @examples
#' spei6(eto = data_all[[EVAPOTRANSPIRATION]], pr = data_all[[PRECIPITATION]])
spei6 = calculate_128 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
    return(calc_spei(eto, pr, data_names, scale=6, na.rm=na.rm))
}
index_units[128] = C_index
index_titles[128] = "Standardised Precipitation-Evapotranspiration Index 6"
index_names[128] = "spei6"
attr(calculate_128, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

#' 129. SPEI12: Standardised Precipitation-Evapotranspiration Index 12
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return SPEI
#' @export
#' @examples
#' spei12(eto = data_all[[EVAPOTRANSPIRATION]], pr = data_all[[PRECIPITATION]])
spei12 = calculate_129 = function(eto, pr, data_names=NULL, na.rm = FALSE, ...){
   return(calc_spei(eto, pr, data_names, scale=12, na.rm=na.rm))
}
index_units[129] = C_index
index_titles[129] = "Standardised Precipitation-Evapotranspiration Index 12"
index_names[129] = "spei12"
attr(calculate_129, "data") <- c(EVAPOTRANSPIRATION, PRECIPITATION)

####Fire-based
#' 130. FWI: Canadian Fire Weather Index
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R
#' https://github.com/SantanderMetGroup/fireDanger
#' Combination of daily values of temperature, relative humidity, surface wind and precipitation
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
#' fwi(taverage=data_all[[TMEAN]], rh = data_all[[HUMIDITY]], w = data_all[[WIND]], pr = data_all[[PRECIPITATION]], dew_point=data_all[[DEWPOINT]], lat = data_all[[LAT]])
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
# dates=names(taverage); Tm=taverage; H=rh; r=pr; W=w; lat = lat; what = "FWI"; init.pars = c(85, 6, 15); spin.up = 0
fwi1D(dates=names(taverage), Tm=taverage, H=rh, r=pr, W=w, lat = lat, what = "FWI", init.pars = c(85, 6, 15), spin.up = 0)



  data = taverage
  data[!missing.values] = data.nas

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  return(byYears)
}
index_units[130] = C_index
index_titles[130] = "Canadian Fire Weather Index"
index_names[130] = "fwi"
attr(calculate_130, "data") <- c(TMEAN, HUMIDITY, WIND, PRECIPITATION, DEWPOINT, LAT)

#' 131. KBDI: Keetch-Byran Drought Index
#' Combination of daily maximum in temperature and precipitation
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R#Keech-Byran_Drought_Index
#' https://github.com/SantanderMetGroup/fireDanger/blob/devel/R/kbdi.R
#' https://www.srs.fs.usda.gov/pubs/rp/rp_se038.pdf
#'
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return KBDI
#' @export
#' @examples
#' kbdi(taverage = data_all[[TMEAN]], pr=data_all[[PRECIPITATION]])
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

  # # fireDanger

  # data.nas = kbdindex(dates=names(taverage), t=taverage, p=pr, wrs = 5, start.date = NULL)


  # missing.values = is.na(taverage) | is.na(names(taverage)) | is.na(pr)
  # data.nas = kbdindex(date=chron(names(taverage[!missing.values])), t=taverage[!missing.values], p=pr[!missing.values])
  # data.all = taverage
  # data.all[] = NA
  # data.all[!missing.values] = data.nas[!missing.values] #fergus: corregir [!missing.values]

  # byYears = calcf_data(data=data.all, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  byYears = NULL
  return(byYears)
}
index_units[131] = C_index
index_titles[131] = "Keetch-Byran Drought Index"
index_names[131] = "kbdi"
attr(calculate_131, "data") <- c(TMEAN, PRECIPITATION)

#' 132. FFDI: McArthur Forest Fire Danger Index
#' Combination of temperature, relative humidity, surface wind speed and KBDI
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R#Keech-Byran_Drought_Index
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
#' ffdi(taverage = data_all[[TMEAN]], pr=data_all[[PRECIPITATION]], rh=data_all[[HUMIDITY]], w=data_all[[WIND]])
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
  byYears = NULL
  return(byYears)
}
index_units[132] = C_index
index_titles[132] = "McArthur Forest Fire Danger Index"
index_names[132] = "ffdi"
attr(calculate_132, "data") <- c(TMEAN, PRECIPITATION, HUMIDITY, WIND)

#' 133. MNI: Modified Nesterov Index
#' Cummulative function of temperature and dew point deficit
#' http://www.atriplex.info/index.php/Fire_Danger_Index_Functions_in_R
#' https://github.com/jbedia/fireDanger/wiki/nesterovIndex
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
#' mni(dew_point=data_all[[DEWPOINT]], taverage=data_all[[TMEAN]], rh=data_all[[HUMIDITY]], pr=data_all[[PRECIPITATION]])
mni = calculate_133 = function(dew_point, taverage, rh, pr, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  if(is.null(dew_point) | is.null(taverage) | is.null(rh) | is.null(pr)) { return(NULL) 
  }

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
  names(data) = names(taverage)

  # fireDanger
  data = nesterovIndex(t=taverage, rh=rh, p=pr)
  names(data) = names(taverage)

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=mean, data_names=data_names, na.rm = na.rm)
  return(byYears)
}
index_units[133] = C_index
index_titles[133] = "Modified Nesterov Index"
index_names[133] = "mni"
attr(calculate_133, "data") <- c(DEWPOINT, TMEAN, HUMIDITY, PRECIPITATION)

#' 134. FFFI: Finnish Forest Fire Index
#' Combination of temperature, relative humidity, wind speed, radiation and precipitation (package fireDanger)
#' https://link.springer.com/chapter/10.1007%2F978-3-642-55903-7_88
#' P-eto+-f1
#' https://github.com/SantanderMetGroup/fireDanger
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return FFFI
#' @export
#' @examples
#' fffi(data = data_all[[RADIATION]], toa=data_all[[RADIATIONTOA]])
fffi = calculate_134 = function(data, toa, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  function_ = function(data, toa){
    toa  = toa[names(data)]
    return(meanf(data/toa))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, toa=toa, data_names=data_names)
  return(byYears)
}
index_units[134] = C_index
index_titles[134] = "Finnish Forest Fire Index"
index_names[134] = "fffi"
attr(calculate_134, "data") <- c(RADIATION, RADIATIONTOA)

####Tourism
#' 135. HCIU: Holliday Climate Index Urban
#' Holliday Climate Index for Urban destinations (Scott et all, 2016) (Tmax,wind,cloudiness,RH, precipitation) Scott, D., Rutty, M., Amelung, B. and Tang, M. (2016): An inter-comparison of the Holiday Climate Index (HCI) and the Tourism Climate Index (TCI), Atmosphere, 7, 80, doi:10.3390/atmos7060080
#' 
#' @param pr precipitation
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return HCIU
#' @export
#' @examples
#' hciu(pr = data_all[[PRECIPITATION]], w=data_all[[WIND]])
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

#' 136. TCI: Tourism Climatic Index
#' Standard index computed by ECA&D; Described at Miezkowski (1985), conceptual formula: TCI = 4cid + cia + 2R + 2S + W, where CId is a daytime comfort index, CIa a daily comfort index, R is cumulated rainfall, S the daily sunshine hours and W wind speed
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
#' tci(data=pr.value, sunshine=radiation.value, w=w.value)
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

#' 137. TCI60: Good tourism days TCI>60
#' Number of days TCI>60 , standard ECA&D
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
#' tci60(data = tci.value)
tci60 = calculate_137 = function(data, sunshine, w, data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tci(data=pr.value, sunshine=radiation.value, w=w.value, data_names=data_names, time.scale=time.scale, na.rm = na.rm)
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

#' 138. TCI80: Excellent tourism days TCI>80
#' Number of days TCI>80, standard ECA&D
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
#' tci80(data = tci.value)
tci80 = calculate_138 = function(data, sunshine, w,  data_names=NULL, time.scale=YEAR, na.rm = FALSE){
  data = tci(data=pr.value, sunshine=radiation.value, w=w.value, data_names=data_names, time.scale=time.scale, na.rm = na.rm)
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
