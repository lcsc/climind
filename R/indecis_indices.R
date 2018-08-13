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


# Abreviaturas
# daily mean temperature TG, ℃
# daily minimum temperature TN, ℃
# daily maximum temperature TX, ℃
# daily precipitation sum RR, mm
# lat, degree

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

# Datos diarios

#' 1. GTX: Mean TX
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' mean_tx(data=tmax.value)
mean_tx = calculate_1 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 2. XTX: Maximum TX
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' maximum_tx(data=tmax.value)
maximum_tx = calculate_2 = function(data, data_names=NULL, time.scale=YEAR){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 3. NTX: Minimum TX
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' minimum_tx(data=tmin.value)
minimum_tx = calculate_3 = function(data, data_names=NULL, time.scale=YEAR){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 4. GTN: Mean TN
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' mean_tn(data=tmin.value)
mean_tn = calculate_4 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data, data_names=data_names, time.scale=time.scale))
}

#' 5. XTN: Maximum TN
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' maximum_tn(data=tmin.value)
maximum_tn = calculate_5 = function(data, data_names=NULL, time.scale=YEAR){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 6. NTN: Minimum TN
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' minimum_tn(data=tmin.value)
minimum_tn = calculate_6 = function(data, data_names=NULL, time.scale=YEAR){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 7. GTG: Mean TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' mean_tg(data=taverage.value)
mean_tg = calculate_7 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data, data_names=data_names, time.scale=time.scale))
}

#' 8. XTG: Maximum TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' maximum_tg(data=taverage.value)
maximum_tg = calculate_8 = function(data, data_names=NULL, time.scale=YEAR){
  return(maximum_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 9. NTG: Minimum TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time 
#' @param time.scale month, season or year
#' @return average temperature
#' @export
#' @examples
#' minimum_tg(data=taverage.value)
minimum_tg = calculate_9 = function(data, data_names=NULL, time.scale=YEAR){
  return(minimum_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 10. CD: Cold days
#' Percentages of days with maximum temperatures lower than the 10th percentile.
#' 
#' @param data maximum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return Cold days
#' @export
#' @examples
#' cold_days(data=tmax.value)
cold_days = calculate_10 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(data, c(.10), na.rm = TRUE)
  function_ = function(data, value){    
    return(100*sumf(data<value)/length(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 11. CN: Cold nights
#' Percentages of days with minimum temperatures lower than the 10th percentile.
#' 
#' @param data minimum temperature
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return Cold nights
#' @export
#' @examples
#' cold_nights(data=tmin.value)
cold_nights = calculate_11 = function(data, data_names=NULL, time.scale=YEAR){
  return(calculate_10(data, data_names, time.scale))
}

#' 12. CDDI: Cold spell duration index
#' Count of days with at least 6 consecutive days when TN < 10th percentile
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Cold spell duration index
#' @export
#' @examples
#' cddi(data=tmin.value)
cddi = calculate_12 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(data, c(.10), na.rm = TRUE)
  function_ = function(data, value){
    data.10 = data <= value
    data.rle = rle(as.numeric(data.10))
    aux = data.rle$values==1 & data.rle$lengths>=6
    count = sumf(data.rle$lengths[aux])
    return(count)
  }

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)

  return(byYears)
}

#' 13. DTR: Diurnal temperature range
#' Mean difference between TX and TN.
#'
#' @param max maximum temperature 
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Diurnal temperature range
#' @export
#' @examples
#' dtr(max=tmax.value, min=tmin.value)
dtr = calculate_13 = function(max, min, data_names=NULL, time.scale=YEAR){
  data = max - min
  byMonths = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=meanf)
  return(byMonths)
}

#' 14. vDTR
#' Mean absolute day-to-day difference in DTR
#'
#' @param max maximum temperature 
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return vDTR
#' @export
#' @examples
#' vdtr(max=tmax.value, min=tmin.value)
vdtr = calculate_14 = function(max, min, data_names=NULL, time.scale=YEAR){
  data = max - min
  data = abs(data[1:(length(data)-1)]-data[2:length(data)])

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=meanf)
  return(byYears)
}

#' 15. FD: Frost days
#' Number of days with minimum temperature <0ºC.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return frost days
#' @export
#' @examples
#' frost_days(data=tmin.value)
frost_days = calculate_15 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data<0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 16. GSL = Growing season length
#' Annual count of days between the first span of at least 6 days with Tmean >5ºC and first span after 1 July of 6 days with Tmean <5 ºC.
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @return growing season length
#' @export
#' @examples
#' gsl(data=taverage.value)
gsl = calculate_16 = function(data, data_names=NULL){
  function_ = function(data){
    BREAKVALUE = 5
    times.data = chron(names(data))
    aux = whichf(times.data == chron(paste0("7/1/", unique(years(times.data)))))
    data.max = whichf(data>BREAKVALUE)[1] #Mayores que 5
    data.min = data<BREAKVALUE #Menores que 5
    data.min[1:length(data.min)<=aux] = FALSE# Menores que 5 después del 1 de julio
    data.min.rle = rle(as.numeric(data.min))
    aux = whichf(data.min.rle$length>6 & data.min.rle$values==1)[1]
    if(is.na(aux) | length(aux)<=0 | length(data.max)<=0){
      count = 0
    }else{
        data.min = sumf(data.min.rle$length[1:aux])
        count = data.min-data.max-7
    }
    return(count)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  # byYears = byYears - 7
  return(byYears)
}

#' 17. ID: Ice days
#' Number of days with maximum temperature <0ºC.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return ice days
#' @export
#' @examples
#' ice_days(data=tmax.value)
ice_days = calculate_17 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data<0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 18. CFD: Maximum number of consecutive frost days.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return maximum consecutive frost
#' @export
#' @examples
#' cfd(data=tmin.value)
cfd = calculate_18 = function(data, data_names=NULL, time.scale=YEAR){
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

#' 19. ETR: Extreme temperature range
#' Difference between the highest TX and the lowest TN.
#' 
#' @param max maximum temperature
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return extreme temperature range
#' @export
#' @examples
#' etr(max=tmax.value, min=tmin.value)
etr = calculate_19 = function(max, min, data_names=NULL, time.scale=YEAR){
  function_ = function(max, min){
    calculate = maxf(max)-minf(min) 
    return(calculate)
  }
  data = cbind(max, min)
  byYears = calcf_data(data=max, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, min=min)
  return(byYears)
}

#' 20. SUD: Summer days
#' Number of days with maximum temperature >25ºC.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Summer days
#' @export
#' @examples
#' summer_days(data=tmax.value)
summer_days = calculate_20 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>25))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 21. CSD: Maximum number of consecutive summer days (TX > 25º)
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return consecutive summer days
#' @export
#' @examples
#' csd(data=tmax.value)
csd = calculate_21 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    data.rle = rle(as.numeric(data>25))
    calculate = maxf(data.rle$lengths[data.rle$values>0])
    if(calculate<0){
      calculate = 0
    }
    return(calculate)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 22. TS: Temperature sums
#' (days tx >17◦C)–(days TX <17◦C)
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Temperature sums
#' @export
#' @examples
#' temperature_sums(data=tmax.value)
temperature_sums = calculate_22 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>17)-sumf(data<17))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 23. TN: Tropical nights
#' Number of days with minimum temperature >20ºC.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Tropical nights
#' @export
#' @examples
#' tropical_nights(data=tmin.value)
tropical_nights = calculate_23 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){  
    return(sumf(data>20))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 24. HD17: Heating degree days
#' (sum(17-TG)) only for days with TG<17ºC
#' 
#' @param data mean temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return HD17
#' @export
#' @examples
#' hd17(data=taverage.value)
hd17 = calculate_24 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(17-data[!is.na(data) & data < 17]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 25. VCD: Very cold days
#' Number of days with minimum temperature <1st percentile.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Very cold days
#' @export
#' @examples
#' very_cold_days(data=tmin.value)
very_cold_days = calculate_25 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sumf(data<value))
  }
  value = quantile(data, c(.01), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 26. VWD: Very warm days
#' Number of days with maximum temperature >99th percentile per year.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Very warm days
#' @export
#' @examples
#' very_warm_days(data=tmax.value)
very_warm_days = calculate_26 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sumf(data>value))
  }
  value = quantile(data, c(.99), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 27. WD: Warm days
#' Percentages of days with maximum temperatures higher than the 90th percentile.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Warm days
#' @export
#' @examples
#' warm_days(data=tmax.value)
warm_days = calculate_27 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sumf(data>value))
  }
  value = quantile(data, c(.90), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 28. WN: Warm nights
#' Percentages of days with minimum temperatures higher than the 90th percentile.
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Warm nights
#' @export
#' @examples
#' warm_nights(data=tmin.value)
warm_nights = calculate_28 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sumf(data>value))
  }
  value = quantile(data, c(.90), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 29. WSD: Warm spell duration index
#' Count of days with at least 6 consecutive days when TX > 90th percentile.
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Warm spell duration index
#' @export
#' @examples
#' wsd(data=tmax.value)
wsd = calculate_29 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    data.rle = rle(as.numeric(data>value))
    aux = data.rle$values==1 & data.rle$lengths>=6
    count = sumf(data.rle$lengths[aux])
    return(count)
  }
  value = quantile(data, c(.90), na.rm = TRUE)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=value)
  return(byYears)
}

#' 30. ZCD: zero crossing days
#' Number of days with Tmax > 0 ºC and Tmin < 0 ºC.
#' 
#' @param max maximum temperature
#' @param min minimum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return zero crossing days
#' @export
#' @examples
#' zcd(max=tmax.value, min=tmin.value)
zcd = calculate_30 = function(max, min, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data))
  }
  data = max>0 & min<0
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 31. OGS6: Onset of growing season 6 days
#' The start of the first span with at least 6 days with Tmean >5ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Onset of growing season 1
#' @export
#' @examples
#' ogs6(data=taverage.value)
ogs6 = calculate_31 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data>5))
    aux = whichf(data.rle$values==1 & data.rle$lengths>=6)[1]
    count = sumf(data.rle$lengths[1:aux])-data.rle$lengths[aux]+1
    return(count)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 32. OGS10: Onset of growing season 2
#' The start of the first span with at least 10 days with Tmean >5ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Onset of growing season 2
#' @export
#' @examples
#' ogs10(data=taverage.value)
ogs10 = calculate32 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=taverage.value[unique(years(names(taverage.value)))[12]==years(names(taverage.value))])
  function_ = function(data){
    data.rle = rle(as.numeric(data>5))
    aux = whichf(data.rle$values==1 & data.rle$lengths>=10)[1]
    count = sumf(data.rle$lengths[1:aux])-data.rle$lengths[aux]+1
    return(count)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 33. Ta_o: Growing season temperature 1
#' Growing season (april to october) mean TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Growing season temperature 1
#' @export
#' @examples
#' ta_o(data=taverage.value)
ta_o = calculate_33 = function(data, data_names=NULL){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 34. Tm_s: Growing season temperature 2
#' Growing season (may to september) mean TG
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Growing season temperature 2
#' @export
#' @examples
#' tm_s(data=taverage.value)
tm_s = calculate_34 = function(data, data_names=NULL){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(MAY, JUN, JUL, AUG, SEP)]
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 35. GD4: Growing degree days
#' Sum of degree days over 4ºC
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return GD4
#' @export
#' @examples
#' gd4(data=taverage.value)
gd4 = calculate_35 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data[data>4]-4))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 36. WKI: Winkler index
#' Sum of degree days over 10°C from April 1 until October 31 = Sum max [(avg. daily temp. – 10), 0]
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return Winkler index
#' @export
#' @examples
#' winkler_index(data = tmax.value)
winkler_index = calculate_36 = function(data, data_names=NULL){
  function_ = function(data){  
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]
    return(sumf(data[data>10]-10))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 37. WSI: Winter Severity index
#' Mean temperature of the coldest month of the year
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return Winter Severity index
#' @export
#' @examples
#' wsi(data = tmax.value)
wsi = calculate_37 = function(data, data_names=NULL){
  function_ = function(data){  
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), data_names=NULL, operation=meanf)
    return(byMonths[!is.na(byMonths) & byMonths==minf(byMonths)])
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 38. STX32: Temperature sums 1a
#' Sums of maximum temperatures >= 32ºC
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return temperature sums 1
#' @export
#' @examples
#' stx32(data = tmax.value)
stx32 = calculate_38 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, month){
    return(sumf(data[data>=32]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, month=selectTime)
  return(byYears)
}

#' 39. D32: Temperature sums 1b
#' Number of days whith TX≥32˚C
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return temperature sums 1
#' @export
#' @examples
#' d32(data = tmax.value)
d32 = calculate_39 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=32))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 40. STN15: Temperature sums 2
#' Sums of minimum air temperatures <= -15ºC recorded in December-February interval
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @return temperature sums 2
#' @export
#' @examples
#' snt15(data = tmin.value)
snt15 = calculate_40 = function(data, data_names=NULL){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sumf(data[data <= -15]))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}


#' 41. STN10: Temperature sums 3
#' Sums of minimum air temperatures <=-10ºC recorded in December-February interval
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @return temperature sums 3
#' @export
#' @examples
#' stn10(data = tmin.value)
stn10 = calculate_41 = function(data, data_names=NULL){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(DEC, JAN, FEB)]
    return(sumf(data[data <= -10]))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 42. PTG: Temperature sums 5
#' Sums of positive TG  calculated for the 1st of February to the 10th April interval
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return temperature sums 5
#' @export
#' @examples
#' ptg(data = taverage.value)
ptg = calculate_42 = function(data, data_names=NULL){
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

####Precipitation-based
#' 43. TP: Total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return total precipitation
#' @export
#' @examples
#' total_precipitation(data = pr.value)
total_precipitation = calculate_43 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 45. XP: Maximum precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return maximum precipitation
#' @export
#' @examples
#' xp(data = pr.value)
xp = calculate_44 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 45. R10mm
#' Annual count of days when daily precipitation amount >= 10mm
#' 
#' @param data precipitation
#' @param date date
#' @param time.scale month, season or year
#' @return R10mm
#' @export
#' @examples
#' r10mm(data = pr.value)
r10mm = calculate_45 = function(data, date, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=10))
  }
  byYears = calcf_data(data=data, date=date, extract_names=select_time_function(time.scale), operation=function_)
  return(byYears)
}

#' 46. R20mm
#' Annual count of days when daily precipitation amount >= 20mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return R20mm
#' @export
#' @examples
#' r20mm(data = pr.value)
r20mm = calculate_46 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. function_(data=pr.value[unique(years(names(pr.value)))[12]==years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=20))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 49. Rx1day
#' Maximum 1-day precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Rx1day
#' @export
#' @examples
#' rx1day(data = pr.value)
rx1day = calculate_49 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 50. Rx5day
#' Maximum consecutive 5-day precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Rx5day
#' @export
#' @examples
#' rx5day(data = pr.value)
rx5day = calculate_50 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    data2 = c(data[c(2:length(data))], 0)
    data3 = c(data[c(3:length(data))], 0, 0)
    data4 = c(data[c(4:length(data))], 0, 0, 0)
    data5 = c(data[c(5:length(data))], 0, 0, 0, 0)
    data.sum = data + data2 + data3 + data4 + data5
    return(maxf(data.sum))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 51. SDII: Simple precipitation intensity index
#' Sum of precipitation in wet days (days with >1mm of precipitation), and dividing that by the number of wet days in the period.
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SDII
#' @export
#' @examples
#' sdii(data = pr.value)
sdii = calculate_51 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data[data>=1])/sumf(data>=1))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 52. DD: Dry days
#' Number of days with less than 1 mm/day
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return dry days
#' @export
#' @examples
#' dry_days(data = pr.value)
dry_days = calculate_52 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data < 1))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 53. EP: Effective precipitation
#' Precipitation minus evapotranspiration
#' 
#' @param pr precipitation
#' @param eto et0
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return effective precipitation
#' @export
#' @examples
#' ep(pr = pr.value, eto = eto.value)
ep = calculate_53 = function(pr, eto, data_names=NULL, time.scale=YEAR){
  function_ = function(pr, eto){
    eto = eto[names(eto)%in%names(pr)]
    return(sumf(pr-eto))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, eto=eto)
  return(byYears)
}

#' 54. LDP: Longest dry period
#' Maximum length of consecutive dry days (RR<1)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return longest dry period
#' @export
#' @examples
#' ldp(data = pr.value)
ldp = calculate_54 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    data.rle = rle(as.numeric(data<1))
    count = 0
    if(sumf(data.rle$values==1)>0){
      count = maxf(data.rle$lengths[data.rle$values==1])
    }
    return(count)  
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 55. LWP: Longest wet period
#' Maximum length of consecutive wet days (RR>=1)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return longest wet period
#' @export
#' @examples
#' lwp(data = pr.value)
lwp = calculate_55 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    data.rle = rle(as.numeric(data>=1))
    count = 0
    if(sumf(data.rle$values==1)>0){
      count = maxf(data.rle$lengths[data.rle$values==1])
    }
    return(count)  
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 56. PVWD: Precipitation fraction due to very wet days
#' Precipitation at days exceeding the 95percentile divided by total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return PVWD
#' @export
#' @examples
#' pvwd(data = pr.value)
pvwd = calculate_56 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(data, c(.95), na.rm = TRUE)
  function_ = function(data, value){
    return(100*sumf(data[data>value])/sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  byYears[is.na(byYears)] = 100
  return(byYears)
}

#' 57. PEWD: Precipitation fraction due to extremely wet days
#' Precipitation at days exceeding the 99percentile divided by total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return PEWD
#' @export
#' @examples
#' pewd(data = pr.value)
pewd = calculate_57 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(data, c(.99), na.rm = TRUE)
  function_ = function(data, value){
    return(100*sumf(data[data>value])/sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  byYears[is.na(byYears)] = 100
  return(byYears)
}


#' 58. HPD: Heavy precipitation days
#' Number of days with precipitation above 50mm
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return heavy precipitation days
#' @export
#' @examples
#' hpd(data = pr.value)
hpd = calculate_58 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data, value){
    return(sumf(data>value))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=50)
  return(byYears)
}


#' 59. R95p
#' Days when RR > 95p
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return R95p
#' @export
#' @examples
#' r95p(data = pr.value)
r95p = calculate_59 = function(data, data_names=NULL, time.scale=YEAR){
  data.q = quantile(data, c(.95), na.rm = TRUE)
  function_ = function(data, value){    
    return(sumf(data>data.q))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 60. PCI: Precipitation Concentration Index
#' PCI=100*sum(Pi^2)/P^2
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return PCI
#' @export
#' @examples
#' pci(data = pr.value)
pci = calculate_60 = function(data, data_names=NULL){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sumf)
    data2Sum = sumf(byMonths^2)
    dataSum2 = sumf(byMonths)^2
    return(100*data2Sum/dataSum2)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 61. MFI: Modified Fournier Index
#' A precipitation concentration index
#' MFI=sum(Pi^2)/P
#' https://es.scribd.com/document/76414093/modified-fournier-index
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return MFI
#' @export
#' @examples
#' mfi(data = pr.value)
mfi = calculate_61 = function(data, data_names=NULL){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sumf)
    data2Sum = sumf(byMonths^2)
    dataSum2 = sumf(byMonths)
    return(data2Sum/dataSum2)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 62. GSP: Growing season precipitation
#' Growing season (april to october) total precipitation
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return GSP
#' @export
#' @examples
#' gsp(data = pr.value)
gsp = calculate_62 = function(data, data_names=NULL){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(APR, MAY, JUN, JUL, AUG, SEP, OCT)]    
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 63. NGSP: Non-growing season precipitation
#' October to april total precipitation, can inform on the resource available for low potential evaporation conditions
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return non growing precipitation
#' @export
#' @examples
#' ngsp(data = pr.value)
ngsp = calculate_63 = function(data, data_names=NULL){
  function_ = function(data){
    data = data[months(chron(names(data)))%in%c(OCT,NOV, DEC, FEB, MAR, APR)]
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 64. TPWD: Total precipitation in wet days
#' Precipitation amount on days with RR >= 1 mm in a choosen period (e.g. year)
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return precipitation in wet days
#' @export
#' @examples
#' tpwd(data = pr.value)
tpwd = calculate_64 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data[data>=1]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 65. RR1 
#' Wet days >= 1 mm (days), ECA&D standard
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return RR1
#' @export
#' @examples
#' rr1(data = pr.value)
rr1 = calculate_65 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=1)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 66. RR3 
#' Wet days >= 3mm (days), ECA&D standard
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return RR3
#' @export
#' @examples
#' rr3(data = pr.value)
rr3 = calculate_66 = function(data, data_names=NULL, time.scale=YEAR){
  #Ej. result = function_(data=pr.value[unique(months_years(names(pr.value)))[12]==months_years(names(pr.value))])
  function_ = function(data){
    return(sumf(data>=3)) #mm
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

####Bioclimatic
#' 67. BIO10
#' TG of Warmest Quarter
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO10
#' @export
#' @examples
#' bio10(data = taverage.value)
bio10 = calculate_67 = function(data, data_names=NULL){
  data.q = quantile(data, c(.75), na.rm = TRUE)
  function_ = function(data, value){
    return(meanf(data[data>=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names, value=data.q)
  return(byYears)
}

#' 68. BIO11
#' TG of Coldest Quarter
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO11
#' @export
#' @examples
#' bio11(data = taverage.value)
bio11 = calculate_68 = function(data, data_names=NULL){
  data.q = quantile(data, c(.25), na.rm = TRUE)
  function_ = function(data, value){
    return(meanf(data[data<=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names, value=data.q)
  return(byYears)
}

#' 69. BIO13
#' Precipitation of Wettest Month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return BIO13
#' @export
#' @examples
#' bio13(data = pr.value)
bio13 = calculate_69 = function(data, data_names=NULL){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sumf)
    return(maxf(byMonths))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 70. BIO14
#' Precipitation of Driest Month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return BIO14
#' @export
#' @examples
#' bio14(data = pr.value)
bio14 = calculate_70 = function(data, data_names=NULL){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=sumf)
    return(minf(byMonths))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 71. BIO15
#' Precipitation Seasonality (Coefficient of Variation)
#' This is a measure of the variation in monthly precipitation totals over the course of the year. This index is the ratio of the standard deviation of the monthly total precipitation to the mean monthly total precipitation (also known as the coefficient of variation) and is expressed as a percentage.
#'
#' @param data precipitation
#' @param data_names names of each period of time
#' @return BIO15
#' @export
#' @examples
#' bio15(data = pr.value)
bio15 = calculate_71 = function(data, data_names=NULL){
  function_ = function(data){
    return(sd(data, na.rm = TRUE)/meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 72. BIO16
#' Precipitation of Wettest Quarter
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return BIO16
#' @export
#' @examples
#' bio16(data = pr.value)
bio16 = calculate_72 = function(data, data_names=NULL){
  data.q = quantile(data, c(.75), na.rm = TRUE)
  function_ = function(data, value){
    return(sumf(data[data>=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 73. BIO17
#' Precipitation of Driest Quarter
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return BIO17
#' @export
#' @examples
#' bio17(data = pr.value)
bio17 = calculate_73 = function(data, data_names=NULL){
  data.q = quantile(data, c(.25), na.rm = TRUE)
  function_ = function(data, value){
    return(sumf(data[data<=value]))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, value=data.q)
  return(byYears)
}

#' 74. BIO18
#' Precipitation of Warmest Quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @return BIO18
#' @export
#' @examples
#' bio18(pr=pr.value, taverage=taverage.value)
bio18 = calculate_74 = function(pr, taverage, data_names=NULL){
  data.q = quantile(taverage, c(.75), na.rm = TRUE)
  data = pr[taverage>=data.q]
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 75. BIO19
#' Precipitation of Coldest Quarter
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @return BIO19
#' @export
#' @examples
#' bio19(pr=pr.value, taverage=taverage.value)
bio19 = calculate_75 = function(pr, taverage, data_names=NULL){
  data.q = quantile(taverage, c(.25), na.rm = TRUE)
  data = pr[taverage<=data.q]
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 76. BIO4
#' Temperature Seasonality (standard deviation *100)
#' The amount of temperature variation over a given year (or averaged years) based on the standard deviation (variation) of monthly temperature averages.
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO4
#' @export
#' @examples
#' bio4(data = taverage.value)
bio4 = calculate_76 = function(data, data_names=NULL){
  function_ = function(data){
    return(100*sd(data, na.rm = TRUE))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 77. BIO5
#' TX of Warmest Month
#' 
#' @param data maximum temperature
#' @param data_names names of each period of time
#' @return BIO5
#' @export
#' @examples
#' bio5(data = tmax.value)
bio5 = calculate_77 = function(data, data_names=NULL){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    return(maxf(byMonths))
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}

#' 78. BIO6
#' TN of Coldest Month
#' 
#' @param data minimum temperature
#' @param data_names names of each period of time
#' @return BIO6
#' @export
#' @examples
#' bio6(data = tmin.value)
bio6 = calculate_78 = function(data, data_names=NULL){
  function_ = function(data){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    return(minf(data))
  }
  byYears = calcf_data(data=data, data_names=data_names, extract_names=years, operation=function_)
  return(byYears)
}

#' 79. BIO7
#' Temperature Annual Range (BIO5-BIO6)
#' TX of Warmest Month minus TM of Coldest Month
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return BIO7
#' @export
#' @examples
#' bio7(data = taverage.value)
bio7 = calculate_79 = function(data, data_names=NULL){
  return(bio5(data, data_names=data_names)-bio6(data, data_names=data_names))
}

#' 80. BIO8
#' TG of Wettest Quarter
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return BIO8
#' @export
#' @examples
#' bio8(taverage = taverage.value, pr = pr.value)
bio8 = calculate_80 = function(taverage, pr, data_names=NULL){
  data.q = quantile(pr, c(.75), na.rm = TRUE)
  data = taverage[pr>=data.q]
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 81. BIO9
#' TG of Driest Quarter
#' 
#' @param taverage medium temperature 
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return BIO9
#' @export
#' @examples
#' bio9(taverage = taverage.value, pr = pr.value)
bio9 = calculate_81 = function(taverage, pr, data_names=NULL){
  data.q = quantile(pr, c(.25), na.rm = TRUE)
  data = taverage[pr<=data.q]
  function_ = function(data){    
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_)
  return(byYears)
}

#' 82. BIO20
#' Annual mean radiation (W m-2)
#' https://www.edenextdata.com/?q=content/climond-bioclimatic-variables-2030 
#' 
#' @param data radiation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO20
#' @export
#' @examples
#' bio20(data = radiation.value)
bio20 = calculate_82 = function(data, data_names=NULL, time.scale=YEAR){
  return(average_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 83. UTCI: Universal Thermal Climate Index
#' UTCI (Blazejczyk et all, 2012) (Air temperature, Humidity, Wind)
#' https://goo.gl/by4hH9
#' http://www.utci.org/
#' https://rdrr.io/github/alfcrisci/rBiometeo/man/UTCI.html
#' Copy https://github.com/alfcrisci/rBiometeo
#' 
#'Given air temperature (Celsius), relative humidity (%), wind velocity (m/sec) and mean radiant temperature ( tmrt in Celsius degree) gives the Universal Thermal Climate Index in Celsius.
#' @param ta medium temperature
#' @param rh dew point
#' @param wind average wind
#' @param tmrt radiation
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return BIO20
#' @export
#' @examples
#' utci(ta = taverage.value, rh = dew_point.value, wind = w.value, tmrt = radiation.value)
utci = calculate_83 = function(ta, rh, wind, tmrt, data_names=NULL, time.scale=YEAR){
    e = ta/10 # e = es(ta)/10; # use vapour pressure in kPa 
    pa = (e*rh/100.0); 
    va = wind;
    va[va < 0.51] = 0.5
    va[va > 17] = 17

    dtm = tmrt - ta;

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
    
    data = apply(utci, c(1), sumf)
    return(average_temp(data=data, data_names=data_names, time.scale=time.scale))
}

#' 84. MI: Mould index
#' Number of days with  a relative humidity over 90\% in combination with temperatures above 10°C 
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Mould index
#' @export
#' @examples
#' mould_index(taverage = taverage.value, rh = rh.value)
mould_index = calculate_84 = function(taverage, rh, data_names=NULL, time.scale=YEAR){
  data = taverage>10 & rh>90
  function_ = function(data){    
    return(sumf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 85. HI: Heat Index: temperature + humidity 
#' http://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
#' Available from the R package weathermetrics
#' 
#' @param taverage medium temperature
#' @param rh relative humidity
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Heat Index
#' @export
#' @examples
#' heat_index(taverage = taverage.value, rh = rh.value)
heat_index = calculate_85 = function(taverage, rh, data_names=NULL, time.scale=YEAR){
  data = heat.index(t = taverage, rh = rh, temperature.metric = "celsius")
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 86. WCI: Wind Chill Index: temperature + wind 
#' Osczevski, R. & Bleustein, M. 2005, Bull. Amer. Meteor. Soc., 86, 1453, doi:10.1175/BAMS-86-10-1453 
#' https://journals.ametsoc.org/doi/abs/10.1175/BAMS-86-10-1453
#' Ta = air temperature; in ºC ; v = wind speed in km/h  
#' http://www.calculator.net/wind-chill-calculator.html
#' WCI = 13.12 + 0.6215*T - 11.37*V + 0.3965*T*V
#' 
#' @param taverage medium temperature
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return WCI
#' @export
#' @examples
#' wci(taverage = taverage.value, w = w.value)
wci = calculate_86 = function(taverage, w, data_names=NULL, time.scale=YEAR){
  data = 13.12 + 0.6215*taverage - 11.37*w + 0.3965*taverage*w
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 87. AT: Apparent Temperature
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
#' at(taverage = taverage.value, w = w.value, vapor = vapor.value)
at = calculate_87 = function(taverage, w, vapor, data_names=NULL, time.scale=YEAR){
  e = 6.015*exp(17.2*taverage/(237.7+taverage))*vapor/100
  data = taverage + 0.33 * e - 0.70 * w - 4.00
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

####wind-based
#' 88. Gustmax
#' number of days with wind gusts above 21 m/s
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Gustmax
#' @export
#' @examples
#' gustmax(data = w.value)
gustmax = calculate_88 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>21))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 89. FXx
#' Maximun value of daily maximum wind gust (m/s), ECA&D standard
#' 
#' @param data maximum wind gust
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FXx
#' @export
#' @examples
#' fxx(data = w_max.value)
fxx = calculate_89 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(maxf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 90. FG
#' Mean of daily mean wind strength (m/s), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FG
#' @export
#' @examples
#' fg(data = w.value)
fg = calculate_90 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 91. FGcalm
#' Calm days (FG <= 2 m/s) (days), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FGcalm
#' @export
#' @examples
#' fgcalm(data = w.value)
fgcalm = calculate_91 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data<=2))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 92. FG6Bft: 
#' Days with daily averaged wind >= 6 Bft (10.8 m/s) (days), ECA&D standard
#' 
#' @param data wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FG6Bft
#' @export
#' @examples
#' fg6bft(data = w.value)
fg6bft = calculate_92 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=10.8))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


####aridity/continentality-indices
#' 93. Eto: Reference Evapotranspiration
#' If data available using Fao-56 Penman-Monteith, if not using the Hargreaves & Samani method.
#' 
#' @param data Eto
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return Eto
#' @export
#' @examples
#' eto(data = eto.value)
eto = calculate_93 = function(data, data_names=NULL, time.scale=YEAR){

  # data = hsPET(date, lat=42, t.min, t.max)

  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


#' 94. UAI: UNEP Aridity Index
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
#' uai(eto = eto.value, pr = pr.value)
uai = calculate_94 = function(eto, pr, taverage, data_names=NULL, time.scale=YEAR){
  data = pr/eto
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}


#' 95. CMD: Climatic Moisture Deficit
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
#' cmd(eto = eto.value, pr = pr.value)
cmd = calculate_95 = function(eto, pr, taverage, data_names=NULL, time.scale=YEAR){
  data = eto-pr
  function_ = function(data){
    return(meanf(data))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 96. MAI: De Martonne Aridity Index
#' Annual rainfall/(Annual TG+10)
#' De Martonne = P / (T + 10); P is the annual total amount of precipitation (mm) and T is the mean annual air temperature (°C)
#' 
#' @param pr precipitation
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @return Martonne Aridity Index
#' @export
#' @examples
#' mai(pr = pr.value, taverage = taverage.value)
mai = calculate_96 = function(pr, taverage, data_names=NULL){
  function_ = function(data, taverage){
    taverage = taverage[names(taverage) %in% names(data)]
    data = sumf(data)
    taverage = meanf(taverage)
    return(data/(taverage+10))
  }
  byYears = calcf_data(data=pr, extract_names=select_time_function(YEAR), data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}

#' 97. EAI: Emberger Aridity Index
#' (100*annual rainfall)/(TGhottest month2-TG coldest month2)
#' 
#' @param taverage medium temperature
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return Emberger Aridity Index
#' @export
#' @examples
#' eai(taverage = taverage.value, pr = pr.value)
eai = calculate_97 = function(taverage, pr, data_names=NULL){
  function_ = function(data, pr){
    byMonths = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    p = sumf(pr) #meanf(pr) ¿media anual de precipitación?
    return(100*p / (maxf(byMonths)^2 - minf(byMonths)^2))
  }
  byYears = calcf_data(data=taverage, extract_names=years, operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}

#' 98. JCI: Johansson Continentality Index
#' (1.7E/sinf)-20.4 where E (in8C) is the annual range of mean monthly air temperatures and f is the geographical latitude of the station
#' ( 1.7 * the annual range of monthly mean air temperatures grados / sin(geographic latitude grados) ) - 20.4
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param value lat
#' @return JCI
#' @export
#' @examples
#' jci(data = taverage.value, value = lat)
jci = calculate_98 = function(data, data_names=NULL, value){
  function_ = function(data, value){  
    data = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    return((1.7 * (maxf(data)-minf(data)) / sin(value)) - 20.4)
  }
  byYears = calcf_data(data=data, extract_names=years, operation=function_, data_names=data_names, value=value)
  return(byYears)
}

#' 99. KOI: Kerner Oceanity Index
#' (100*(To-Ta))/E where To and Ta are the October and April mean values of TG respectively and E is the annual range of monthly mean air temperatures, in°C.
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @return Kerner Oceanity Index
#' @export
#' @examples
#' koi(data = taverage.value)
koi = calculate_99 = function(data, data_names=NULL){  
  function_ = function(data){
    data = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    data = 100 * (data[colnames(data)==OCT]-data[colnames(data)==APR]) / (maxf(data)-minf(data))
    return(data)
  }
  byYears = calcf_data(data=data, extract_names=years, data_names=data_names, operation=function_)
  return(byYears)
}


#' 100. PiCI: Pinna Combinative index
#' 1/2((P/(T+10))+(12Pd/(Td+10))) where P and T are the annual mean values of precipita-tion and air temperature and P′d,T′dare the mean values ofprecipitation and air temperature of the driest month
#' 
#' @param pr precipitation 
#' @param taverage medium temperature
#' @param data_names names of each period of time
#' @return Pinna Combinative index
#' @export
#' @examples
#' pici(pr = pr.value, taverage = taverage.value)
pici = calculate_100 = function(pr, taverage, data_names=NULL){
  function_ = function(data, taverage){
    taverage = taverage[names(taverage) %in% names(data)]
    data.month = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    taverage.month = calcf_data(data=taverage, extract_names=select_time_function(MONTH), operation=meanf)
    return(1/2* ((meanf(data)/(meanf(taverage)+10)) + (12*minf(data.month) / (taverage.month[data.month==minf(data.month)][1]+10))))
  }
  byYears = calcf_data(data=pr, extract_names=years, data_names=data_names, operation=function_, taverage=taverage)
  return(byYears)
}

#' 101. BI: Budyko Index
#' (Rn/L*P)*100, where Rn is the mean annual net radiation (also known as the net radiation balance), P is the mean annual precipitation, and L is the latent heat of vaporization for water
#' https://es.wikipedia.org/wiki/Clasificaci%C3%B3n_clim%C3%A1tica_de_Budyko
#' de vaporización: 2257 kJ/kg (539,4 cal/g) a 97 °C.
#' 
#' @param data net radiation 
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return Budyko Index
#' @export
#' @examples
#' budyko_index(data = radiation.value, pr = pr.value)
budyko_index = calculate_101 = function(data, pr, data_names=NULL){
  function_ = function(data, pr){
    pr = pr[names(pr) %in% names(data)]
    r = 0.8 * meanf(data)
    pp = meanf(pr)
    l = 2257
    return(100*pp/(l*r))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names, pr=pr)
  return(byYears)
}

#' 102. MOI: Marsz Oceanity Index
#'  MOI = ( 0.731 * geographic latitude grados + 1.767 ) / the annual range of monthly mean air temperatures grados
#' 
#' @param data medium temperature
#' @param data_names names of each period of time
#' @param value lat 
#' @return n0to10
#' @export
#' @examples
#' moi(data = taverage.value, value = lat)
moi = calculate_102 = function(data, data_names=NULL, value){
  function_ = function(data, value){
    data = calcf_data(data=data, extract_names=select_time_function(MONTH), operation=meanf)
    return(( 0.731 * value + 1.767 ) / (maxf(data)-minf(data)) )
  }
  byYears = calcf_data(data=data, extract_names=years, operation=function_, data_names=data_names, value=value)
  return(byYears)
}

####snow-based
#' 103. SS: Snowfall sum
#' Sum of snowfall
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return n0to10
#' @export
#' @examples
#' snowfall_sum(data = snow.value)
snowfall_sum = calculate103 = function(data, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=sumf, data_names=data_names)
  return(byYears)
}

#' 104. SD0_10: Snow depth n0to10
#' The number of days with snow depth in the range 1-10 cm
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SD0_10
#' @export
#' @examples
#' sd0_10(data = snow_depth.value)
sd0_10 = calculate_104 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=1 & data<=10))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 105. SD10_20: Snow depth n10to20
#' The number of days with snow depth of 10-20 cm
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SD10_20
#' @export
#' @examples
#' sd10_20(data = snow_depth.value)
sd10_20 = calculate_105 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>=10 & data<=20))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 106. SD: snow depth
#' mean of daily snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return snow depth
#' @export
#' @examples
#' snow_depth(data = snow_depth.value)
snow_depth = calculate_106 = function(data, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

#' 107. FSD: Freq. of snow days
#' number of snow days
#' 
#' @param data snowfall
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return freq. of snow days
#' @export
#' @examples
#' freq_snow_days(data = snow.value)
freq_snow_days = calculate_107 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 108. MSD: Mild snowy days
#' annual number of days with snow depth more than 5 cm.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return mild snowy days
#' @export
#' @examples
#' msd(data = snow_depth.value)
msd = calculate_108 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>5))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 109. HSD: Heavy snowy days
#' annual number of days with snow depth more than 50 cm.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return heavy snowy days
#' @export
#' @examples
#' hsd(data = snow_depth.value)
hsd = calculate_109 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>50))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 110. FSC: The arrival date of first snowcover
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @return first snowcover
#' @export
#' @examples
#' fsc(data = snow_depth.value)
fsc = calculate_110 = function(data, data_names=NULL){
  function_ = function(data){    
    return(whichf(data>0)[1])
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names)
  return(byYears)
}

#' 111. FPSC: The arrival date of first permanent snowcover
#' First day of the longest period with consecutive snow cover day.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @return first permanent snowcover
#' @export
#' @examples
#' fpsc(data = snow_depth.value)
fpsc = calculate_111 = function(data, data_names=NULL){
  function_ = function(data){
    data = as.numeric(data>0)
    data.rle = rle(as.numeric(data))
    data.i = whichf(data.rle$lengths>1 & data.rle$values>0)
    if(length(data.i)>0){
      data.i = data.i[length(data.i)]
      if(is.na(data.i) | data.i==1){
        return(data.i)
      }else{
          return(sumf(1, data.rle$lengths[1:(data.i-1)]))
      }
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names)
  return(byYears)
}

#' 112. LPSC: The departure date of last permanent snowcover
#' Last day of the longest period with consecutive snow cover day.
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @return last permanent snowcover
#' @export
#' @examples
#' lpsc(data = snow_depth.value)
lpsc = calculate_112 = function(data, data_names=NULL){
  function_ = function(data){
    data = as.numeric(data>0)
    data.rle = rle(as.numeric(data))
    data.i = whichf(data.rle$lengths>1 & data.rle$values>0)
    if(length(data.i)>0){
      data.i = data.i[length(data.i)]
      if(is.na(data.i) | data.i==1){
        return(data.i)
      }else{
          return(sumf(1, data.rle$lengths[1:(data.i)])-1)
      }
    }else{
      return(NA)
    }
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(YEAR), operation=function_, data_names=data_names)
  return(byYears)
}


#' 113. ASD: Average snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return average snow depth
#' @export
#' @examples
#' asd(data = snow_depth.value)
asd = calculate_113 = function(data, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

#' 114. SCD: Amount of snow covered days
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return snow covered days
#' @export
#' @examples
#' scd(data = snow_depth.value)
scd = calculate_114 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(sumf(data>0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}


#' 115. MSD: Maximum snow depth
#' 
#' @param data snow depth
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return maximum snow depth
#' @export
#' @examples
#' msd(data = snow_depth.value)
msd = calculate_115 = function(data, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=maxf, data_names=data_names)
  return(byYears)
}

####Cloud/radiation-based
#' 116. SSD: Sum of sunshine duration
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SSD, h
#' @export
#' @examples
#' ssd(data = insolation.value)
ssd = calculate_116 = function(data, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=sumf, data_names=data_names)
  return(byYears)
}


#' 117. SND: Sunny days
#' days with mean cloud cover less than 10\%.
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SND
#' @export
#' @examples
#' snd(data = cloud_cover.value)
snd = calculate_117 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(sumf(data<10))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 118. ClD: Cloudy days
#' Number of days with cloud base below 100 meter.
#' 
#' @param data cloud base below 100 meter
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return cloudy days
#' @export
#' @examples
#' cloudy_days(data = cloud_cover_less_100.value)
cloudy_days = calculate_118 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(sumf(data>0))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 119. CC: Mean CC
#' Mean daily cloud cover.
#' 
#' @param data cloud cover
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return mean CC
#' @export
#' @examples
#' mean_cc(data = cloud_cover.value)
mean_cc = calculate_119 = function(data, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

#' 120. SSp
#' Sunshine duration fraction with respect to day length (\%), standard ECA&D: 100*(SS/SSmax) SS: sum of sunshine duration, SSmax: maximun daylight hours 
#' 
#' @param data sunshine duration
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return SSp
#' @export
#' @examples
#' ssp(data = insolation.value)
ssp = calculate_120 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){    
    return(100*meanf(data)/24)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(byYears)
}

#' 121. ACI: Atmospheric Clarity Index
#' Ratio between solar radiation at surface and solar radiation at TOA (alt top of the atmosphere empirically obtained)
#' https://goo.gl/Wzs1Zk
#' http://www.greenrhinoenergy.com/solar/radiation/atmosphere.php
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return ACI
#' @export
#' @examples
#' aci(data = radiation.value, toa=toa.value)
aci = calculate_121 = function(data, toa, data_names=NULL, time.scale=YEAR){
  function_ = function(data, toa){
    toa = toa[names(toa) %in% names(data)]
    return(meanf(data/toa))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names, toa=toa)
  return(byYears)
}

####Drought indices
#' 122. SPI 1: Standardized Precipitation Index, 1 month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return SPI
#' @export
#' @examples
#' spi_1(data = pr.value)
spi_1 = calculate_122 = function(data, data_names=NULL){
  return(calc_spi(data, data_names, scale=1))
}

#' 123. SPI 3: Standardized Precipitation Index, 3 month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return SPI
#' @export
#' @examples
#' spi_3(data = pr.value)
spi_3 = calculate_123 = function(data, data_names=NULL){
  return(calc_spi(data, data_names, scale=3))
}

#' 124. SPI 6: Standardized Precipitation Index, 6 month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return SPI
#' @export
#' @examples
#' spi_6(data = pr.value)
spi_6 = calculate_124 = function(data, data_names=NULL){
  return(calc_spi(data, data_names, scale=6))
}

#' 125. SPI 12: Standardized Precipitation Index, 12 month
#' 
#' @param data precipitation
#' @param data_names names of each period of time
#' @return SPI
#' @export
#' @examples
#' spi_12(data = pr.value)
spi_12 = calculate_125 = function(data, data_names=NULL){
  return(calc_spi(data, data_names, scale=12))
}

#' 126. SPEI 1: Standardized Precipitation Evapotranspiration Index, 1 month
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return SPEI
#' @export
#' @examples
#' spei_1(eto = eto.value, pr = pr.value)
spei_1 = calculate_126 = function(eto, pr, data_names=NULL){
  return(calc_spei(eto, pr, data_names, scale=1))  
}

#' 127. SPEI 1: Standardized Precipitation Evapotranspiration Index, 3 month
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return SPEI
#' @export
#' @examples
#' spei_3(eto = eto.value, pr = pr.value)
spei_3 = calculate_127 = function(eto, pr, data_names=NULL){
  return(calc_spei(eto, pr, data_names, scale=3))
}

#' 128. SPEI 6: Standardized Precipitation Evapotranspiration Index, 6 month
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return SPEI
#' @export
#' @examples
#' spei_6(eto = eto.value, pr = pr.value)
spei_6 = calculate_128 = function(eto, pr, data_names=NULL){
    return(calc_spei(eto, pr, data_names, scale=6))
}

#' 129. SPEI 12: Standardized Precipitation Evapotranspiration Index, 12 month
#' 
#' @param eto et0
#' @param pr precipitation
#' @param data_names names of each period of time
#' @return SPEI
#' @export
#' @examples
#' spei_12(eto = eto.value, pr = pr.value)
spei_12 = calculate_129 = function(eto, pr, data_names=NULL){
   return(calc_spei(eto, pr, data_names, scale=12))
}

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
#' @return FWI
#' @export
#' @examples
#' fwi(taverage=taverage.value, rh = rh.value, w = w.value, 
#' pr = pr.value, lat = lat, dew_point=dew_point.value)
fwi = calculate_130 = function(taverage, rh, w, pr, dew_point, lat, data_names=NULL, time.scale=YEAR){

  # fergus: Comparar las 2 funciones
  dayLength = DayLengths(lat)
  data = index_CFWI(Month=as.numeric(months(names(taverage))),Days=as.POSIXlt(chron(names(taverage)))$yday+1,Temp=taverage, Dew = dew_point, WS = w, Rain = pr, daylist=dayLength)
  names(data) = names(taverage)

  missing.values = is.na(taverage) | is.na(names(taverage)) | is.na(rh) | is.na(pr) | is.na(w)
  data.nas = fwi1D(months=as.numeric(months(names(taverage[!missing.values]))), Tm=taverage[!missing.values], H=rh[!missing.values], r=pr[!missing.values], W=w[!missing.values], lat = lat)
  data = taverage
  data[!missing.values] = data.nas

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

#' 131. KBDI: Keetch-Byran Drought Index
#' Combination of daily maximum in temperature and precipitation
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
#' kbdi(taverage = taverage.value, rh=rh.value, w=w.value, pr=pr.value)
kbdi = calculate_131 = function(taverage, pr, rh, w, data_names=NULL, time.scale=YEAR){
  # fergus: Comparar las 2 funciones
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), operation=sumf)
  map = meanf(byYears)
  data = index_KBDI(Temperature=taverage, Rain=pr, MAP=map)
  names(data) = names(taverage)

  missing.values = is.na(taverage) | is.na(names(taverage)) | is.na(rh) | is.na(pr) | is.na(w)
  data.nas = kbdindex(date=chron(names(taverage[!missing.values])), t=taverage[!missing.values], p=pr[!missing.values], h=rh[!missing.values], w=w[!missing.values]/1000)
  data = taverage
  data[!missing.values] = data.nas

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

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
#' @return FFDI
#' @export
#' @examples
#' ffdi(taverage = taverage.value, pr=pr.value, rh=rh.value, w=w.value)
ffdi = calculate_132 = function(taverage, pr, rh, w, data_names=NULL, time.scale=YEAR){
  byYears = calcf_data(data=pr, extract_names=select_time_function(time.scale), operation=sumf)
  map = meanf(byYears)
  kdbiData = index_KBDI(Temperature=taverage, Rain=pr, MAP=map)
  data = index_MA(Temperature=taverage, Rain=pr, DewPoint=rh, MAP=map, Wind=w, KBDI=kdbiData)
  names(data) = names(taverage)
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

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
#' @return MNI
#' @export
#' @examples
#' mni(dew_point=dew_point.value, rh=rh.value, taverage=taverage.value, pr=pr.value)
mni = calculate_133 = function(dew_point, taverage, rh, pr, data_names=NULL, time.scale=YEAR){
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

  data = nesterovIndex(t=taverage, rh=rh, p=pr)
  names(data) = names(taverage)

  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=meanf, data_names=data_names)
  return(byYears)
}

#' 134. FFFI: Finnish Forest Fire Index
#' Combination of temperature, relative humidity, wind speed, radiation and precipitation (package fireDanger)
#' https://link.springer.com/chapter/10.1007%2F978-3-642-55903-7_88
#' P-eto+-f1
#' https://github.com/jbedia/fireDanger
#' 
#' @param data net radiation 
#' @param toa solar radiation at TOA
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return FFFI
#' @export
#' @examples
#' fffi(data = radiation.value, toa=toa.value)
fffi = calculate_134 = function(data, toa, data_names=NULL, time.scale=YEAR){
  function_ = function(data, toa){
    toa  = toa[]
    return(data/toa)
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, toa=toa, data_names=data_names)
  return(byYears)
}

####Tourism
#' 135. HCI:Urban
#' Holliday Climate Index for Urban destinations (Scott et all, 2016) (Tmax,wind,cloudiness,RH, precipitation) Scott, D., Rutty, M., Amelung, B. and Tang, M. (2016): An inter-comparison of the Holiday Climate Index (HCI) and the Tourism Climate Index (TCI), Atmosphere, 7, 80, doi:10.3390/atmos7060080
#' 
#' @param pr precipitation
#' @param w average wind
#' @param data_names names of each period of time
#' @param time.scale month, season or year
#' @return HCI
#' @export
#' @examples
#' hci(pr = pr.value, w=w.value)
hci = calculate_135 = function(pr, w, data_names=NULL, time.scale=YEAR){
  #tc: thermal comfort 
  # data = 4*tc + 2*cloud_cover + 3*pr + w
  # function_ = function(data){
  #   return(meanf(data))
  # }
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(NULL)
}

#' 136. TCI: Tourism Climatic Index
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
#' tci(data=pr.value, sunshine=radiation.value, w=w.value)
tci = calculate_136 = function(data, sunshine, w, data_names=NULL, time.scale=YEAR){
  # #cia: Daily Comfort Index
  # #cid: Daytime Comfort Index 
  # data = 2 * ( 4*cid + cia + 2*pr + 2*sunshine + w )
  # function_ = function(data){
  #   return(meanf(data))
  # }
  # byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), operation=function_, data_names=data_names)
  return(NULL)
}

#' 137. TCI60: TCI>60
#' Number of days TCI>60 , standard ECA&D
#' 
#' @param data Tourism Climatic Index
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return TCI60
#' @export
#' @examples
#' tci60(data = tci.value)
tci60 = calculate_137 = function(data, data_names=NULL, time.scale=YEAR){
  function_ = function(data){
    return(sumf(data>60))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}

#' 138. TCI80: Excellent tourism days
#' Number of days TCI>80, standard ECA&D
#' 
#' @param data Tourism Climatic Index
#' @param time.scale month, season or year
#' @param data_names names of each period of time
#' @return TCI80
#' @export
#' @examples
#' tci80(data = tci.value)
tci80 = calculate_138 = function(data, data_names=NULL, time.scale=YEAR){ 
  function_ = function(data){
    return(sumf(data>80))
  }
  byYears = calcf_data(data=data, extract_names=select_time_function(time.scale), data_names=data_names, operation=function_)
  return(byYears)
}
