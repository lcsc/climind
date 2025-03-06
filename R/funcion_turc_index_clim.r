#' Turc Index
#' 
#' @param tmax daily maximum temperature, Celsius
#' @param tmin daily minimum temperature, Celsius
#' @param rh relative humidity, percentage 
#' @param pr daily precipitation, mm
#' @param toa solar radiation, J/m2/day
#' @param lat latitude, degree
#' @param wfc water Field Capacity, initial water balance value
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return index value
#' @keywords internal
calc_turc_index = function(tmax, tmin, rh, pr, toa, lat, wfc, data_names=NULL, na.rm=FALSE){
  if(is.null(tmax)) { return(NULL) }
	toa_ok <- toa / 1000000
  byMonths = calcf_data(data=tmax, time.scale=MONTH, operation=mean, na.rm=FALSE, data_names=NULL)
	tmin_byMonths = calcf_data(data=tmin, time.scale=MONTH, operation=mean, na.rm=FALSE, data_names=NULL)
	rh_byMonths = calcf_data(data=rh, time.scale=MONTH, operation=mean, na.rm=FALSE, data_names=NULL)
	pr_byMonths = calcf_data(data=pr, time.scale=MONTH, operation=sum, na.rm=FALSE, data_names=NULL)
	toa_byMonths = calcf_data(data=toa_ok, time.scale=MONTH, operation=sum, na.rm=FALSE, data_names=NULL)
	lat_byMonths = calcf_data(data=lat, time.scale=MONTH, operation=mean, na.rm=FALSE, data_names=NULL)
	wfc_byMonths = calcf_data(data=wfc, time.scale=MONTH, operation=mean, na.rm=FALSE, data_names=NULL)
  # byMonths = byMonths[as.character(1979:2017), ]
  if((na.rm & sum(!is.na(byMonths))!=0) | (!na.rm & sum(is.na(byMonths))==0)){
    byMonths.vector = array(t(byMonths), dim=length(byMonths))
		tmin_byMonths.vector = array(t(tmin_byMonths), dim=length(tmin_byMonths))
		rh_byMonths.vector = array(t(rh_byMonths), dim=length(rh_byMonths))
		pr_byMonths.vector = array(t(pr_byMonths), dim=length(pr_byMonths))
		toa_byMonths.vector = array(t(toa_byMonths), dim=length(toa_byMonths))
		lat_byMonths.vector = array(t(lat_byMonths), dim=length(lat_byMonths))
		wfc_byMonths.vector = array(t(wfc_byMonths), dim=length(wfc_byMonths))
    spi.vector = array(month_turc_index(byMonths.vector, tmin_byMonths.vector, rh, pr_byMonths.vector, toa_byMonths.vector, lat_byMonths.vector, wfc_byMonths.vector)$fitted[, 1])
  }else{
    spi.vector = NA
  }
  spi.matrix = t(array(spi.vector, dim=c(dim(byMonths)[2], dim(byMonths)[1])))
  colnames(spi.matrix)=colnames(byMonths)
  rownames(spi.matrix)=rownames(byMonths)
  spi.matrix[is.na(byMonths)] = NA
  return(spi.matrix)
}

#' Turc Index by month
#' 
#' @param tmax daily maximum temperature, Celsius
#' @param tmin daily minimum temperature, Celsius
#' @param rh relative humidity, percentage 
#' @param pr daily precipitation, mm
#' @param toa solar radiation, MJ/m2/day
#' @param lat latitude, degree
#' @param wfc Water Field Capacity, initial water balance value
#' @param data_names names of each period of time
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return index value
#' @keywords internal
month_turc_index <- function(tmax, tmin, rh, pr, toa, lat, wfc){
	years.data <- length(tmax)

	# --- 1. Julian days setup ---
  YearlyMidpoints <- rep(c(15, 46, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349), years.data)  # Midpoint days for each month
  AvgTemp <- (tmax + tmin) / 2  # Average temperature

  # --- 2. Solar factor (Fh) ---
  # Solar declination angle (radians)
  delta <- 0.409 * sin(0.0172 * YearlyMidpoints - 1.39)
  # Latitude in radians
  latr <- lat / 57.2957795

  # Sunset hour angle (omegas)
  omegas <- acos(pmin(pmax(-tan(latr) * tan(delta), -1), 1))  # Ensure omegas is in [-1, 1]

  # Day length (N)
  DayLengthInHours <- 7.64 * omegas  # Approximate day length in hours

  # Solar factors (Fh1, Fh2)
  Fh1 <- DayLengthInHours - 5 - ((lat / 40)^2)  # Adjustment for latitude and day length
  Fh2 <- 0.03 * ((toa * 23.884) - 100)  # Solar radiation contribution
  Fh1.2 <- pmin(Fh1, Fh2)  # Minimum solar factor between Fh1 and Fh2
  Fh <- pmax(Fh1.2, 0)  # Set negative values to zero

  # --- 3. Thermal factor (Ft) ---
  Ft.1 <- (AvgTemp * (60 - AvgTemp) / 1000) * ((tmin - 1) / 4)  # Adjusted factor when tmin is between 1 and 5
  Ft.2 <- AvgTemp * (60 - AvgTemp) / 1000  # Standard thermal factor
  Ft <- ifelse(tmin <= 1, 0, ifelse(tmin < 5, Ft.1, Ft.2))  # Conditions for tmin

  # --- 4. Reference evapotranspiration (ETo) ---
  ETo.1 <- 0.4 * (AvgTemp / (AvgTemp + 15)) * (23.884 * toa + 50)  # ETo for rh > 50%
  ETo.2 <- ETo.1 * (1 + (50 - rh) / 70)  # Adjusted ETo for rh <= 50%
  ETo <- ifelse(rh > 50, ETo.1, ETo.2)  # Conditional ETo
  ETo <- pmax(ETo, 0)  # Prevent negative ETo values

  # --- 5. Water balance calculation ---
  # Initialize variables for water balance
  ET <- R <- DIF <- rep(0, length(AvgTemp))  # ET: actual evapotranspiration; R: water reserve; DIF: water balance difference
  R[1] <- WFC  # Initial water reserve

  for (i in 2:length(AvgTemp)) {
    # Update water reserve (R): it cannot exceed 100 or go below 0
    R[i] <- pmin(pmax(R[i - 1] + pr[i - 1] - ET[i - 1], 0), 100)

    # Water balance difference
    DIF[i] <- R[i] + pr[i] - ETo[i]

    # Actual evapotranspiration
    ET[i] <- ifelse(DIF[i] > 0, ETo[i], R[i] + pr[i])
  }

  # --- 6. Dryness factor (Fs) ---
  X <- pmin(ETo, ETo * 0.3 + 50)  # Adjusted dryness threshold
  EtDeficit <- ETo - ET  # Evapotranspiration deficit
  # Dryness factor (Fs), constrained to non-negative values
  Fs <- ifelse(X == 0 & EtDeficit == 0, 0, pmax((X - EtDeficit) / X, 0))

  # --- 7. Final Turc Index calculation (CA) ---
  CA <- Fh * Ft * Fs  # Combined index: product of solar, thermal, and dryness factors

  return(CA)
}
