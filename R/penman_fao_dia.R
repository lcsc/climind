
# Ej. en penman_fao_period en /mnt/dostb2/fuendetodos/DATOS/repositorio/spei/trunk/fergus/datos/code_web_maps/penman_fao_raster.R
#' FAO-56 Penman-Monteith reference evapotranspiration (ET_0)
#'
#' @param Tmin minimum temperature, Celsius
#' @param Tmax maximum temperature, Celsius
#' @param U2 average wind, m/s at 2m
#' @param J day of the year
#' @param Ra radiation, (MJ m-2 d-1)
#' @param lat latitude, degrees, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
#' @param Rs daily incoming solar radiation (MJ m-2 d-1)
#' @param tsun sunshine duration, hours
#' @param CC CC
#' @param ed actual vapour pressure
#' @param Tdew dew point, Celsius
#' @param RH relative humidity, percentage
#' @param P atmospheric pressure, kPa
#' @param P0 P0
#' @param z mde
#' @param crop "short" short reference crop or "tail" tail reference crop
#' @param na.rm na.rm
#' @return et0, mm/day
#' @keywords internal
penman_fao_diario <-
  function(Tmin, Tmax, U2, J, Ra=NA, lat=NA, Rs=NA, tsun=NA, CC=NA, ed=NA, Tdew=NA, RH=NA, P=NA, P0=NA, z=NA, crop='short', na.rm=FALSE) {

    ET0 <- Tmin*NA

    n <- length(Tmin)
    ##m <- ncol(Tmin)
    ##c <- cycle(Tmin)

    # Mean temperature
    T <- (Tmin + Tmax)/2

    # 1. Latent heat of vaporization, lambda (eq. 1.1)
    # lambda <- 2.501 - 2.361e-3*T

    # 3. Psychrometric constant, gamma (eq. 1.4)
    # 4. P: atmospheric pressure, kPa
    # if(is.na(P0)){ P0  <- matrix(101.3,nrow=nrow(T),ncol=ncol(T)) }
    if (is.na(P0)) {
        dim = dim(T)
        if (is.null(dim)) {
            dim = length(T)
        }
        P0  <- array(101.3, dim = dim)
    }
    if (is.na(P)) {
        P <- P0*(((293 - 0.0065*z)/293)^5.26)
    }

    ## FAO
    gamma <- 0.665e-3*P

    # 6. Saturation vapour pressure, ea
    # saturation vapour pressure at tmx (eq. 1.10, p. 66)
    etmx <- 0.611*exp((17.27*Tmax)/(Tmax + 237.3))
    # saturation vapour pressure at tmn (eq. 1.10, p. 66)
    etmn <- 0.611*exp((17.27*Tmin)/(Tmin + 237.3))
    # mean saturation vapour pressure (eq. 1.11, p. 67)
    ea <- (etmx + etmn)/2

    ## We need et when we use FAO-PM
    et <- 0.611*exp((17.27*T)/(T + 237.3))

    # 2. Slope of the saturation vapour pressure function, Delta (eq. 1.3)

    #Allen, 1994
    ## Delta <- 4099*ea/(T+237.3)^2

    # FAO
    Delta <- 4099*et/(T + 237.3)^2
    #Delta <- 2504*exp((12.27*T)/(T+237.3))/(T+237.3)^2

    # 7. Actual vapour pressure, ed
    if (length(ed) != n | sum(!is.na(ed)) == 0) {
      if (length(Tdew) == n) {
        # (eq. 1.12, p. 67)
        ed <- 0.611*exp((17.27*Tdew)/(Tdew + 237.3))
      } else if (length(RH) == n) {
        # (eq. 1.16, p. 68)

        ## Allen, 1994
        #ed <- RH / ((50/etmn)+(50/etmx))

        ## FAO
        ed <- ea*(RH/100)
      } else {
        # (eq. 1.19, p. 69)
        ed <- etmn
      }
    }

    ## Usando Tdew hay dias en que el diferencial de tensión de vapor sale negativo. Zonas donde la interpolación satura...
    ## son zonas donde la humedad relativa satura al 100%, imponemos ed = ea
    ww <- which(ea - ed < 0)
    if (length(ww) > 0) {
      ed[ww] <- ea[ww]
    }

    ### FINS AQUI FUNCIONA ###

    # delta: solar declination, rad (1 rad = 57.2957795 deg) (eq. 1.25)
    delta <- 0.409*sin(0.0172*J - 1.39)

    # dr: relative distance Earth-Sun, [] (eq. 1.24)
    dr <- 1 + 0.033*cos(0.0172*J)

    # omegas: sunset hour angle, rad (eq. 1.23)
    latr <- lat/57.2957795

    ### FINS AQUI FUNCIONA ###
    sset <- Tmin
    sset <- -tan(latr)*tan(delta)

    omegas <- sset*0
    omegas[sset >= {-1} & sset <= 1] <- acos(sset[sset >= {-1} & sset <= 1])
    # correction for high latitudes
    omegas[sset < {-1}] <- max(omegas)

    # 9. Extraterrestrial radiation, Ra (MJ m-2 d-1)

    # Estimate Ra (eq. 1.22)
    if (sum(!is.na(Ra)) == 0) {
        Ra <- 37.6*dr*(omegas*sin(latr)*sin(delta) + cos(latr)*cos(delta)*sin(omegas))
    }
    ##Ra <- ifelse(values(Ra)<0,0,Ra)

    # 11. Net radiation, Rn (MJ m-2 d-1)
    # Net radiation is the sum of net short wave radiation Rns and net long wave
    # (incoming) radiation (Rnl).
    # Rs: daily incoming solar radiation (MJ m-2 d-1)

    # nN: relative sunshine fraction []
    if (sum(!is.na(Rs)) == 0) {
      if (length(tsun) == n) {
        # Based on sunshine hours
        # 10. Potential daylight hours (day length, h), N (eq. 1.34)
        N <- 7.64*omegas
        nN <- tsun/N
      }else{
        return(ET0)
      }
      # (eq. 1.37)
      as <- 0.25; bs <- 0.5
      Rs <- Tmin
      Rs <- (as + bs*(nN))*Ra
    }

    # Rso: clear-sky solar radiation (eq. 1.40)
    # Note: mostly valid for z<6000 m and low air turbidity
    #if (ncol(as.matrix(z))==ncol(as.matrix(Tmin))) {
    if (exists('z') & sum(!is.na(z)) > 0) {
      Rso <- (0.75 + 2e-5*z) * Ra
    } else {
      Rso <- (0.75 + 2e-5*840) * Ra
    }
    # Empirical constants
    ac <- 1.35; bc <- -0.35; a1 <- 0.34; b1 <- -0.14
    # Reference crop albedo
    alb <- 0.23
    # Rn, MJ m-2 d-1 (eq. 1.53)
    Rn <- (1 - alb)*Rs - (ac*Rs/Rso + bc) * (a1 + b1*sqrt(ed)) * 4.9e-9 * ((273.15 + Tmax)^4 + (273.15 + Tmin)^4)/2
    Rn[Rs == 0] <- 0

    # Soil heat flux density, G
    # Using forward / backward differences for the first and last observations,
    # and central differences for the remaining ones.
    G <- 0
    # Wind speed at 2m, U2 (eq. 1.62)
    #U2 <- U2 * 4.85203/log((zz-0.08)/0.015)

    # Daily ET0 (eq. 2.18)
    if (crop == 'short') {
      c1 <- 900; c2 <- 0.34 # short reference crop (e.g. clipped grass, 0.12 m)
    } else {
      c1 <- 1600; c2 <- 0.38 # tall reference crop (e.g. alfalfa, 0.5 m)
    }
    ET0 <- (0.408*Delta*(Rn - G) + gamma*(c1/(T + 273))*U2*(ea - ed)) /
      (Delta + gamma*(1 + c2*U2))

    return(ET0)
}

#' Transforma datos de in en r o al revés
#'
#' @param J Días de inicio de cada semana del año, partiendo desde 0 ¿?
#' @param lat latitud en grados en spTransform(coordenadas,CRS(crslonlat))
#' @param tsun Insolación en horas de sol o radiación en ¿MJ/m2?
#' @param z mde, modelo de elevación digital del terreno
#' @param ret Que hacer, calcular in desde r o al contrario
#'
#' @return insolación en horas de sol o radiación en ¿MJ/m2?
#' @keywords internal
penman_rs <- function(J,
           lat = NA,
           tsun = NA,
           z = NA,
           ret = RADIATION) {
    # delta: solar declination, rad (1 rad = 57.2957795 deg) (eq. 1.25)
    delta <- 0.409 * sin(0.0172 * J - 1.39)

    # dr: relative distance Earth-Sun, [] (eq. 1.24)
    dr <- 1 + 0.033 * cos(0.0172 * J)

    # omegas: sunset hour angle, rad (eq. 1.23)
    latr <- lat / 57.2957795

    ### FINS AQUI FUNCIONA ###
    sset <- -tan(latr) * tan(delta)

    omegas <- sset * 0
    omegas[sset >= {
      -1
    } & sset <= 1] <- acos(sset[sset >= {
      -1
    } & sset <= 1])
    # correction for high latitudes
    omegas[sset < {
      -1
    }] <- max(omegas)

    # 9. Extraterrestrial radiation, Ra (MJ m-2 d-1)

    # Estimate Ra (eq. 1.22)
    Ra <-
      37.6 * dr * (omegas * sin(latr) * sin(delta) + cos(latr) * cos(delta) *
                     sin(omegas))

    # Based on sunshine hours
    # 10. Potential daylight hours (day length, h), N (eq. 1.34)
    N <- 7.64 * omegas

    # (eq. 1.37)
    as <- 0.25
    bs <- 0.5

    if (ret == RADIATION) {
      # Devolvemos C_R
      Rs <- (as + bs * (tsun / N)) * Ra
    } else{
      # Devolvemos C_IN
      Rs = N * (tsun / Ra - as) / bs
    }
    Rs[Rs < 0] = 0
    return(Rs)
}

#' Daily FAO-56 Penman-Monteith reference evapotranspiration (ET0)
#'
#' Computes daily reference evapotranspiration (ET0) using the FAO-56
#' Penman-Monteith formulation for a short or tall reference crop.
#' This implementation is valid \strong{only for daily time steps}; i.e.
#' it, assumes soil heat flux \eqn{G = 0}.
#'
#' @param Tmin Numeric vector of minimum daily air temperature (Celsius).
#' @param Tmax Numeric vector of maximum daily air temperature (Celsius).
#' @param U2 Numeric vector of daily mean wind speed at 2 m (m s\eqn{^{-1}}).
#' @param J Integer vector of Julian day (1-366).
#'
#' @param Ra Optional numeric vector of daily extraterrestrial radiation
#'   (MJ m\eqn{^{-2}} d\eqn{^{-1}}). If not provided, it is computed from
#'   \code{J} and \code{lat} using \code{potential_radiation()}.
#' @param lat Numeric scalar or vector giving latitude in decimal degrees
#'   (positive north, negative south). Required if \code{Ra} is not supplied,
#'   or if \code{tsun} (sunshine duration) is used to derive \code{Rs}.
#' @param Rs Optional numeric vector of incoming shortwave radiation
#'   at the surface (MJ m\eqn{^{-2}} d\eqn{^{-1}}). If not provided,
#'   it is estimated from \code{tsun} and \code{Ra} using the
#'   Ångström-Prescott relation.
#' @param tsun Optional numeric vector of daily bright sunshine duration (h).
#'   Used to estimate \code{Rs} when \code{Rs} is not provided.
#' @param CC Optional numeric vector of cloud cover (fraction or \%).
#'   Currently not used (reserved for future extensions).
#'
#' @param ea Optional numeric vector of actual vapour pressure (kPa).
#'   If provided with the correct length, it is used directly and other
#'   humidity inputs (\code{Tdew}, \code{Twet}, \code{Tdry}, \code{RHx},
#'   \code{RHn}, \code{RH}) are ignored.
#' @param Tdew Optional numeric vector of dew point temperature (Celsius),
#'   used to derive \code{ea} when \code{ea} is not supplied.
#' @param Twet Optional numeric vector of wet bulb temperature (Celsius),
#'   used to derive \code{ea} when \code{ea} is not supplied.
#' @param Tdry Optional numeric vector of dry bulb temperature (Celsius),
#'   used to derive \code{ea} when \code{ea} is not supplied.
#' @param apsy Optional psychrometer ventilation coefficient (1 / Celsius).
#'   Typical values are 0.000662 for ventilated (Assmann type) psychrometers,
#'   0.000800 for naturally ventilated psychrometers,
#'   0.001200 for non-ventilated psychrometers installed indoors.
#' @param RHx Optional numeric vector of maximum daily relative humidity (\%).
#'   Used to derive \code{ea} when neither \code{ea}, \code{Tdew} nor
#'   (\code{Twet}, \code{Tdry}) are supplied.
#' @param RHn Optional numeric vector of minimum daily relative humidity (\%).
#'   Used together with \code{RHx} to derive \code{ea} when both are available.
#' @param RH Optional numeric vector of relative humidity (\%, daily mean
#'   or representative). Used to derive \code{ea} when none of
#'   \code{ea}, \code{Tdew}, (\code{Twet}, \code{Tdry}), or
#'   (\code{RHx}, \code{RHn}) are supplied.
#'
#' @param P Optional numeric vector or scalar of atmospheric pressure (kPa).
#'   If not provided, it is estimated from \code{P0} and \code{z}.
#' @param P0 Optional numeric vector or scalar of reference pressure (kPa),
#'   typically standard sea-level pressure (101.3 kPa). Used as a base to
#'   compute \code{P} from \code{z} when \code{P} is not provided.
#' @param z Optional numeric vector or scalar of elevation above sea level (m),
#'   used to estimate \code{P} when \code{P} is missing.
#'
#' @param crop Character string indicating the reference crop type.
#'   One of \code{"short"} or \code{"tall"}.
#'   \itemize{
#'     \item \code{"short"}: short reference crop (clipped grass, height
#'       \eqn{\sim 0.12} m), consistent with FAO-56 grass reference.
#'     \item \code{"tall"}: tall reference crop (alfalfa-type, height
#'       \eqn{\sim 0.5} m), used in ASCE but included here for convenience.
#'   }
#'
#' @param na.rm Logical, currently unused. Reserved for future handling of
#'   missing values within the calculation.
#'
#' @details
#' The function implements the FAO-56 Penman-Monteith equation on a daily
#' time step. Solar geometry and extraterrestrial radiation \eqn{R_a} are
#' computed following FAO-56 using \code{potential_radiation}, and
#' \eqn{R_s} (incoming shortwave radiation) can be supplied or estimated
#' from sunshine duration using the Ångström-Prescott relation.
#'
#' Net radiation \eqn{R_n} is computed as the sum of net shortwave and
#' net longwave radiation, adopting the commonly used FAO-56 coefficients.
#' The soil heat flux \eqn{G} is assumed negligible at daily time steps
#' and set to zero.
#'
#' The \code{crop} argument affects only the aerodynamic term through constants
#' \code{c1} and \code{c2}:
#' \itemize{
#'   \item \code{crop = "short"}: \code{c1 = 900}, \code{c2 = 0.34}.
#'   \item \code{crop = "tall"}: \code{c1 = 1600}, \code{c2 = 0.38}.
#' }
#'
#' \strong{Humidity input precedence}
#'
#' The actual vapour pressure \code{ea} is derived using the first available
#' input (or combination of inputs) in the following precedence order:
#'
#' \tabular{lll}{
#'   \strong{Order} & \strong{Inputs used} & \strong{Formula / Source} \cr
#'   1 & \code{ea} & used directly \cr
#'   2 & \code{Tdew} & \eqn{e_a = e_s(T_{dew})} (FAO-56 Eq. 14) \cr
#'   3 & \code{Twet + Tdry + apsy} & \eqn{e_a = e_s(T_{wet}) - a_{psy} P (T_{dry} - T_{wet})} (FAO-56 Eq. 15-16) \cr
#'   4 & \code{RHx + RHn} & \eqn{e_a = (e_{s,min} RH_x + e_{s,max} RH_n)/2} (FAO-56 Eq. 17) \cr
#'   5 & \code{RHx} & \eqn{e_a = e_{s,min} RH_x} (FAO-56 Eq. 18) \cr
#'   6 & \code{RH} & \eqn{e_a = e_s RH/100} (FAO-56 Eq. 19) \cr
#'   7 & fallback \code{Tmin} & \eqn{e_a = e_{s,min}} (ICID Eq. 1.19; FAO fallback) \cr
#' }
#'
#' If none of these inputs is available (all are missing or all-NA),
#' the function raises an error.
#'
#' @return
#' A numeric vector of length equal to \code{length(Tmin)} with daily reference
#' evapotranspiration values (mm d\eqn{^{-1}}).
#'
#' @examples
#' # FAO-56 Example 18; reference ET0 is 3.9 mm / day
#' Tmin <- 12.3                  # Celsius
#' Tmax <- 21.5                  # Celsius
#' RHx <- 84                     # %
#' RHn <- 63                     # %
#' U2 <- 2.078                   # m/s
#' tsun <- 9.25                  # hours
#' z  <- 100                     # m a.s.l
#' J  <- 187                     # day of year
#' lat <- 50.8                   # degrees N
#'
#' pm_et0_fao(
#'   Tmin = Tmin, Tmax = Tmax, U2 = U2, J = J,
#'   lat = lat, tsun = tsun, RHx = RHx, RHn = RHn, z = z
#' )
#'
#' @export
pm_et0_fao <- function(
    Tmin,
    Tmax,
    U2,
    J,
    Ra    = NA,
    lat   = NA,
    Rs    = NA,
    tsun  = NA,
    CC    = NA,
    ea    = NA,
    Tdew  = NA,
    Twet  = NA,
    apsy  = NA,
    Tdry  = NA,
    RHx   = NA,
    RHn   = NA,
    RH    = NA,
    P     = NA,
    P0    = NA,
    z     = NA,
    crop  = c("short", "tall"),
    na.rm = FALSE
) {  
  n <- length(Tmin)
  crop   <- match.arg(crop)  # Mean air temperature (Celsius)
  Tmean <- (Tmin + Tmax) / 2  # Latent heat of vaporization (MJ kg-1)
  lambda <- 2.501 - 2.361e-3 * Tmean  # --- Atmospheric pressure and psychrometric constant ---  
  if (all(is.na(P0))) {
    P0 <- rep(101.3, n)  # sea-level reference (kPa)
  }  
  if (all(is.na(P))) {
    if (all(is.na(z))) {
      P <- P0
    } else {
      P <- P0 * (((293 - 0.0065 * z) / 293) ^ 5.26)
    }
  }  # FAO-56: gamma = 0.665e-3 * P
  gamma <- 0.665e-3 * P  # --- Saturation vapour pressure ---  # FAO-56 eq. 11: saturated vapour pressure (kPa)
  etmx <- 0.6108 * exp((17.27 * Tmax) / (Tmax + 237.3))
  etmn <- 0.6108 * exp((17.27 * Tmin) / (Tmin + 237.3))
  et   <- 0.6108 * exp((17.27 * Tmean) / (Tmean + 237.3))  # FAO-56 eq. 12: mean saturation vapour pressure (kPa)
  es <- (etmx + etmn) / 2  # --- Actual vapour pressure ea (kPa) ---  
  if (!all(is.na(ea))) {
    if (length(ea) != n && length(ea) != 1) {
      stop("If 'ea' is supplied, it must have length 1 or the same length as 'Tmin'.")
    }  } else if (!all(is.na(Tdew))) {
    # from dew-point temperature (FAO-56 eq. 14)
    ea <- 0.6108 * exp((17.27 * Tdew) / (Tdew + 237.3))  } else if (!all(is.na(Twet)) && !all(is.na(Tdry))) {
    # from psychrometric data; FAO-56 eq. 15-16
    if (all(is.na(apsy))) {
      stop("Psychrometer ventilation coefficient 'apsy' (1/Celsius) must be supplied when using Twet/Tdry.")
    }
    es_wet <- 0.6108 * exp((17.27 * Twet) / (Twet + 237.3))
    gpsy   <- apsy * P  # kPa/Celsius
    ea     <- es_wet - gpsy * (Tdry - Twet)  } else if (!all(is.na(RHx)) && !all(is.na(RHn))) {
    # from max and min daily relative humidity; FAO-56 eq. 17
    ea <- (etmn * (RHx / 100) + etmx * (RHn / 100)) / 2  } else if (!all(is.na(RHx)) && all(is.na(RHn))) {
    # from max daily relative humidity only; FAO-56 eq. 18
    ea <- etmn * (RHx / 100)  } else if (!all(is.na(RH)) && all(is.na(RHx)) && all(is.na(RHn))) {
    # from mean daily relative humidity; FAO-56 eq. 19
    ea <- es * (RH / 100)  } else if (length(Tmin) == n) {
    # fallback: from Tmin (ICID eq. 1.19; FAO fallback)
    ea <- etmn  } else {
    stop(
      "Failed to compute actual vapour pressure 'ea'. ",
      "Please provide 'ea', 'Tdew', 'Twet/Tdry/apsy', 'RHx/RHn', 'RHx' or 'RH' with the same length as 'Tmin'."
    )
  }  # clamp ea to es if needed
  idx <- which(ea > es)
  if (length(idx) > 0) {
    ea[idx] <- es[idx]
  }  # --- Slope of saturation vapour pressure curve, Delta (FAO-56 eq. 13) ---  
  Delta <- 4098 * et / (Tmean + 237.3)^2  # --- Solar geometry and radiation ---  # Daylength N (h), only needed if tsun is used
  if (!all(is.na(lat))) {
    latr  <- lat * pi / 180
    delta <- 0.409 * sin(0.0172 * J - 1.39)
    sset  <- -tan(latr) * tan(delta)
    sset_clamped <- pmin(1, pmax(-1, sset))
    omegas       <- acos(sset_clamped)
    N <- 24 / pi * omegas
  } else {
    N <- rep(NA_real_, n)
  }  # Extraterrestrial radiation Ra (MJ m-2 d-1)
  if (all(is.na(Ra))) {
#    Ra <- potential_radiation(J = J, lat = lat)
    # constants
    latr <- lat * pi/180
    sinlat <- sin(latr)
    coslat <- cos(latr)
    # solar declination
    delta <- 0.409 * sin(0.0172 * J - 1.39)
    # inverse distance
    dr <- 1 + 0.033 * cos(0.0172 * J)
    # sunset hour angle using vectorized clamping:
    sset <- -tan(latr) * tan(delta)
    sset_clamped <- pmin(1, pmax(-1, sset))
    omegas <- acos(sset_clamped)
    # Ra in MJ m-2 d-1
    Ra <- 37.6 * dr *
      (omegas * sinlat * sin(delta) +
         coslat * cos(delta) * sin(omegas))
    Ra[Ra < 0] <- 0
  } else {
    if (length(Ra) != n && length(Ra) != 1) {
      stop("Length of 'Ra' must be 1 or match length of 'Tmin'.")
    }
  }  # Incoming shortwave radiation Rs (MJ m-2 d-1)
  if (all(is.na(Rs))) {
    if (!all(is.na(tsun)) && length(tsun) == n) {
      if (all(is.na(N))) {
        stop("Latitude 'lat' is required to compute daylength N when using 'tsun'.")
      }
      nN <- tsun / N
      as <- 0.25
      bs <- 0.50
      Rs <- (as + bs * nN) * Ra
    } else {
      stop("Either 'Rs' or 'tsun' (sunshine hours) must be provided to estimate Rs.")
    }
  }  # Clear-sky radiation Rso (MJ m-2 d-1)
  if (all(is.na(z))) {
    z_eff <- 840  # default elevation if not provided
  } else {
    z_eff <- z
  }
  Rso <- (0.75 + 2e-5 * z_eff) * Ra  # Net radiation Rn (MJ m-2 d-1)
  ac  <- 1.35
  bc  <- -0.35
  a1  <- 0.34
  b1  <- -0.14
  alb <- 0.23  
  Rn <- (1 - alb) * Rs -
    (ac * Rs / Rso + bc) *
    (a1 + b1 * sqrt(ea)) *
    4.903e-9 *
    ((273.15 + Tmax)^4 + (273.15 + Tmin)^4) / 2  
    Rn[Rs == 0] <- 0  # Soil heat flux G (MJ m-2 d-1), negligible at daily scale
  G <- 0  # Crop coefficients for aerodynamic term
  if (crop == "short") {
    c1 <- 900
    c2 <- 0.34
  } else { # "tall"
    c1 <- 1600
    c2 <- 0.38
  }  # Daily reference ET0 (mm d-1), FAO-56 Penman-Monteith
  ET0 <- (0.408 * Delta * (Rn - G) +
            gamma * (c1 / (Tmean + 273)) * U2 * (es - ea)) /
    (Delta + gamma * (1 + c2 * U2))  
  return(ET0)
}
