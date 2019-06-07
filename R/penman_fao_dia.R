
# Ej. en penman_fao_period en /mnt/dostb2/fuendetodos/DATOS/repositorio/spei/trunk/fergus/datos/code_web_maps/penman_fao_raster.R

#' FAO-56 Penman-Monteith reference evapotranspiration (ET_0)
#' 
#' @param Tmin minimum temperature, Celsius
#' @param Tmax maximum temperature, Celsius
#' @param U2 average wind, m/s at 2m
#' @param J day of the year
#' @param Ra radiation, W/m2
#' @param lat latitude, degrees, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')
#' @param Rs daily incoming solar radiation (J m-2 d-1)
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
    T <- (Tmin+Tmax)/2
    
    # 1. Latent heat of vaporization, lambda (eq. 1.1)
    lambda <- 2.501 - 2.361e-3*T
        
    # 3. Psychrometric constant, gamma (eq. 1.4)
    # 4. P: atmospheric pressure, kPa
    # if(is.na(P0)){ P0  <- matrix(101.3,nrow=nrow(T),ncol=ncol(T)) }
    if(is.na(P0)){ 
        dim = dim(T)
        if(is.null(dim)){
            dim = length(T) 
        }
        P0  <- array(101.3, dim=dim) 
    }
    if(is.na(P)){ 
        P <- P0*(((293-0.0065*z)/293)^5.26) 
    }
        
    ## FAO
    gamma <- 0.665e-3*P
        
    # 6. Saturation vapour pressure, ea
    # saturation vapour pressure at tmx (eq. 1.10, p. 66)
    etmx <- 0.611*exp((17.27*Tmax)/(Tmax+237.3))
    # saturation vapour pressure at tmn (eq. 1.10, p. 66)
    etmn <- 0.611*exp((17.27*Tmin)/(Tmin+237.3))
    # mean saturation vapour pressure (eq. 1.11, p. 67)
    ea <- (etmx+etmn)/2
    
    ## We need et when we use FAO-PM
    et <- 0.611*exp((17.27*T)/(T+237.3))
    
    # 2. Slope of the saturation vapour pressure function, Delta (eq. 1.3)
    
    #Allen, 1994
    ## Delta <- 4099*ea/(T+237.3)^2
    
    # FAO
    Delta <- 4099*et/(T+237.3)^2
    #Delta <- 2504*exp((12.27*T)/(T+237.3))/(T+237.3)^2
    
    # 7. Actual vapour pressure, ed
    if(length(ed)!=n | sum(!is.na(ed))==0) {
      if (length(Tdew)==n) {
        # (eq. 1.12, p. 67)
        ed <- 0.611*exp((17.27*Tdew)/(Tdew+237.3))
      } else if(length(RH)==n) {
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
    ww <- which(ea-ed<0)
    if (length(ww)>0){
      ed[ww] <- ea[ww]
    }
    
    ### FINS AQUI FUNCIONA ###    
 
    # delta: solar declination, rad (1 rad = 57.2957795 deg) (eq. 1.25)
    delta <- 0.409*sin(0.0172*J-1.39)
    
    # dr: relative distance Earth-Sun, [] (eq. 1.24)
    dr <- 1 + 0.033*cos(0.0172*J)
    
    # omegas: sunset hour angle, rad (eq. 1.23)
    latr <- lat/57.2957795
      
    ### FINS AQUI FUNCIONA ###
    sset <- Tmin
    sset <- -tan(latr)*tan(delta)
    
    omegas <- sset*0
    omegas[sset>={-1} & sset<=1] <- acos(sset[sset>={-1} & sset<=1])
    # correction for high latitudes
    omegas[sset<{-1}] <- max(omegas)    
    
    # 9. Extraterrestrial radiation, Ra (MJ m-2 d-1)
    
    # Estimate Ra (eq. 1.22)
    if(sum(!is.na(Ra))==0){    
        Ra <- 37.6*dr*(omegas*sin(latr)*sin(delta)+cos(latr)*cos(delta)*sin(omegas))
    }
    ##Ra <- ifelse(values(Ra)<0,0,Ra)
    
    # 11. Net radiation, Rn (MJ m-2 d-1)
    # Net radiation is the sum of net short wave radiation Rns and net long wave
    # (incoming) radiation (Rnl).
    # Rs: daily incoming solar radiation (MJ m-2 d-1)
    
    # nN: relative sunshine fraction []  
    if(sum(!is.na(Rs))==0){
      if (length(tsun)==n) {
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
      Rs <- (as+bs*(nN))*Ra
    }

    # Rso: clear-sky solar radiation (eq. 1.40)
    # Note: mostly valid for z<6000 m and low air turbidity
    #if (ncol(as.matrix(z))==ncol(as.matrix(Tmin))) {
    if (exists('z') & sum(!is.na(z))>0) {
      Rso <- (0.75+2e-5*z)* Ra
    } else {
      Rso <- (0.75+2e-5*840) * Ra
    }
    # Empirical constants
    ac <- 1.35; bc <- -0.35; a1 <- 0.34; b1 <- -0.14
    # Reference crop albedo
    alb <- 0.23
    # Rn, MJ m-2 d-1 (eq. 1.53)
    Rn <- (1-alb)*Rs - (ac*Rs/Rso+bc) * (a1+b1*sqrt(ed)) * 4.9e-9 * ((273.15+Tmax)^4+(273.15+Tmin)^4)/2	
    Rn[Rs==0] <- 0
    
    # Soil heat flux density, G
    # Using forward / backward differences for the first and last observations,
    # and central differences for the remaining ones.
    G <- 0
    # Wind speed at 2m, U2 (eq. 1.62)
    #U2 <- U2 * 4.85203/log((zz-0.08)/0.015)
    
    # Daily ET0 (eq. 2.18)
    if (crop=='short') {
      c1 <- 900; c2 <- 0.34 # short reference crop (e.g. clipped grass, 0.12 m)
    } else {
      c1 <- 1600; c2 <- 0.38 # tall reference crop (e.g. alfalfa, 0.5 m)
    }
    ET0 <- (0.408*Delta*(Rn-G) + gamma*(c1/(T+273))*U2*(ea-ed)) /
      (Delta + gamma*(1+c2*U2))
    
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
    Rs[Rs<0] = 0
    return(Rs)
}
