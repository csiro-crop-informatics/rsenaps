
#' Auto unbox a list
#'
#' @param x the list
#'
#' @return a new list with unbox
#' @export
unbox_list <- function(x) {
    for (i in seq(along = x)) {
        if (class(x[[i]]) == 'list') {
            x[[i]] <- unbox_list(x[[i]])
        } else if (length(x[[i]]) == 1) {
            x[[i]] <- jsonlite::unbox(x[[i]])
        }
    }
    x
}

#' The time for sunrise and sunset
#' from 90 degree in am and pm. +ve above the horizon, -ve below the horizon.
#' @param  doy day of year number
#' @param lat latitude of site (deg)
#' @param long longitude of site (deg)
#' @param timezone local time
#' @param  angle angle to measure time between, such as twilight (deg).
#' angular distance between 90 deg and end of twilight - altitude of sun. +ve up, -ve down.
#' @param flag rise or set
#' @export
sunRiseSet <- function(doy, lat, long, timezone = 0, angle = 6, flag = 'rise')
{
    doy_len <- length(doy)
    lat <- rep(lat, length.out = doy_len)
    long <- rep(long, length.out = doy_len)
    dg2rdn <- pi / 180
    rdn2dg <- 180 / pi
    lngHour <- long / 15
    if (flag == 'rise')
    {
        tt <- doy + ((6 - lngHour) / 24)
    } else
    {
        tt <- doy + ((18 - lngHour) / 24)
    }
    MM <- (0.9856 * tt) - 3.289
    LL <- MM + (1.916 * sin(MM * dg2rdn)) + (0.020 * sin(2 * MM * dg2rdn)) + 282.634
    LL <- (LL + 360 * 100) %% 360
    RA <- rdn2dg * atan(0.91764 * tan(LL * dg2rdn))
    RA <- (RA + 360 * 100) %% 360
    Lquadrant <- (floor(LL/90)) * 90
    RAquadrant <- (floor(RA/90)) * 90
    RA <- RA + (Lquadrant - RAquadrant)
    RA <- RA / 15
    sinDec <- 0.39782 * sin(LL * dg2rdn)
    cosDec <- cos(asin(sinDec))
    cosH <- (cos((angle + 90)* dg2rdn) - (sinDec * sin(lat * dg2rdn))) / (cosDec * cos(lat * dg2rdn))

    if (sum(cosH > 1) > 0)
    {
        stop('the sun never rises on this location (on the specified date)')
    }
    if (sum(cosH < -1) > 0)
    {
        stop('the sun never sets on this location (on the specified date)')
    }

    if (flag == 'rise')
    {
        HH <- 360 - rdn2dg * acos(cosH)
    } else
    {
        HH <- rdn2dg * acos(cosH)
    }
    HH <- HH / 15
    TT <- HH + RA - (0.06571 * tt) - 6.622
    UT <- TT - lngHour
    # UT <- (UT + 24 * 100) %% 24
    localT <- UT + timezone
    localT <- (localT + 24 * 100) %% 24

    return(localT)
}
