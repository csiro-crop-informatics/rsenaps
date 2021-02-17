

#' Calculate an aggregated view of observations. The aggregation is calculated on demand.
#'
#' @param streamid Stream identifier or a comma separated list of stream identifiers
#' @param aggperiod The number of milliseconds in each aggregation interval
#' @param start Start date
#' @param end End date
#' @param si Is the start parameter treated as an inclusive boundary
#' @param ei Is the end parameter treated as an inclusive boundary
#' @param tz The timezone of timestamp. GMT/UTC time in default
#' @param limit Limit the number of results. The limit is 1000 by default.
#' @export
get_aggregation <- function(streamid,
                            aggperiod,
                             start = NULL,
                             end = NULL,
                             si = TRUE,
                             ei = TRUE,
                             tz = "GMT",
                             limit = 1000) {
    # Check the length of streamid
    if (length(streamid) > 1) {
        stop('Only 1 stream is supported.')
    }

    # Check the length of streamid
    if (length(aggperiod) > 1) {
        stop('Only 1 aggperiod is supported.')
    }

    if (!is.numeric(aggperiod)) {
        stop("aggperiod requries a numeric number")
    }

    query <- list(streamid = streamid,
                  aggperiod = sprintf('%d', aggperiod),
                  limit = sprintf('%d', limit))

    query$si <- TRUE
    query$ei <- TRUE

    # Check the start and end class
    # Assign enough range for start and end as aggregation require both values
    if (is.null(start)) {
        start <- as.Date("1900-01-01")
    }

    if (is.null(end)) {
        end <- as.Date("9999-01-01")
    }
    if (!is.null(start)) {
        if (sum(class(start) %in% c('POSIXct', "POSIXt", "POSIXlt", "Date")) == 0) {
            stop('The "start" should date/time class.')
        }
        if ("Date" %in% class(end) & !si) {
            start <- start + 1
        }
        query$start <- format_datetime(start, tz = tz)
        if (!si) {
            query$si <- FALSE
        }
    }
    if (!is.null(end)) {

        if (sum(class(end) %in% c('POSIXct', "POSIXt", "POSIXlt", "Date")) == 0) {
            stop('The "end" should date/time class.')
        }
        # For Date, add an extra day
        if ("Date" %in% class(end) & ei) {
            end <- end + 1
        }
        query$end <- format_datetime(end, tz = tz)
        if (!ei) {
            query$ei <- FALSE
        }
    }
    result <- list()
    k <- 0
    repeat {
        tryCatch({
            req <- request(GET, 'aggregation', query = query)
            # print(query$start)
            httr::stop_for_status(req)
            response <- httr::content(req)
            res <- response$results %>%
                map_df(function(x) {
                    data_frame(timestamp = x$t,
                               avg = x$v$avg,
                               min = x$v$min,
                               max = x$v$max,
                               count = x$v$count)
                })
            rm(req)
            rm(response)
            if (nrow(res) == 0) {
                break
            }
            res$timestamp <- lubridate::with_tz(
                as.POSIXct(res$timestamp,
                           format = "%Y-%m-%dT%H:%M:%OS", tz = 'GMT'),
                tz = tz)
            k <- k + 1
            result[[k]] <- res
            if (nrow(res) < query$limit) {
                break
            } else {
                query$start <- format_datetime(res$timestamp[nrow(res)], tz = tz)
                query$si <- FALSE
            }
        }, error = function(e) {
            stop(e)
        })

    }
    bind_rows(result)



}

