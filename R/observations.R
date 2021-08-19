

#' Get a collection of observations
#'
#' @param streamid Stream identifier or a comma separated list of stream identifiers
#' @param start Start date
#' @param end End date
#' @param si Is the start parameter treated as an inclusive boundary
#' @param ei Is the end parameter treated as an inclusive boundary
#' @param tz The timezone of timestamp. GMT/UTC time in default
#' @param descending Sort the results. By default results are returned in ascending order.
#' @param limit Limit the number of results. The limit is 1000 by default.
#' @export
get_observations <- function(streamid,
                             start = NULL,
                             end = NULL,
                             si = TRUE,
                             ei = TRUE,
                             tz = "GMT",
                             descending = FALSE,
                             limit = 1000) {
    # Check the length of streamid
    if (length(streamid) > 1) {
        stop('Only 1 stream is supported.')
    }


    query <- list(streamid = streamid, limit = sprintf('%d', limit))

    if (descending) {
        query$sort <- 'descending'
    }

    query$si <- TRUE
    query$ei <- TRUE

    # Check the start and end class
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
            req <- request(GET, 'observations', query = query)
            # print(query$start)
            httr::stop_for_status(req)
            response <- httr::content(req)
            res <- response$results %>%
                map_df(function(x) {
                    tibble::tibble(timestamp = x$t, value = x$v$v)
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
            if (nrow(res) <= query$limit) {
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


#' Add new observations into a stream
#'
#' @param id The stream id
#' @param timestamp The timestamp vector of observations
#' @param value The observed values
#'
#' @export
put_observations <- function(id, timestamp, value) {

    if (class(id) != 'character' | length(id) > 1) {
        stop('Stream id should be character element')
    }
    if (length(timestamp) != length(value)) {
        stop('timestamp and value should have the same length.')
    }
    if (sum(is.na(timestamp)) > 0 | sum(is.na(value)) > 0) {
        warning('NA values in the timestamp and/or value. NA will be ignored')
    }

    if (sum(class(timestamp) %in% c('POSIXct')) == 0) {
        stop('POSIXct class is requried for timestamp')
    }
    df <- data.frame(t = format(timestamp, format="%Y-%m-%dT%H:%M:%OS3%z"),
                     v = value)
    df <- na.omit(df)
    if (nrow(df) == 0) {
        warning('No observtions')
        return(NULL)
    }
    df <- mapply(function(t, v) list(t=t, v=list(v=v)), df[[1]], df[[2]], SIMPLIFY=
                     FALSE)
    names(df) <- NULL

    json <- jsonlite::toJSON(list(results = df), auto_unbox = TRUE)
    response <- request(httr::POST, path = 'observations',
                        query = list(streamid = id),
                        body = json,
                        encode = 'json')
    status <- httr::status_code(response)

    if (status != 201) {
        stop(httr::http_status(response)$message)
    }

}


#' Delete all observations in a stream.
#'
#' @param id The stream id
#' @return The status code of request
#' @export
delete_observations <- function(id) {

    response <- request(DELETE, path = 'observations',
                        query = list(streamid = id))
    status <- status_code(response)
    status
}
