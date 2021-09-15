


#' The all modelsin the Senaps
#'
#' @return A list of models
#' @export
get_models <- function() {

    response <- request(GET, 'models')
    httr::stop_for_status(response)
    response <- httr::content(response)

    models <- response$`_embedded`$models
    models
}



#' The model detail information in the Senaps
#'
#' @param id A model id
#'
#' @return A list of model meta data
#' @export
get_model <- function(id) {
    response <- request(GET, paste0('models/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}


#' Upload a model into Senaps
#'
#' @param archive The file path to a model archive with file type zip or tar.gz
#'
#' @return TRUE if model is uploaded
#' @export
put_model <- function(archive) {
    body <- list(archive = upload_file(archive))
    response <- request(POST, 'models', body = body)
    httr::stop_for_status(response)
    TRUE
}