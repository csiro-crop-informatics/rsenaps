


#' The all base images in the Senaps
#'
#' @return A list of base images
#' @export
get_baseimages <- function() {

    response <- request(GET, 'base-images')
    httr::stop_for_status(response)
    response <- httr::content(response)

    baseimages <- response$`_embedded`$baseimages
    baseimages
}



#' The base image in the Senaps
#'
#' @param id A baseimage id
#'
#' @return A list for base image
#' @export
get_baseimage <- function(id) {

    response <- request(GET, paste0('base-images/', id))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response
}
