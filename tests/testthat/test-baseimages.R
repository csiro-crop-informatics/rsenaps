context("Base images")
library(RSenaps)
library(webmockr)

Sys.setenv(SENAPS_APIKEY = "thisismykey")

senaps_options(apikey = Sys.getenv('SENAPS_APIKEY'))
webmockr::enable(adapter = "httr")

webmockr::stub_registry_clear()

# make a stub
stub_request('get', uri = 'https://sensor-cloud.io/api/analysis/base-images?apikey=thisismykey') %>%
    wi_th(
        headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
    ) %>%
    to_return(body = list(
        `_embedded` = list(baseimages = "stuff")
    ),
    status = 200,
    headers = list(`content-type` = "application/json"))

# make request

z <- RSenaps::get_baseimages()

# run tests
test_that("http response is as expected", {
    expect_equal(z, "stuff")
})
