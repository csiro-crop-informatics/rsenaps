context("Test streams api")
library("webmockr")
library("RSenaps")

Sys.setenv(SENAPS_APIKEY = "thisismykey")

senaps_options(apikey = Sys.getenv('SENAPS_APIKEY'))
webmockr::enable(adapter = "httr")

webmockr::stub_registry_clear()

# make a stub
stub_request('get', uri = 'https://sensor-cloud.io/api/sensor/v2/streams/count?groupids=streamid&apikey=thisismykey') %>%
    wi_th(
        headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
    ) %>%
    to_return(body = list(
        count = 5
    ),
    status = 200,
    headers = list(`content-type` = "application/json"))

# stub_registry()

z <- RSenaps::count_streams("streamid")

# run tests (nothing returned means it passed)
test_that("http response is as expected", {
    expect_equal(z, 5)
})
