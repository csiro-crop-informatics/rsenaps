library("webmockr")
library("crul")
library("testthat")
library("RSenaps")

senaps_options(apikey = Sys.getenv('SENAPS_APIKEY'))
webmockr::enable(adapter = "httr")

webmockr::stub_registry_clear()

# make a stub
stub_request('get', uri = 'https://sensor-cloud.io/api/sensor/v2/streams/count?groupids=streamid&apikey=thisismykey') %>%
    wi_th(
        headers = list('Accept' = 'application/json, text/xml, application/xml, */*')
    )

# stub_registry()

z <- RSenaps::count_streams("streamid")

# run tests (nothing returned means it passed)
expect_is(z, "HttpResponse")
expect_equal(z$status_code, 200)
expect_equal(z$parse("UTF-8"), "success!")
