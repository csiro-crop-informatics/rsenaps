context("No apikey")
library(RSenaps)
library(webmockr)

Sys.setenv(SENAPS_APIKEY = "")

senaps_options(apikey = Sys.getenv('SENAPS_APIKEY'))

expect_error(get_baseimages(), "apikey or username/password should be specified")
