library(testthat)
library(RSenaps)

Sys.setenv(SENAPS_APIKEY = "thisismykey")

test_check("RSenaps")
