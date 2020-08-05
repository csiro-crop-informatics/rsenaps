context("Local execution")
library(RSenaps)

Sys.setenv("SENAPS_LOCAL"='')

test_that("missing env var returns FALSE", {
    expect_false(local_senaps())
    expect_message(local_senaps(), "There is no SENAPS_LOCAL environment variable set")
})

Sys.setenv("SENAPS_LOCAL"=FALSE)

test_that("false env var returns FALSE", {
    expect_false(local_senaps())
    expect_message(local_senaps(), "SENAPS_LOCAL environment variable is set to FALSE")
})

test_that("seting env var to TRUE returns TRUE", {
    expect_message(local_senaps(set_local = TRUE), "SENAPS_LOCAL environment variable has been set to TRUE")
    expect_true(local_senaps(set_local = TRUE))
})

test_that("set env var returns TRUE", {
    expect_true(local_senaps())
})
