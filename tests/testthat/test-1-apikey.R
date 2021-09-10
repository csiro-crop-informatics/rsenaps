test_that("error on unnamed arguments", {
    expect_error(senaps_options("A"))
})

test_that("error on empty api key", {
    expect_error(senaps_options(apikey = ""))
})

test_that("set api key", {
    skip_on_cran()
    skip_if(NO_API_KEY_IN_ENVIRONMENT)
    old_options <- senaps_options()
    expect_equal(old_options$apikey, "")
    senaps_options(apikey = RSENAPS_TEST_API_KEY)
    new_options <- senaps_options()
    expect_equal(new_options$apikey, RSENAPS_TEST_API_KEY)
})
