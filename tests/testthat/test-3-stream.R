test_that("delete non-existing stream", {
    skip_on_cran()
    skip_if(NO_API_KEY_IN_ENVIRONMENT)
    stream_name <- paste0(prefix, "non-exising-stream-safdskdfhskfsifdaskf")
    expect_error(delete_stream(stream_name))
})


test_that("add stream", {
    skip_on_cran()
    skip_if(NO_API_KEY_IN_ENVIRONMENT)
    # Add stream in root group
    stream_name <- paste0(prefix, "stream1")
    expect_true(put_stream(id = stream_name,
                           reportingPeriod = 'PT5M',
                           samplePeriod = 'PT5M',
                           observedProperty = 'http://registry.it.csiro.au/def/environment/property/air_temperature',
                           unitOfMeasure = 'http://registry.it.csiro.au/def/qudt/1.1/qudt-unit/DegreeCelsius',
                           organisation = organisation,
                           groups = root_group))
    # Get stream
    new_stream <- get_stream(stream_name)
    expect_equal(new_stream$id, stream_name)
    expect_equal(new_stream$groupids, root_group)
    expect_equal(new_stream$organisation, organisation)
    # Delete stream
    expect_true(delete_stream(stream_name))
})
