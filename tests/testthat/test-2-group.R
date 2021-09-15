test_that("delete non-existing group", {
    skip_on_cran()
    skip_if(NO_API_KEY_IN_ENVIRONMENT)
    group_name <- paste0(prefix, "non-exising-group-safdskdfhskfsifdaskf")
    expect_error(delete_group(group_name))
})


test_that("add group", {
    skip_on_cran()
    skip_if(NO_API_KEY_IN_ENVIRONMENT)
    # Add group
    group_name <- paste0(prefix, "group1")
    expect_true(put_group(id = group_name, name = group_name, description = group_name,
              organisation = organisation,
              groups = root_group))
    # Get group
    new_group <- get_group(group_name)
    expect_equal(new_group$id, group_name)
    expect_equal(new_group$groups, root_group)
    expect_equal(new_group$organisation, organisation)
    # Delete group
    expect_true(delete_group(group_name))
})
