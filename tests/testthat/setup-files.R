RSENAPS_TEST_API_KEY = Sys.getenv("RSENAPS_TEST_API_KEY")

NO_API_KEY_IN_ENVIRONMENT = RSENAPS_TEST_API_KEY == ""
digits <- 0:9
createRandString <- function() {
    v = c(sample(letters, 5, replace = TRUE),
          sample(digits, 4, replace = TRUE),
          sample(letters, 1, replace = TRUE))
    return(paste0(v,collapse = ""))
}

prefix <- paste0("rsenaps-test-", createRandString(), "-")

root_group <- "rsenaps-test"
organisation <- "csiro"

