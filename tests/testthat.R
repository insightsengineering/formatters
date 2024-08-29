library(testthat)
library(formatters)
require("dplyr", quietly = TRUE)
test_check("formatters", reporter = "check")
