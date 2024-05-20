test_that("reorder_ref_fnotes orders referential footnotes correctly", {
  # all numeric
  rf <- c("{4} - test 1", "{1} - one", "{11} - eleven", "{100} - test 2", "{3} - three", "{7} - !!")
  res <- reorder_ref_fnotes(rf)

  expect_identical(
    res,
    c("{1} - one", "{3} - three", "{4} - test 1", "{7} - !!", "{11} - eleven", "{100} - test 2")
  )

  # numeric and character
  rf <- c("{*} - test 1", "{1} - one", "{11} - eleven", "{**} - test 2", "{3} - three", "{!} - !!")
  res <- reorder_ref_fnotes(rf)

  expect_identical(
    res,
    c("{1} - one", "{3} - three", "{11} - eleven", "{!} - !!", "{*} - test 1", "{**} - test 2")
  )

  # with asterisks
  rf <- c("{*} - test 1", "{1} - one", "** eleven", "{**} - test 2", "* three", "{!} - !!")
  res <- reorder_ref_fnotes(rf)

  expect_identical(
    res,
    c("{1} - one", "{!} - !!", "{*} - test 1", "{**} - test 2", "* three", "** eleven")
  )
})
