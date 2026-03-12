test_that("clean_herring returns expected columns", {
  raw <- herring_read()
  clean <- cleaning_herring(raw)

  expect_true("year" %in% names(clean))
  expect_true("age"  %in% names(clean))
  expect_true("length" %in% names(clean))
  expect_true(nrow(clean) > 0)
})
