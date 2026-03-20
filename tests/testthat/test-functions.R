# Testing function clean_herring.
test_that("clean_herring returns expected columns", {
  raw <- herring_read()
  clean <- cleaning_herring(raw)

  expect_true("year" %in% names(clean))
  expect_true("age"  %in% names(clean))
  expect_true("length" %in% names(clean))
  expect_true(nrow(clean) > 0)
})

# Testing function count_per_year.
test_that("count_per_year works", {
  count_clean <- tibble::tibble(
    year  = c(2000, 2000, 2000, 2001),
    id    = c(1,    1,    2,    3),
    indno = c(1,    1,    1,    1)
  )
  count_out <- count_per_year(count_clean)
  expect_equal(count_out$n_ids[count_out$year == 2000], 2)
  expect_equal(count_out$n_ids[count_out$year == 2001], 1)
})

# Testing function weight_per_year.
test_that("weight_per_year works", {
  weight_clean <- tibble::tibble(year = c(2000, 2001, 2002),
                                 weight = c(100, 200, 250)
  )
  weight_out <- weight_per_year(weight_clean)
  expect_equal(weight_out$total_weight[weight_out$year == 2001], 200/1e6)
})

# Testing function age_count_for_year.
test_that("age_count_for_year works", {
  age_clean <- tibble::tibble(
    year = c(2000, 2001, 2001, 2002),
    age  = c(1,    2,    2,    3)
  )
  age_out <- age_count_for_year(age_clean)
  expect_equal(age_out$n[age_out$age == 2], 2)
})

# Testing function age_summary_for_year.
test_that("age_summary_for_year works", {
  age_clean <- tibble::tibble(
    year = c(2000, 2001, 2001, 2002),
    age  = c(1,    2,    2,    3)
  )
  age_out <- age_summary_for_year(age_clean)
  expect_equal(age_out$year,     c(2000, 2001, 2002))
  expect_equal(age_out$n_fish,   c(1, 2, 1))
  expect_equal(age_out$mean_age, c(1, 2, 3))
  expect_equal(age_out$max_age,  c(1, 2, 3))
})

# Testing function location_catches_summary.
test_that("location_catches_summary works", {
  location_clean <- tibble::tibble(
    year =   c(2000, 2000, 2001, 2001, 2002),
    month =  c(1,    1,    2,    2,    11  ),
    lon =    c(20,   20,   22,  -40,   10  ), # lon -40 is outside range (30>lon>-30).
    lat =    c(77,   77,   70,   64,   22  ), # lat 22 is outside range (80>lat>30).
    age  =   c(1,    3,    2,    3,    2   ),
    weight = c(200,  300,  200,  300,  200 )
  )
  location_out <- location_catches_summary(location_clean)
  expected_out <- tibble::tibble(
    year        =  c(2000, 2001),
    month       =  c(1,    2   ),
    lon         =  c(20,   22  ),
    lat         =  c(77,   70  ),
    n_fish      =  c(2,    1   ),
    mean_age    =  c(2,    2   ),
    mean_weight =  c(250,  200 )
  )
  expect_equal(location_out, expected_out)
})
