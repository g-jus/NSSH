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

test_that("weight_per_year works", {
  weight_clean <- tibble::tibble(year = c(2000, 2001, 2002),
                                 weight = c(100, 200, 250))
  weight_out <- weight_per_year(weight_clean)
  expect_equal(weight_out$total_weight[weight_out$year == 2001], 200/1e6)
})

test_that("age_count_for_year works", {
  age_clean <- tibble::tibble(
    year = c(2000, 2001, 2001, 2002),
    age  = c(1,    2,    2,    3)
  )
  age_out <- age_count_for_year(age_clean)
  expect_equal(age_out$n[age_out$age == 2], 2)
})
