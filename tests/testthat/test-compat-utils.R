test_that("filter dates", {

  #
  data_result <- filter_dates(data = data_pharma,
              range_date = c("2022-08-08", "2022-11-13"),
              by = "Published-at"
              )
  data_expect <- data_pharma[data.table::between(data_pharma$`Published-at`,
                                                 "2022-08-08", "2022-11-13"),]

  expect_equal(data_result$`Published-at`, data_expect$`Published-at`)

  # not change dates to filter
  min_date <- min(data_pharma$`Published-at`)
  max_date <- max(data_pharma$`Published-at`)
  data_result <- filter_dates(data = data_pharma,
                              range_date = c(min_date, max_date),
                              by = "Published-at"
  )
  expect_equal(data_result, data_pharma)

  # one dates to filter
  data_result <- filter_dates(data = data_pharma,
                              range_date = c("2022-11-13"),
                              by = "Published-at"
  )

  expect_equal(as.character(unique(data_result$`Published-at`)), "2022-11-13")


})


test_that("filter variables with multiple cats in same rows", {

  var_expect <- c("Health Technology", "Medical/Clinical Trials")
  data_result <- filter_list(data = data_pharma,
                             cats = var_expect,
                              by = "Health categories",
                             .id = "story-id")
  result <- strsplit(data_result$`Health categories`, split = ",") |>
    unlist() |>
    unique()
  expect_equal(result, var_expect)

})
