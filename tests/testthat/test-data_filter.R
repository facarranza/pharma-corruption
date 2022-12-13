test_that("filter data", {

  # filter one variable
  test_list <- list("id_country_region" = c("Burundi"))
  data_result <- data_filter(data = data_pharma,
                             dic = dic_pharma,
                             var_inputs = test_list)
  data_expect <- data_pharma |>
    dplyr::filter(`Country/Region` %in% "Burundi")
  expect_equal(data_result, data_expect)


  # filter categorical and date column
  test_list <- list("id_country_region" = c("United States", "Mexico", "Brazil"),
                    "id_published_at" = c("2021-10-26", "2022-10-29"))
  data_result <- data_filter(data = data_pharma,
                             dic = dic_pharma,
                             var_inputs = test_list)
  data_expect <- data_pharma |>
    dplyr::filter(`Country/Region` %in% c("United States", "Mexico", "Brazil"),
                  `Published-at` >= "2021-10-26" &
                  `Published-at` <= "2022-10-29")
  expect_equal(data_result, data_expect)

})


