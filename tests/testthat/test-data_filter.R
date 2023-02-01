test_that("filter data", {

  # filter one variable
  test_list <- list("id_country_region" = c("Burundi"))
  data_result <- data_filter(data = data_pharma,
                             dic = dic_pharma,
                             var_inputs = test_list)
  data_expect <- data_pharma |>
    dplyr::filter(`Country / region` %in% "Burundi")
  expect_equal(data_result, data_expect)


  # filter categorical and date column
  test_list <- list("id_country_region" = c("United States", "Mexico", "Brazil"),
                    "id_published_at" = c("2021-10-26", "2022-10-29"))
  data_result <- data_filter(data = data_pharma,
                             dic = dic_pharma,
                             var_inputs = test_list)
  data_expect <- data_pharma |>
    dplyr::filter(`Country / region` %in% c("United States", "Mexico", "Brazil"),
                  `Published-at` >= "2021-10-26" &
                  `Published-at` <= "2022-10-29")
  expect_equal(data_result, data_expect)

  # filter data with multiples categorica values in rows
  hope_health <- c("Health Technology", "Medical/Clinical Trials")
  hope_corruption <- "Corporate Crime"
  test_list <- list("id_country_region" = c("United States", "Mexico", "Brazil"),
                    "id_health_categories" = hope_health,
                    "id_corruption_categories" = hope_corruption)
  data_result <- data_filter(data = data_pharma,
                             dic = dic_pharma,
                             var_inputs = test_list,
                             .id = "story-id")
  result_health <- strsplit(data_result$`Health categories`, split = ",") |>
    unlist() |>
    unique()

  expect_equal(result_health, hope_health)
  expect_equal(unique(data_result$`Corruption categories`), hope_corruption)

  # filter numeric var
  test_list <- list("id_location_lat" = 43.16,
                    "id_location_lon" = -4.09)
  data_result <- data_filter(data = data_pharma,
                             dic = dic_pharma,
                             var_inputs = test_list)
  expect_equal(unique(data_result$location.lat), 43.16)
})


