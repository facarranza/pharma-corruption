test_that("Aggregation", {

  data_viz <- variable_selection(data = data_pharma, viz = "map")
  data_result <- var_aggregation(data_viz, Total = dplyr::n())
  data_expect <- data_pharma |>
    dplyr::group_by(`Country/Region`) |>
    dplyr::summarise(Total = dplyr::n())
  expect_equal(data_result, data_expect)
})
