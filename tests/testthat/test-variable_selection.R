test_that("Variable selection", {

  # Without viz
  data_result <- variable_selection(data = data_pharma,
                                    viz = NULL,
                                    "Year", "product", "entities")
  data_expect <- c("Year", "product", "entities")
  expect_equal(names(data_result), data_expect)

  # With viz
  data_result <- variable_selection(data = data_pharma, viz = "map")
  expect_equal(names(data_result), "Country/Region")

  data_result <- variable_selection(data = data_pharma, viz = "map_bubbles")
  expect_equal(names(data_result), c("location.lat", "location.lon"))
})
