test_that("Viz selection", {
  data_viz <- variable_selection(data = data_pharma, viz = "map")
  data_viz  <- var_aggregation(data_viz, dic_pharma, Total = dplyr::n())
  data_result <- viz_selection(data_viz, dic_pharma, "map")
  expect_equal(data_result, "lfltmagic::lflt_choropleth_GnmNum")

  data_viz <- variable_selection(data = data_pharma, viz = "bar")
  data_viz  <- var_aggregation(data_viz, dic_pharma, Total = dplyr::n())
  data_result <- viz_selection(data_viz, dic_pharma, "bar")
  expect_equal(data_result, "hgchmagic::hgch_bar_CatNum")

  data_viz <- variable_selection(data = data_pharma, viz = "line")
  data_viz  <- var_aggregation(data_viz, dic_pharma, Total = dplyr::n())
  data_result <- viz_selection(data_viz, dic_pharma, "line")
  expect_equal(data_result, "hgchmagic::hgch_line_CatYeaNum")

})
