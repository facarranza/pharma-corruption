test_that("Viz selection", {
  data_viz <- variable_selection(data = data_pharma, viz = "map")
  data_result <- viz_selection(data_viz, dic_pharma, "map")
  expect_equal(data_result, "lfltmagic::lflt_choropleth_GnmNum")
})
