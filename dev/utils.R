# create data file
usethis::use_data_raw()

# add test to generic functions
usethis::use_test("compt-utils")

# add test to data filters
usethis::use_test("data_filter")
# add test to var select
usethis::use_test("variable_selection")
# add test to var_aggregation
usethis::use_test("var_aggregation")
# add test to viz select
usethis::use_test("viz_selection")
# add test click info
usethis::use_test("write_html")


# build
devtools::load_all()
devtools::document()
devtools::install()

# check package
devtools::check()

# add packages in description
attachment::att_amend_desc()

