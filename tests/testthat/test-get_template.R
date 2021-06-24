test_that("get_template() works", {
  mc_template <- get_template("Mudança do clima", lang = "pt")


  expect_s3_class(mc_template, "data.frame")
  expect_gt(nrow(mc_template), 75)
  expect_equal(ncol(mc_template), 3)

  expect_error(get_template("Climate_change", lang = "en"),
               "This language is not implemented yet.")



})
