test_that("get_categories works", {

  jb_categories <- get_categories("Jair Bolsonaro", "pt")



  expect_s3_class(jb_categories, "data.frame")
  expect_equal(nrow(jb_categories), 26)
  expect_equal(ncol(jb_categories), 3)

})




test_that("get_pages_in_categories works", {

  mc_pages <- get_pages_in_categories("Mudanças climáticas", "pt")

  expect_s3_class(mc_pages, "data.frame")
  expect_gt(nrow(mc_pages), 50)
  expect_equal(ncol(mc_pages), 3)

})
