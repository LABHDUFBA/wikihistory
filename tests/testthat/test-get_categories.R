test_that("get_categories works", {

  jb_categories <- get_categories("Jair Bolsonaro", "pt")



  expect_s3_class(jb_categories, "data.frame")
  expect_equal(nrow(jb_categories), 26)
  expect_equal(ncol(jb_categories), 3)

})
