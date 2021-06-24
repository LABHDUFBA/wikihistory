test_that("clean_history() works", {


  jb_100_pt <- get_history("Jair Bolsonaro", n_limit = 100, lang = "pt")

  clean_jb_100_pt <- clean_history(jb_100_pt, lang = "pt")

  expect_s3_class(clean_jb_100_pt, "data.frame")
  expect_equal(nrow(clean_jb_100_pt), 100)
  expect_equal(ncol(clean_jb_100_pt), 12)


})
