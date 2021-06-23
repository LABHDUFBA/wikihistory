test_that("user_edits works", {

  jb_users <- get_history("Jair Bolsonaro", n_limit = 100, lang = "pt") %>%
  get_users_edits()

  jb_user_edits <- user_edits(jb_users[1], lang = "pt")

  expect_s3_class(jb_user_edits, "data.frame")
  expect_equal(nrow(jb_user_edits), 1000)
  expect_equal(ncol(jb_user_edits), 3)



})
