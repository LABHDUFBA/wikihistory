test_that("user_edits works", {

  jb_users <- get_history("Jair Bolsonaro", n_limit = 100, lang = "pt") %>%
  get_users_edits()

  jb_user_edits <- user_edits(jb_users[1], lang = "pt")

  expect_s3_class(jb_user_edits, "data.frame")
  expect_equal(nrow(jb_user_edits), 1000)
  expect_equal(ncol(jb_user_edits), 3)



})

test_that("get_all_edits works", {

   jb_all_edits <- get_history("Jair Bolsonaro", n_limit = 10, lang = "pt") %>%
    get_users_edits() %>%
    get_all_edits()



  expect_s3_class(jb_all_edits, "data.frame")
  expect_gt(nrow(jb_all_edits), 1000)
  expect_equal(ncol(jb_all_edits), 3)



})


