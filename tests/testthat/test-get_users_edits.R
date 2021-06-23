test_that("get_users works", {

  jb_100_pt <- get_history("Jair Bolsonaro", n_limit = 100, lang = "pt")

  users_jb <- get_users_edits(jb_100_pt)

  expect_type(users_jb, "character")

})
