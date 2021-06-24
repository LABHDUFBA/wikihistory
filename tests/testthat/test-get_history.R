test_that("multiplication works", {
  jb_100_pt <- get_history("Jair Bolsonaro", n_limit = 100, lang = "pt")

  expect_s3_class(jb_100_pt, "data.frame")
  expect_equal(nrow(jb_100_pt), 100)
  expect_equal(ncol(jb_100_pt), 9)

  # ESCREVER MAIS TESTES!!
  })
