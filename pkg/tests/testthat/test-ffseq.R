test_that("ffseq works", {
  i <- seq_len(10)
  fi <- ffseq_len(10)
  expect_equal(i, fi[])
})
