
test_that("test utils :: is.constant", {
    expect_equal(is.constant(list('L1' = c("A"), 'L2' = c("A"))), TRUE)
    expect_equal(is.constant(list('L1' = c("A"), 'L2' = c("-"))), TRUE)
    expect_equal(is.constant(list('L1' = c("A"), 'L2' = c("B"))), FALSE)
})


