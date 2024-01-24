
test_that("test get_words", {
    cldfobj <- rcldf::cldf("data")
    words <- get_words(cldfobj)
    expect_equal(words, c("NAME", "NARROW", "NEAR", "NECK", "NEW", "NOSE"))
})
