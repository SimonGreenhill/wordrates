
test_that("test get_words", {
    cldfobj <- rcldf::cldf("data/nagaraja_et_al2013")
    words <- get_words(cldfobj)
    expect_equal(words, c("NAME", "NARROW", "NEAR", "NECK", "NEW", "NOSE"))
})
