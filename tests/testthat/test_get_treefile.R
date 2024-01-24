
test_that("test get_treefile", {
    cldfobj <- read.phlorest("data")
    p <- get_treefile(cldfobj, 'posterior')
    s <- get_treefile(cldfobj, 'summary')
    x <- get_treefile(cldfobj, 'whatever')

    expect_equal(x, NULL)
})
