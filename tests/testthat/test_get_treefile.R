
test_that("test get_treefile", {
    cldfobj <- read.phlorest("data/nagaraja_et_al2013")
    p <- get_treefile(cldfobj, 'posterior')
    s <- get_treefile(cldfobj, 'summary')
    x <- get_treefile(cldfobj, 'whatever')

    expect_equal(x, NULL)
})
