
test_that("test read.phlorest", {
    # fail on bad data.nex
    expect_error(read.phlorest(cldfobj, "invalid.nex"))

    # make sure we have data attached
    phl <- read.phlorest("data")
    expect_equal(names(phl$data), c("Khasi", "Khmu", "Lyngngam", "Palaung", "PnarJaintia", "WarLamin"))
    expect_equal(phl$data$Khasi[1:5], c("1", "0", "1", "0", "1"))

    # and trees
    expect_equal(basename(phl$posterior), 'posterior.trees.zip')
    expect_equal(basename(phl$summary), 'summary.trees')
})


test_that("test phlorest", {
    # make sure we have data attached
    phl <- phlorest("data")
    expect_equal(names(phl$data), c("Khasi", "Khmu", "Lyngngam", "Palaung", "PnarJaintia", "WarLamin"))
    expect_equal(phl$data$Khasi[1:5], c("1", "0", "1", "0", "1"))

    # and trees
    expect_equal(basename(phl$posterior), 'posterior.trees.zip')
    expect_equal(basename(phl$summary), 'summary.trees')
})
