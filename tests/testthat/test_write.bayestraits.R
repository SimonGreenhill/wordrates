
test_that("test write.bayestraits", {
    phl <- read.phlorest("data/nagaraja_et_al2013")
    states <- get_states(phl, 'NAME')

    outfile <- tempfile()
    write.bayestraits(states, outfile)
    obtained <- readLines(outfile)

    expect_equal(obtained[[1]], "Khasi                AC")
    expect_equal(obtained[[2]], "Khmu                 ABCD")
    expect_equal(obtained[[3]], "Lyngngam             -")
    expect_equal(obtained[[4]], "Palaung              -")
    expect_equal(obtained[[5]], "PnarJaintia          B")
    expect_equal(obtained[[6]], "WarLamin             C")

    unlink(outfile)
})
