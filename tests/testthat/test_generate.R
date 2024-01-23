test_that("test generate", {
    staging_dir <- tempdir()
    suppressMessages(
        generate(
            "data/nagaraja_et_al2013",
            staging_dir,
            filepattern='test_%s.dat',
            commands=c("test", "bayestraits")
        )
    )
    # should have created one file per word
    for (word in c("NAME", "NARROW", "NEAR", "NECK", "NEW", "NOSE")) {
        expect_true(file.exists(file.path(staging_dir, sprintf('test_%s.dat', word))))
    }
    # and trees
    treefile <- file.path(staging_dir, 'posterior.trees')
    expect_true(file.exists(treefile))
    nex <- ape::read.nexus(treefile)
    expect_equal(class(nex), 'multiPhylo')
    expect_equal(length(nex), 2)

    # and commands
    cmdfile <- file.path(staging_dir, 'bayestraits.cmd')
    expect_true(file.exists(cmdfile))
    bt <- readLines(cmdfile)
    expect_equal(bt[1], 'test')
    expect_equal(bt[2], 'bayestraits')

})

