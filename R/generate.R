#' Generates a full set of Bayestraits analysis files
#'
#' @param mdpath the path to the directory or metadata JSON file.
#' @param staging_dir the path where to write the files.
#' @param filepattern the pattern for output files.
#' @param commands a list of the commands to save to bayestraits.cmd
#' @export
generate <- function(mdpath, staging_dir, filepattern='trait_%s.dat', commands=c()) {
    cldfobj <- read.phlorest(mdpath)

    # create staging dir if it doesn't exist
    if (!file.exists(staging_dir)) {
        message(sprintf("creating staging_dir %s\n", staging_dir))
        dir.create(staging_dir)
    }

    for (word in get_words(cldfobj)) {
        filename <- get_filename(staging_dir, word, filepattern)
        #cldfobj$metadata[['rdf:ID']]
        message(sprintf("generate %s -> %s\n", word, filename))
        states <- get_states(cldfobj, word)
        if (is.constant(states)) { message(sprintf("warning: %s is constant", word)) }
        write.bayestraits(states, filename)
    }

    message(sprintf("copying trees -> posterior.trees\n"))
    if (!is.null(cldfobj$posterior)) {
        if (tolower(tools::file_ext(cldfobj$posterior)) == 'zip') {
            message('unzipping')
            trees <- ape::read.nexus(unz(cldfobj$posterior, 'posterior.trees'))
        } else {
            trees <- ape::read.nexus(trees)
        }
        ape::write.nexus(trees, file=file.path(staging_dir, 'posterior.trees'), translate=TRUE)  # bayestraits needs translated trees
    } else if (!is.null(cldfobj$summary)) {
        message('falling back to summary trees as no posterior trees found')
        file.copy(cldfobj$summary, staging_dir)
    } else {
        message('no trees found')
    }




    if (length(commands)) {
        writeLines(commands, file.path(staging_dir, 'bayestraits.cmd'))
    }

}

