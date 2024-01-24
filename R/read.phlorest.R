#' Loads a Phlorest Dataset.
#'
#' @param mdpath the path to the directory or metadata JSON file.
#' @param datafile the expected datafile name (default="data.nex")
#' @return A `phlorest` object
#' @export
#' @examples
#' mdpath <- system.file("testthat/data", "cldf-metadata.json", package = "wordrates")
#' phl <- phlorest(mdpath)
read.phlorest <- function(mdpath, datafile='data.nex') {
    cldfobj <- rcldf::cldf(mdpath)

    # load data
    datafile <- file.path(cldfobj$base_dir, datafile)
    if (!file.exists(datafile)) {
        stop(sprintf("no %s in %s", datafile, cldfobj$base_dir))
    }
    cldfobj$data <- ape::read.nexus.data(datafile)

    cldfobj$posterior <- get_treefile(cldfobj, 'posterior')
    cldfobj$summary <- get_treefile(cldfobj, 'summary')

    class(cldfobj) <- c('cldf', 'phlorest')
    cldfobj
}

#' Loads a Phlorest Dataset.
#'
#' @param mdpath the path to the directory or metadata JSON file.
#' @param datafile the expected datafile name (default="data.nex")
#' @return A `phlorest` object
#' @export
#' @examples
#' mdpath <- system.file("testthat/data", "cldf-metadata.json", package = "wordrates")
#' phl <- phlorest(mdpath)
phlorest <- read.phlorest
