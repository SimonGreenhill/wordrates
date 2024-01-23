#' Loads a Phlorest Dataset.
#'
#' @param mdpath the path to the directory or metadata JSON file.
#' @return A `phlorest` object
#' @export
#' @examples
#' phl <- cldf(system.file("testthat/data/nagaraja_et_al2013", "cldf-metadata.json", package = "rcldf"))
read.phlorest <- function(metadata, datafile='data.nex') {
    cldfobj <- rcldf::cldf(metadata)

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
