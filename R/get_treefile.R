#' Returns the file path to the phylogenies in the dataset
#'
#' @param cldfobj A rcldf::cldf/phlorest object.
#' @param type A string - "best", "summary", "posterior".
#' @return A file path
#' @export
#' @examples
#' mdpath <- system.file("testthat/data", "cldf-metadata.json", package = "wordrates")
#' phl <- read.phlorest(mdpath)
#' get_treefile(phl, 'posterior')
#' 'testthat/data/posterior.trees'
#' get_treefile(phl, 'summary')
#' 'testthat/data/summary.trees'
get_treefile <- function(cldfobj, type='posterior') {
    if (type == 'posterior') {
        treefile <- file.path(cldfobj$base_dir, 'posterior.trees.zip')
        if (file.exists(treefile)) {
            return(treefile)
        }
        treefile <- file.path(cldfobj$base_dir, 'posterior.trees')
        if (file.exists(treefile)) {
            return(treefile)
        }
    } else if (type %in% c('summary', 'mcc', 'mcct')) {
        treefile <- file.path(cldfobj$base_dir, 'summary.trees')
        if (file.exists(treefile)) {
            return(treefile)
        }
    } else {
        return(NULL)
    }
}
