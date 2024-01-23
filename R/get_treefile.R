#' Returns the file path to the phylogenies in the dataset
#'
#' @param cldfobj A rcldf::cldf/phlorest object.
#' @param type A string - "best", "summary", "posterior".
#' @return A file path
#' @export
#' @examples
#' cldfobj <- cldf(system.file("testthat/data/nagaraja_et_al2013", "cldf-metadata.json", package = "rcldf"))
#' get_treefile(cldfobj, 'posterior')
#' 'testthat/data/nagaraja_et_al2013/posterior.trees'
#' get_treefile(cldfobj, 'summary')
#' 'testthat/data/nagaraja_et_al2013/summary.trees'
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
