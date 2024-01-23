#' writes a file in Bayestraits form
#'
#' @param filename filename to write to.
#' @param states a states object from get_states
#' @return NULL
#' @export
write.bayestraits <- function(states, filename) {
    lines <- sapply(
        names(states),
        function(x) sprintf("%-20s %s", x, paste0(states[[x]], collapse="")),
        simplify=TRUE,
        USE.NAMES=FALSE
    )
    writeLines(lines, filename)
}
