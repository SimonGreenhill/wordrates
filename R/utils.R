#' Checks if a state object is constant
#'
#' @param states A list of states
#' @return Boolean TRUE/FALSE
#' @export
#' @examples
#' is.constant(list('L1' = c("A"), 'L2' = c("A"))) == TRUE
#' is.constant(list('L1' = c("A"), 'L2' = c("-"))) == TRUE
#' is.constant(list('L1' = c("A"), 'L2' = c("B"))) == FALSE
is.constant <- function(states) {
    length(unique(unlist(states[states!='-']))) == 1
}




get_filename <- function(staging_dir, word, filepattern) {
    word <- gsub("(", "", word, fixed=TRUE)
    word <- gsub(")", "", word, fixed=TRUE)
    word <- gsub(" ", "_", word, fixed=TRUE)
    file.path(staging_dir, sprintf(filepattern, word))
}
