#' returns a named list of taxon => cognate sets
#'
#' @param cldfobj A phlorest object.
#' @param word the word to extract states for
#' @param alphabet the coding alphabet to use (default=LETTERS)
#' @param missing_state the state to use for missing values (default = "-")
#' @return A named list of states
#' @export
#' @examples
#' phl <- phlorest(system.file("testthat/data/nagaraja_et_al2013", "cldf-metadata.json", package = "rcldf"))
#' get_states(phl, 'NAME')
get_states <- function(cldfobj, word, alphabet=LETTERS, missing_state="-") {
    if (!inherits(cldfobj, "phlorest")) stop("'cldfobject' must inherit from class phlorest")

    # check word is in parameters
    params <- cldfobj$tables$ParameterTable %>% dplyr::filter(Concepticon_Gloss == word)
    if (nrow(params) == 0) { stop(paste("Invalid word", word)) }

    param_ids <- as.integer(params$ID)
    # make sure data ids are numeric
    if (any(is.na(param_ids))) { stop("Parameter IDs are not numeric")}

    states <- lapply(
        names(cldfobj$data),
        function(taxon) recode(cldfobj$data[[taxon]][param_ids], alphabet=alphabet, missing_state=missing_state)
    )
    names(states) <- names(cldfobj$data)
    states
}


recode <- function(states, alphabet=LETTERS, missing_state="-") {
    if (length(states) > length(alphabet)) { stop("Too many states!") }
    labels <- alphabet[seq_along(states)]
    out <- labels[which(states == "1")]
    if (length(out) == 0) out <- c(missing_state)
    out
}




