ALPHABET <- unique(c(LETTERS, 1:9, 0, letters))

#' returns a named list of taxon => cognate sets
#'
#' @param cldfobj A phlorest object.
#' @param word the word to extract states for
#' @param alphabet the coding alphabet to use (default=c(LETTERS, 1:9, 0))
#' @param missing_state the state to use for missing values (default = "-")
#' @return A named list of states
#' @importFrom rlang .data
#' @export
#' @examples
#' mdpath <- system.file("testthat/data", "cldf-metadata.json", package = "wordrates")
#' get_states(read.phlorest(mdpath), 'NAME')
get_states <- function(cldfobj, word, alphabet=ALPHABET, missing_state="-") {
    if (!inherits(cldfobj, "phlorest")) stop("'cldfobject' must inherit from class phlorest")

    # check word is in parameters
    params <- cldfobj$tables$ParameterTable |> dplyr::filter(.data$Concepticon_Gloss == word)
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


recode <- function(states, alphabet=ALPHABET, missing_state="-") {
    if (length(states) > length(alphabet)) {
        stop(sprintf("Too many states! (have %d, need %d)", length(alphabet), length(states)))
    }
    labels <- alphabet[seq_along(states)]
    out <- labels[which(states == "1")]
    if (length(out) == 0) out <- c(missing_state)
    out
}




