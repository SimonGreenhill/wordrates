#' Extracts a list of words from the phlorest dataset
#'
#' @param cldfobj A rcldf::cldf/phlorest object.
#' @return A list of words
#' @export
#' @examples
#' cldfobj <- cldf(system.file("testthat/data/nagaraja_et_al2013", "cldf-metadata.json", package = "rcldf"))
#' get_words(cldfobj)
# c("NAME", "NARROW", ...)
get_words <- function(cldfobj) {
    cldfobj$tables$ParameterTable |>
        dplyr::filter(Concepticon_Gloss != "" & !is.na(Concepticon_Gloss)) |>
        dplyr::pull(Concepticon_Gloss) |>
        unique() |>
        sort()
}
