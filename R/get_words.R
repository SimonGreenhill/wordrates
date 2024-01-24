#' Extracts a list of words from the phlorest dataset
#'
#' @param cldfobj A rcldf::cldf/phlorest object.
#' @return A list of words
#' @export
#' @examples
#' mdpath <- system.file("testthat/data", "cldf-metadata.json", package = "wordrates")
#' phl <- phlorest(mdpath)
#' get_words(phl)
# c("NAME", "NARROW", ...)
get_words <- function(cldfobj) {
    cldfobj$tables$ParameterTable |>
        dplyr::filter(.data$Concepticon_Gloss != "" & !is.na(.data$Concepticon_Gloss)) |>
        dplyr::pull(.data$Concepticon_Gloss) |>
        unique() |>
        sort()
}
