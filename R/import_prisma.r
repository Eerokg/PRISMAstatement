#' Importing screening data into a PRISMA graph
#'
#' Currently only data from [Rayyan](https://rayyan.qcri.org/) is supported.
#' If one article has multiple reasons, only the first one is used.
#' Rayyan separates reasons with commas, so they cannot be used within a reason.
#' Only files that include reasons and decisions should be imported (see export options in Rayyan).
#'
#' @param ft a CSV-file exported from Rayyan with the results of the full-text screening.
#' @param abs a CSV-file exported from Rayyan with the results of the title and abstract screening
#' @param reviewer the name of the reviewer whose decisions should be used. By default the first
#' decision encountered in the entry is used.
#'
#' @return A list with the class 'prisma_import'
#'
#' @examples
#' \dontrun{
#' prisma_data <- import_prisma(ft = 'full_text.csv', abs = 'abstract.csv', reviewer = 'John')
#' }
#' @importFrom utils read.csv
#' @importFrom stringr str_detect str_match
#' @export
import_prisma <- function(ft, abs = NULL, reviewer = NULL){
  exclude_string <- ".*\"Excluded\""
  ft <- utils::read.csv(ft)
  if (!is.null(reviewer)) {
    exclude_string <- paste0(".*\"", reviewer, "\"=>\"Excluded\".*")
    if (!any(stringr::str_detect(ft$notes, paste0("\"", reviewer, "\""))))
      stop("Reviewer not found in full-text file! Check the spelling.")
  }
  if(!is.null(abs)) {
    abs <- utils::read.csv(abs)
    abs_n <- nrow(abs)
    if (!is.null(reviewer) & !any(stringr::str_detect(abs$notes, paste0("\"", reviewer, "\""))))
      stop("Reviewer not found in abstracts file! Check the spelling.")
    abs_excl <- sum(stringr::str_detect(abs$notes, exclude_string))
  } else { abs_n <- NULL
           abs_excl <- NULL }
  ft_n <- nrow(ft)
  ft_excl <- sum(stringr::str_detect(ft$notes, exclude_string))
  reasons <- stringr::str_match(ft$notes, "RAYYAN-EXCLUSION-REASONS: ([^,]*)")[,2]
  reasons <- as.data.frame(table(as.factor(reasons)))
  reasons <- reasons[order(-reasons$Freq),]
  reasons <- list(abs_n = abs_n, abs_excluded = abs_excl, ft_n = ft_n, ft_excluded = ft_excl, reasons = reasons)
  attr(reasons, "class") <- "prisma_import"
  if(reasons$ft_excluded != sum(reasons$reasons$Freq))
    stop("There are exclusions without reasons. Add missing reasons and try again!")
  return(reasons)
}

#' @method print prisma_import
#' @export
print.prisma_import <- function(x, ...) {
  cat("Imported data:\n", rep_len("*", 10))
  if(!is.null(x$abs_n)) {
    cat("Articles (without duplicates):", x$abs_n, "\n")
    cat("Excluded abstracts:", x$abs_excluded, "\n")
  }
  cat("Full-text articles:", x$ft_n, "\n")
  cat("Excluded full-texts:", x$ft_excluded, "\n")
  cat("Reasons:\n", paste0(x$reasons$Var1, ": ", x$reasons$Freq, "\n"))
  cat("Included articles:", x$ft_n - x$ft_excluded, "\n")
}
