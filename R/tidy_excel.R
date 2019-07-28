#' Import Tidy Excel Sheets into R
#'
#' @param file \strong{.xls} or \strong{.xlsx} file
#' @param ... arguments passed down to \link[readxl:read_excel]{read_excel()}
#'
#' @details This function wraps the steps for importing excel files having more
#'   than 1 sheet through \code{mutate()} a \emph{sheetName} column after
#'   intermediate iterations of sheet names via \code{map()}. For excel files
#'   that have a single sheet, they are dealt by \code{readxl()} with default
#'   arguments. \emph{sheetName} column will therefore be omitted.
#'
#' @return A \link[tibble]{tibble}
#'
#' @seealso \itemize{ \item \link[dplyr]{mutate} \item \link[purrr]{map} }
#'
#' @export
#'
#' @examples
#' tidy_excel(readxl::readxl_example("type-me.xlsx"))
tidy_excel <- function(file, ...) {
        file_suffix <- regmatches(file, regexpr("[^.]+$(.*)", file))
        if(!grepl("\\.xls?(.)$", file)) {
                stop(paste("\nExpects xls or xlsx file but",
                           file_suffix, collapse = "", "instead."), call. = FALSE)}
        sheet <- readxl::excel_sheets(file)
        out <- readxl::read_excel(file, ...)
        if(length(sheet) < 2) {
                return(out)
        } else {
                sheet %>%
                        purrr::map_df(~{readxl::read_excel(file, sheet = .x) %>%
                                        dplyr::mutate(sheetNames = .x)})
        }

}

