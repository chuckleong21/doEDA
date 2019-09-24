#' Import Tidy Excel Sheets into R
#'
#' @param file \strong{.xls} or \strong{.xlsx} file
#' @param flatten should output be displayed in tidy format for \code{TRUE}?
#' @param col_names \code{TRUE} to use the first row as column names,
#'   \code{FALSE} to get default names
#' @param col_types Default as \code{NULL} to guess all from the spreadsheet or
#'   a character vector containing one entry per column from these options:
#'   "skip", "guess", "logical", "numeric", "date", "text" or "list".
#' @param na Character vector of strings to interpret as missing values. By
#'   default, readxl treats blank cells as missing data
#' @param trim_ws Should leading and trailing whitespace be trimmed?
#' @param skip 	Minimum number of rows to skip before reading anything, be it
#'   column names or data. Leading empty rows are automatically skipped, so this
#'   is a lower bound. Ignored if range is given.
#' @param ... other arguments to pass down to \code{\link[readxl]{read_excel}}
#' @inheritParams readxl::read_excel
#'
#' @details This function wraps the steps for importing excel files having more
#'   than 1 sheet through \code{mutate()} a \emph{sheetName} column after
#'   intermediate iterations of sheet names via \code{map()}. For excel files
#'   that have a single sheet, they are dealt by \code{readxl()} with default
#'   arguments. \emph{sheetName} column will therefore be omitted.
#'
#' @return A \link[tibble]{tibble}
#'
#' @seealso \link[readxl]{read_excel}
#' @importFrom dplyr group_split
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr map_df map
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' library(readxl)
#' tidy_excel(readxl_example("type-me.xlsx"))
tidy_excel <- function(file, flatten = FALSE, col_names, col_types, na, trim_ws, skip, ...) {
        file_suffix <- regmatches(file, regexpr("[^.]+$(.*)", file))
        if(!grepl("\\.xls?(.)$", file)) {
                stop(paste("\nExpects xls or xlsx file but",
                           file_suffix, collapse = "", "instead."), call. = FALSE)}
        sheet <- excel_sheets(file)
        out <- read_excel(file, ...)
        if(length(sheet) < 2) {
                return(out)
        } else {
                out <- sheet %>%
                        map_df(~{read_excel(file, sheet = .x) %>%
                                        mutate(sheetNames = .x)})
                if(flatten) {
                        if(any(is.na(out)))
                                warning("Incoherent data cells are coerced into NAs",
                                        call. = FALSE)
                        return(out)
                } else{
                        seq_along(sheet) %>%
                                map(~read_excel(path = file, sheet = .x)) %>%
                                setNames(sheet)
                }

        }

}

