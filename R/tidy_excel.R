#' Import Tidy Excel Sheets into R
#'
#' @param file \strong{.xls} or \strong{.xlsx} file
#' @param ... arguments passed down to \link[readxl:read_excel]{read_excel()}
#' @param flatten should output be displayed in tidy format for \code{TRUE}?
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
#' @importFrom dplyr group_split
#' @importFrom readxl excel_sheets read_excel
#' @importFrom purrr map_df map
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' library(readxl)
#' tidy_excel(readxl_example("type-me.xlsx"))
tidy_excel <- function(file, ..., flatten = FALSE) {
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

