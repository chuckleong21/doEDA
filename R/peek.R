#' @title Peek At Data As A Whole
#'
#' @param .tbl_df data.frame object
#'
#' @return Return a ggplot object via \code{vis_dat} from \code{visdat}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' d <- data.frame(int = 1:10,
#'                 string = letters[1:10],
#'                 factor = factor(month.abb[1:10]),
#'                 stringsAsFactors = FALSE)
#' peek(d)
peek <- function(.tbl_df) {

        stopifnot(is.data.frame(.tbl_df))

        dim <- dim(.tbl_df)

        diagnosis <- dlookr::diagnose(.tbl_df)

        PK <- diagnosis$variables[which(diagnosis$unique_count == nrow(.tbl_df))]

        groupnm <-
                diagnosis %>%
                dplyr::group_by(.data$types) %>%
                dplyr::group_keys() %>%
                unlist(use.names = F)

        vartype <- sort(table(groupnm), decreasing = T)

        diagnosis <-
                diagnosis %>%
                dplyr::group_by(.data$types) %>%
                dplyr::group_split() %>%
                purrr::set_names(groupnm)

        list(peekture = list(dim = dim, variables = vartype, diagnosis = diagnosis, PK = PK)) %>%
                list2env(envir = .GlobalEnv) %>%
                invisible()
        return(visdat::vis_dat(.tbl_df))
}
