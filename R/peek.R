#' @title Peek At Data As A Whole
#'
#' @param .tbl_df data.frame object
#'
#' @return Return a ggplot object via \code{vis_dat} from \code{visdat}
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' d <- data.frame(int = 1:10,
#'                 string = letters[1:10],
#'                 factor = factor(month.abb[1:10]))
#' peek(d)
peek <- function(.tbl_df) {

        stopifnot(is.data.frame(.tbl_df))

        dim <- dim(.tbl_df)

        diagnosis <- dlookr::diagnose(.tbl_df)

        PK <- diagnosis$variables[which(diagnosis$unique_count == nrow(.tbl_df))]

        groupnm <-
                diagnosis %>%
                group_by(types) %>%
                group_keys %>%
                unlist(use.names = F)

        diagnosis <-
                diagnosis %>%
                group_by(types) %>%
                group_split() %>%
                purrr::set_names(groupnm)

        vartype <- sort(table(diagnosis$types), decreasing = T)

        list(peekture = list(dim = dim, variables = vartype, diagnosis = diagnosis, PK = PK)) %>%
                list2env(envir = .GlobalEnv) %>%
                invisible()
        return(visdat::vis_dat(.tbl_df))
}
