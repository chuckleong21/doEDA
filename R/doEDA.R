#' Generate graphs for EDA with a line of code
#'
#' @param type a string for chart type. Options availble are \itemize{
#' \item{\strong{"pie"}} for pie chart
#' \item{\strong{"waffle"}} for waffle chart
#' \item{\strong{"hbar"}} for horizontal bar chart
#' \item{\strong{"tree"}} for tree map
#' }
#' @param .tbl_df data frame
#' @param ... arguments passed down to functions \code{pieChart()},
#'   \code{waffleChart()}, \code{hbarChart()} and \code{treeMap()}
#'
#' @return pie chart / horizontal bar chart via ggplot2 waffle chart via
#'   waffle::waffle treemap via treemap::treemap
#' @export doEDA
#' @importFrom grDevices palette
#'
#' @seealso \link[ggplot2]{ggplot2}, \link[waffle]{waffle}, \link[treemap]{treemap}
#'
#' @examples
#' library(dplyr)
#' data("HairEyeColor")
#' df <- as.data.frame(HairEyeColor)
#' #pie chart
#' doEDA(type = "pie", .tbl_df = df, .group = Eye)
#'
#' #waffle chart
#' doEDA("waffle", df, .group = Eye, divisor = 0.1)
#'
#' #horizontal bar chart
#' doEDA(type = "hbar", .tbl_df = df, .innerGroup = vars(Hair, Eye, Sex),
#' .outerGroup = vars(Hair, Eye), .xVar = Sex, .fill = Hair)
#'
#' #treemap
#' doEDA(type = "tree", .tbl_df = df, .group = Hair, index = 'Hair')
doEDA <- function(type, .tbl_df, ...) {
    stopifnot(is.data.frame(.tbl_df))

    if(missing(type)) stop("Choose a chart type. See ?doEDA for details", call. = FALSE)
    switch(type, "pie" = pieChart(.tbl_df, ...),
                  "waffle" = waffleChart(.tbl_df, ...),
                  "hbar" = hbarChart(.tbl_df, ...),
                  "tree" = treeMap(.tbl_df, ...))
}

pieChart <- function(.tbl_df, .group, start = 0) {
    group <- dplyr::enquo(.group)

    .tbl_df %>%
        dplyr::group_by(!!group) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
        dplyr::arrange(dplyr::desc(!!group)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = "", y = .data$prop, fill = !!group),
                          stat ="identity", width = 1) +
        ggplot2::coord_polar("y", start) +
        ggplot2::geom_text(ggplot2::aes(x = 1, y = cumsum(.data$prop) - .data$prop / 2,
                                        label = scales::percent(.data$prop)))
}

waffleChart <- function(.tbl_df, .group, divisor = 30, title = "", size = 2, palette = "Set2") {
    group <- dplyr::enquo(.group)

    lvl <- levels(dplyr::pull(.tbl_df, !!group))
    rcolors <- RColorBrewer::brewer.pal(n = length(lvl), name = palette)
    x <- .tbl_df %>%
        dplyr::group_by(!!group) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::pull(.data$n) %>%
        purrr::set_names(nm = lvl)
    x <- x / divisor
    waffle::waffle(x, title = title, colors = rcolors, size = size)
}

hbarChart <- function(.tbl_df, .innerGroup, .outerGroup, .xVar, .fill) {
    stopifnot(is.list(.innerGroup), is.list(.outerGroup),
              length(.outerGroup) < length(.innerGroup))
    xVar <- dplyr::enquo(.xVar)
    fill <- dplyr::enquo(.fill)

    .tbl_df %>%
        dplyr::group_by(!!!.innerGroup) %>%
        dplyr::count() %>%
        dplyr::ungroup() %>%
        dplyr::group_by(!!!.outerGroup) %>%
        dplyr::mutate(prop = .data$n / sum(.data$n)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = !!xVar, y = .data$prop, fill = !!fill)) +
        ggplot2::geom_col(position = "fill") +
        ggplot2::coord_flip()
}

treeMap <- function(.tbl_df, .group, index, type = "index", sizeby = "n",
                    fontfamily.labels = "Helvetica",
                    fontfamily.title = "Helvetica", title = "") {
    group <- dplyr::enquo(.group)

    .tbl_df %>%
        dplyr::group_by(!!group) %>%
        dplyr::count() %>%
        dplyr::arrange(dplyr::desc(.data$n)) %>%
        dplyr::ungroup() %>%
        treemap::treemap(index = index,
                vSize = sizeby,
                type = type,
                fontfamily.labels = fontfamily.labels,
                fontfamily.title = fontfamily.title,
                title = title)
}
