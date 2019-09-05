#' Draw a fast pie chart
#'
#' @description \code{pieChart()} saves the repeating data manipulation and
#'   ggplot2 graphic grammars to produce a pie chart. It is for visualizing
#'   distribution of factor levels of a single variable.
#'
#' @param .tbl_df A data frame
#' @param .group A \emph{factor} variable
#' @param start \code{\link[ggplot2:coord_polar]{coord_polar()}} argument to
#'   control where the counting position starts. Defaults at 0
#' @param direction 1, clockwise; -1, anticlockwise
#' @param labelon logical scalar. Should labels be added via
#'   \code{\link[ggplot2:geom_text]{geom_text()}}?
#' @param ... arguments passed down to \code{\link[ggplot2:labs]{labs()}} for
#'   \emph{title}, \emph{subtitle}, etc.
#'
#' @return A \strong{ggplot} object
#' @export
#' @importFrom dplyr group_by count ungroup mutate arrange desc pull
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar geom_text labs ggtitle
#' @importFrom ggplot2 geom_col coord_flip
#' @importFrom rlang enexpr enquo enquos is_symbol sym quo_name
#'
#' @examples
#' pieChart(invest, gender)
pieChart <- function(.tbl_df, .group, start = 0, direction = 1, labelon = TRUE, ...) {
    group <- enexpr(.group)
    if(!is_symbol(group)) group <- sym(group)
    group <- enquo(group)

    w <- "pieChart() is not preferred for group with >10 factor levels."
    groupc <- quo_name(group)
    if(is.null(levels(.tbl_df[[groupc]]))) {
        .tbl_df[[groupc]] <- as.factor(.tbl_df[[groupc]])
        if(nlevels(.tbl_df[[groupc]]) > 10) warning(w, call. = FALSE)
    }

    p <-  .tbl_df %>%
        group_by(!!group) %>%
        count() %>%
        ungroup() %>%
        mutate(prop = .data$n / sum(.data$n)) %>%
        arrange(desc(!!group)) %>%
        ggplot() +
        geom_bar(aes(x = "", y = .data$prop, fill = !!group),
                          stat = "identity") +
        coord_polar("y", start = start, direction = direction)
   if(labelon) {
       p + geom_text(aes(x = 1, y = cumsum(.data$prop) - .data$prop / 2,
                                       label = scales::percent(.data$prop))) +
           labs(x = "", y = groupc, ...)
   } else p
}

#' Draw a fast waffle chart
#'
#' @description \code{waffleChart()} is the flatten version of
#'   \code{pieChart()}. It works the best when a pie chart is less straight
#'   forward to reveal distribution of a factor variable.
#'
#' @param .tbl_df A data frame
#' @param .group A \emph{factor} variable
#' @param divisor A non-zero positive number
#' @param tiles the default number of 100 for the number of tiles displayed on
#'   plotting canvas if \code{divisor = NULL}
#' @param title A string
#' @param size A number.
#' @param palette A character vector or a string
#' @param legend_pos legend position
#'
#' @details A wrapper function inherits from
#'   \code{\link[waffle:waffle]{waffle()}}. It spares the data wrangling process
#'   for percentage computation and assemble for a named vector. The wrapper
#'   function also internally deals with conflict between the number of factor
#'   levels and the number of colors supplied to palette via
#'   \link[RColorBrewer]{RColorBrewer}.
#'
#' @return A \strong{ggplot} object
#' @export
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#'
#' @examples
#' waffleChart(invest, age, divisor = 100)
waffleChart <- function(.tbl_df, .group, divisor = NULL, tiles = 100, title = NULL,
                        size = 2, palette = "Set2", legend_pos = "right") {
    # symbol and string
    group <- enexpr(.group)
    if(!is_symbol(group)) group <- sym(group)
    group <- enquo(group)

    # palette arg
    lvl <- levels(as.factor(pull(.tbl_df, !!group)))
    maxcolors <- ifelse(length(palette) > 1, length(palette),
                        brewer.pal.info[palette, "maxcolors"])
    if(length(palette) > 1 & length(lvl) >= maxcolors) {
        rcolors <- palette
    } else {
        rcolors <- brewer.pal(n = length(lvl), name = palette)
    }

    # get the part
    x <- .tbl_df %>%
        group_by(!!group) %>%
        count() %>%
        ungroup() %>%
        pull(.data$n) %>%
        purrr::set_names(nm = lvl)
    # pick a divisor
    if(is.null(divisor)) divisor <- sum(x) / tiles
    x <- x / divisor
    # draw waffle
    p <- waffle::waffle(x, colors = rcolors, size = size, legend_pos = legend_pos)
    if(is.null(title)) {
        p
    } else p + ggtitle(title)
}

#' Visualize proportions of subgroup samples
#'
#' @description \code{hbarChart()} frees user from repetitive workflows for
#'   creating horizontal bar chart in order to visualize subgroup distributions.
#'
#' @param .tbl_df A data frame
#' @param ... fixed columns names supplied from \code{.tbl_df}
#' @param columns numeric vector that has a length shorter than the supplied by
#'   \code{...}
#' @param .xVar One of the variables from ... besides the .fill variable
#' @param .fill One of the variables from ...
#'
#' @details .innerGroup should, as the argument suggests, be a list of variables
#'   for grouping. They are the unit groups. On the other hand, .outerGroup is a
#'   list of variables shorter than .innerGroup. In turn .outerGroup is at an
#'   outer level and .innerGroup is a subset of .outerGroup. All the variables
#'   passed into .innerGroup and .outerGroup must be selected by \code{vars()}
#'   as they are quosures inside the function body. You could modify the labels
#'   for y axis using \code{scale_y_continuous()} since it is a ggplot object.
#'
#' @return A \strong{ggplot} object
#' @export
#' @importFrom ggplot2 geom_col coord_flip facet_wrap scale_color_brewer
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' hbarChart(invest, gender, age, investment, invest_plan, perc,
#' columns = 3:4, .xVar = age, .fill = perc) +
#' facet_wrap(vars(invest_plan)) +
#' scale_fill_brewer(type = "qual", palette = "Blues")
hbarChart <- function(.tbl_df, ..., columns, .xVar, .fill) {
    innergroup <- enquos(...)
    outergroup <- innergroup[columns]
    stopifnot(is.list(innergroup), is.numeric(columns),
              length(outergroup) < length(innergroup))
    xVar <- enquo(.xVar)
    fill <- enquo(.fill)

    .tbl_df %>%
        group_by(!!!innergroup) %>%
        count() %>%
        ungroup() %>%
        group_by(!!!outergroup) %>%
        mutate(prop = .data$n / sum(.data$n)) %>%
        ungroup() %>%
        ggplot(aes(x = !!xVar, y = .data$prop, fill = !!fill)) +
        geom_col(position = "fill") +
        coord_flip() +
        labs(y = "")
}

#' Fast tree map wrapper
#'
#' @description This is a wrapper for the workflow producing a tree map via
#'   \code{treemap} package. See
#'   \code{\link[treemap:treemap-package]{treemap-package}}.
#'
#' @param .tbl_df A data frame
#' @param .group A factor variable
#' @param index A string identical to .group
#' @inheritParams treemap::treemap
#' @param type A string for index type
#' @inheritParams treemap::treemap
#' @param sizeby A string for which the tile size depending on
#' @param fontfamily.labels A string for font type
#' @param fontfamily.title A string for font type
#' @param title A title
#' @param ... arguments passed down to \code{treemap()}
#'
#' @details Other than most of the arguments inherited from treemap(), treeMap()
#'   embbeded a default font type, \emph{Helvetica} for Latin characters and
#'   \emph{STXihei} CJK characters on \strong{MacOS}, when
#'   \code{fontfamily.title} and \code{fontfamily.labels} are \strong{both} not
#'   specified. One should note that both \emph{fontfamily.*} arguments should
#'   be be absent one-sided. This will not change the font type according to
#'   user's will other than the default ones.
#'
#' @seealso \link[treemap:treemap]{treemap}
#' @return A \strong{ggplot} object
#' @export
#'
#' @examples
#' treeMap(invest, investment, "investment")
treeMap <- function(.tbl_df, .group, index, type = "index", sizeby = "n",
                    fontfamily.labels, fontfamily.title, title = NULL, ...) {
    group <- enquo(.group)
    grouplbls <- levels(as.factor(pull(.tbl_df, !!group)))

    if(missing(fontfamily.labels) | missing(fontfamily.title)) {
        if(mean(grepl("^[\u4e00-\u9fa5]{0,}$", grouplbls)) > .5) {
            fontfamily.labels <- fontfamily.title <- "STXihei"
        } else {fontfamily.labels <- fontfamily.title <- "Helvetica"}
    }

    if(is.null(title)) title <- ""

    .tbl_df %>%
        group_by(!!group) %>%
        count() %>%
        arrange(desc(.data$n)) %>%
        ungroup() %>%
        treemap::treemap(index = index,
                vSize = sizeby,
                type = type,
                fontfamily.labels = fontfamily.labels,
                fontfamily.title = fontfamily.title,
                title = title, ...)
}

#' Data on Investment preferences
#'
#' A dummy dataset containing a genders, age, investment and other attributes on
#' investors.
#'
#' @docType data
#' @author 广州金十信息科技有限公司 \email{job@@jin10.com}
#' @keywords data
#' @usage data(invest)
#' @format A data frame with 14311 rows and 5 variables:
#' \describe{
#' \item{gender}{Female or Male}
#' \item{age}{5 segemented age range between 18 to 51+ years old}
#' \item{invest_plan}{future investment choices for financial products}
#' \item{perc}{percentage weightages on invest_plan}
#' \item{investment}{current investments}
#' }
"invest"
