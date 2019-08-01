#' ggplot theme wrapper for display CJK characters
#'
#' @param family font family type
#'
#' @return ggplot2 theme list
#' @export
#'
#' @examples
#' NULL
theme_chn <- function(family = "STXihei") {
        ggplot2::theme(
                axis.text = ggplot2::element_text(family),
                axis.title = ggplot2::element_text(family),
                legend.text = ggplot2::element_text(family),
                legend.title = ggplot2::element_text(family),
                strip.text = ggplot2::element_text(family),
                panel.background = ggplot2::element_blank(),
                title = ggplot2::element_text(family)
        )
}
