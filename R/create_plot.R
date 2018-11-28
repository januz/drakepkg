#' @title Create a Plot of Iris Data
#' @description Create a specific histogram for the \code{iris} data
#' @param data the \code{iris} dataset
#' @name create_plot
#' @return a \link{ggplot2} plot object
#' @export
create_plot <- function(data) {

  ggplot2::ggplot(data, ggplot2::aes(x = Petal.Width, fill = Species)) +
    ggplot2::geom_histogram(binwidth = 0.1) +
    ggplot2::theme_gray(20)

}
