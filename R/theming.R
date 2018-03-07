#' Dahaniel ggplot2 theme
#'
#' @param base_size base font size
#' @param base_family font family used
#'
#' @return a ggplot2 theme based on solarized 2
#' @export
#'
theme_dahaniel <- function(base_size = 7, base_family = "") {
  require(ggplot2)
  require(ggthemes)
  theme_solarized_2(base_size = base_size, base_family = base_family) +
    theme(title = element_text(color = "black"),
          text = element_text(color = "black"),
          panel.border = element_rect(color = "grey88", fill = NA, size = .2),
          axis.line.x = element_line(color = "black", size = .2),
          axis.line.y = element_line(color = "black", size = .2),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey88", size = .2),
          panel.grid.minor = element_line(color = "grey92", size = .15),
          axis.ticks = element_line(color = "black", size = .2),
          plot.background = element_rect(fill = "white"),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(color = "black"),
          legend.background = element_rect(fill = "white"),
          legend.key = element_rect(fill = "white"),
          legend.key.size = unit(.8, "lines"),
          legend.text = element_text(color = "black"),
          legend.title = element_text(color = "black"))
}



#' abcde
#'
#' @param label the label you want to plot
#'
#' @return a text grob
#' @export
#'
#' @examples
abcd <- function(label) {textGrob(label, x = 0, y = 1, hjust = -1, vjust = 1.1, gp = gpar(fontsize = 10))}
