tweet_theme <- function(
  primary.font = "Josefin Sans Light",
  primary.text.color = "#080708",
  primary.text.size = 16,
  title.font = "VCR OSD Mono",
  title.color = primary.text.color,
  title.size = 20,
  subtext.font = primary.font,
  subtext.color = "#343434",
  subtext.size = 15,
  panel.background = "#FDFFFC",
  panel.border.color = "#59C9A5",
  plot.background = "#FDFFFC"
) {
  ggplot2::theme(
    # Main title styles
    plot.title=ggplot2::element_text(family=title.font,
                                     size = title.size,
                                     color=title.color),
    
    # Subtitle styles
    plot.subtitle=ggplot2::element_text(family=subtext.font,
                                        size=subtext.size,
                                        color=subtext.color),
    
    # Panel BG
    panel.background = ggplot2::element_rect(
      fill=panel.background,
      colour = NA
    ),
    
    # Panel border
    panel.border = ggplot2::element_rect(
      color = panel.border.color,
      fill = NA,
      linetype = "solid",
      size = 0.75
    ),
    
    # Plot BG
    plot.background = ggplot2::element_rect(
      fill=plot.background,
      colour = NA
    ),
    
    # axis.text.x=ggplot2::element_text(margin=ggplot2::margin(5, b=10)),
    # axis.text.y = ggplot2::element_text(margin=ggplot2::margin(l = 10)),
    axis.ticks=ggplot2::element_blank(),
    axis.line=ggplot2::element_blank(),
    
    axis.title=ggplot2::element_text(family=primary.font,
                                     size=primary.text.size,
                                     color=primary.text.color),
    axis.text=ggplot2::element_text(family=primary.font,
                                    size=primary.text.size,
                                    color=primary.text.color)
  )
}