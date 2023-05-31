#' Title
#'
#' An opinionated ggplot2 theme based on [hrbrthemes::theme_ipsum()]. The theme
#' automatically supports Markdown in axis and facet labels through
#' [ggtext::element_markdown()] and provides reasonable defaults for visually
#' appealing graphics.
#'
#' @inheritParams hrbrthemes::theme_ipsum
#' @param ... Additional arguments passed to [hrbrthemes::theme_ipsum].
#'
#' @return A ggplot2 theme.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   theme_measr()
theme_measr <- function(base_family = "Open Sans", base_size = 11.5, ...) {
  ret <- hrbrthemes::theme_ipsum(base_family = base_family,
                                 base_size = base_size,
                                 ...)

  ret <- ret +
    ggplot2::theme(legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   legend.position = "bottom",
                   strip.text.x = ggtext::element_markdown(),
                   strip.text.y = ggtext::element_markdown(),
                   axis.title.x = ggtext::element_markdown(),
                   axis.title.y = ggtext::element_markdown(),
                   axis.text.x = ggtext::element_markdown(color = "black"),
                   axis.text.y = ggtext::element_markdown(color = "black"))

  ret
}


#' Set global plot theme options
#'
#' Calling this function will set [theme_measr()] as the default theme for all
#' plots created with ggplot2. Additionally, new default color scales are
#' specified to override the ggplot2 defaults for continuous, binned, and
#' discrete scales.
#'
#' @param base_family Base font family.
#' @param v_scale Viridis scale for continuous variables. See
#'   [ggplot2::scale_colour_viridis_c].
#' @param d_scale Discrete color scale to use.
#' @param ... Additional parameters passed to [theme_measr()].
#'
#' @return None. Called for side effects.
#' @export
set_theme <- function(base_family = "Open Sans",
                      v_scale = "mako",
                      d_scale = palette_measr, ...) {
  # install and load fonts
  if (base_family != "") {
    if (!(base_family %in% showtextdb::font_installed())) {
      showtextdb::font_install(showtextdb::google_fonts(base_family))
    }
    font_files <-
      system.file("fonts", base_family,
                  c("regular.ttf", "bold.ttf", "italic.ttf", "bolditalic.ttf"),
                  package = "showtextdb")
    sysfonts::font_add(family = base_family,
                       regular = font_files[1], bold = font_files[2],
                       italic = font_files[3], bolditalic = font_files[4])
    showtext::showtext_auto()
  }

  # set theme
  ggplot2::theme_set(theme_measr(base_family = base_family, ...))
  hrbrthemes::update_geom_font_defaults(family = base_family,
                                        face = "plain", size = 3.5,
                                        color = "black")

  # change default color scales
  cont_fill <- function(..., option = v_scale) {
    ggplot2::scale_fill_continuous(..., option = option, type = "viridis")
  }
  cont_colr <- function(..., option = v_scale) {
    ggplot2::scale_colour_continuous(..., option = option, type = "viridis")
  }
  binn_fill <- function(..., option = v_scale) {
    ggplot2::scale_fill_binned(..., option = option, type = "viridis")
  }
  binn_colr <- function(..., option = v_scale) {
    ggplot2::scale_colour_binned(..., option = option, type = "viridis")
  }
  disc_fill <- function(..., type = d_scale) {
    ggplot2::scale_fill_discrete(..., type = type)
  }
  disc_colr <- function(..., type = d_scale) {
    ggplot2::scale_colour_discrete(..., type = type)
  }

  options(ggplot2.continuous.fill = cont_fill,
          ggplot2.continuous.colour = cont_colr,
          ggplot2.binned.fill = binn_fill,
          ggplot2.binned.colour = binn_colr,
          ggplot2.discrete.fill = disc_fill,
          ggplot2.discrete.colour = disc_colr)
}
