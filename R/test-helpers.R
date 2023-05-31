local_theme <- function(font, continuous, discrete, ..., env = parent.frame()) {
  # save defaults
  op <- options(ggplot2.continuous.colour = NULL,
                ggplot2.continuous.fill = NULL,
                ggplot2.binned.colour = NULL,
                ggplot2.binned.fill = NULL,
                ggplot2.discrete.colour = NULL,
                ggplot2.discrete.fill = NULL)
  og_thm <- ggplot2::theme_get()

  set_theme(base_family = font, v_scale = continuous, d_scale = discrete, ...)

  withr::defer({
    # restore default ggplot2 theme
    ggplot2::theme_set(og_thm)
    options(op)

    # restore font defaults
    hrbrthemes::update_geom_font_defaults(family = "", size = 3.88,
                                          color = "black")
    GeomText$default_aes$face <- NULL
    GeomLabel$default_aes$face <- NULL
  }, envir = env)
}
