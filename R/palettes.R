#' Color palette proposed by Okabe and Ito
#'
#' Two color palettes taken from the article "Color Universal Design" by Okabe
#' and Ito, http://jfly.iam.u-tokyo.ac.jp/color/. The variant
#' `palette_okabeito` contains a gray color, while `palette_okabeito_black`
#' contains black instead.
#'
#' @return A character vector of colors.
#' @export
palette_okabeito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                      "#D55E00", "#CC79A7", "#999999")

#' @rdname palette_okabeito
#' @export
palette_okabeito_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                            "#0072B2", "#D55E00", "#CC79A7", "#000000")


#' Color palette based on measr branding
#'
#' A color palette inspired by the hex sticker logo for the measr package.
#'
#' @return A character vector of colors.
#' @export
palette_measr <- c("#023047", "#D7263D", "#8ECAE6", "#219EBC", "#F3D3BD")


#' Custom color ramps
#'
#' These color ramp functions create color scales that can be used for making
#' ggplot2 plots and gt tables. Color ramps are based on the [palette_wjake]
#' color palette.
#'
#' @inheritParams grDevices::colorRamp
#' @param x Colors to pull from the color ramp. Numbers range from 0-1, which
#'   is a normalized sliding scale of the color ramp.
#'
#' @details
#' `make_color_pal` can be used to create a color ramp function for any set of
#' valid colors.
#'
#' `ramp_blue`, `ramp_yellow`, and `ramp_yelblu` are pre-made color ramps based
#' on the blue and yellow colors from the [palette_wjake] color palette.
#'
#' @name color_ramp

#' @rdname color_ramp
#' @export
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

#' @rdname color_ramp
#' @export
ramp_blue <- make_color_pal(c("#FFFFFF", "#8ECAE6", "#219EBC", "#023047"), bias = 1)
