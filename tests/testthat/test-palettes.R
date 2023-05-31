suppressPackageStartupMessages(library(ggplot2))

test_that("discrete scales work", {
  mpg <- mpg[mpg$manufacturer %in% c("audi", "dodge", "ford", "jeep"), ]

  disc_fill_base <- ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
    geom_bar(show.legend = FALSE) +
    theme_minimal()

  disc_color_base <- ggplot(mpg, aes(x = hwy, y = displ, color = factor(cyl))) +
    geom_point(size = 4) +
    theme_minimal()

  okabeito_fill <- disc_fill_base +
    scale_fill_discrete(type = palette_okabeito)

  okabeito_colr <- disc_color_base +
    scale_color_discrete(type = palette_okabeito)

  measr_fill <- disc_fill_base +
    scale_fill_discrete(type = palette_measr)

  measr_colr <- disc_color_base +
    scale_color_discrete(type = palette_measr)

  vdiffr::expect_doppelganger("okabeito-disc-fill", okabeito_fill)
  vdiffr::expect_doppelganger("okabeito-disc-colr", okabeito_colr)
  vdiffr::expect_doppelganger("measr-disc-fill", measr_fill)
  vdiffr::expect_doppelganger("measr-disc-colr", measr_colr)
})
