suppressPackageStartupMessages(library(ggplot2))

test_that("theme measr works", {
  df <- data.frame(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)

  plot <- ggplot(df, aes(x, y, colour = z)) +
    geom_point(size = 10) +
    facet_wrap(~ a)

  vdiffr::expect_doppelganger("theme_atlas", plot + theme_measr())
})

test_that("setting the theme works", {
  local_theme(font = "", continuous = "mako", discrete = palette_measr)
  thm <- theme_get()

  expect_equal(thm$text$size, 11.5)
  expect_equal(thm$plot.title$size, 18)
  expect_equal(thm$plot.subtitle$size, 12)
  expect_equal(thm$strip.text$size, 12)
  expect_equal(thm$plot.caption$size, 9)
  expect_equal(thm$axis.title$size, 9)
})

test_that("setting continuous fill works", {
  df_f <- data.frame(x = rep(1:3, times = 3), y = rep(1:3, each = 3), z = 1:9)

  # default theme
  standard_cont_fill <- ggplot(df_f, aes(x, y, fill = z)) +
    geom_tile()
  standard_binn_fill <- ggplot(df_f, aes(x, y, fill = z)) +
    geom_tile() +
    scale_fill_binned()
  vdiffr::expect_doppelganger("standard-cont-fill", standard_cont_fill)
  vdiffr::expect_doppelganger("standard-binn-fill", standard_binn_fill)


  # test continuous fill
  local_theme(font = "Open Sans", continuous = "mako",
              discrete = palette_measr)
  mako_cont_fill <- ggplot(df_f, aes(x, y, fill = z)) +
    geom_tile()
  mako_binn_fill <- ggplot(df_f, aes(x, y, fill = z)) +
    geom_tile() +
    scale_fill_binned()
  vdiffr::expect_doppelganger("mako-cont-fill", mako_cont_fill)
  vdiffr::expect_doppelganger("mako-binn-fill", mako_binn_fill)

  local_theme(font = "Open Sans", continuous = "plasma",
              discrete = palette_measr)
  plasma_cont_fill <- ggplot(df_f, aes(x, y, fill = z)) +
    geom_tile()
  plasma_binn_fill <- ggplot(df_f, aes(x, y, fill = z)) +
    geom_tile() +
    scale_fill_binned()
  vdiffr::expect_doppelganger("plasma-cont-fill", plasma_cont_fill)
  vdiffr::expect_doppelganger("plasma-binn-fill", plasma_binn_fill)
})

test_that("setting continuous color works", {
  df_c <- data.frame(x = rep(1:3, times = 3), y = rep(1:3, each = 3), z = 1:9)

  # default theme
  standard_cont_colr <- ggplot(df_c, aes(x, y, color = z)) +
    geom_point(size = 10)
  standard_binn_colr <- ggplot(df_c, aes(x, y, color = z)) +
    geom_point(size = 10) +
    scale_color_binned()
  vdiffr::expect_doppelganger("standard-cont-colr", standard_cont_colr)
  vdiffr::expect_doppelganger("standard-binn-colr", standard_binn_colr)


  # test continuous color
  local_theme(font = "Open Sans", continuous = "viridis",
              discrete = palette_measr)
  viridis_cont_colr <- ggplot(df_c, aes(x, y, color = z)) +
    geom_point(size = 10)
  viridis_binn_colr <- ggplot(df_c, aes(x, y, color = z)) +
    geom_point(size = 10) +
    scale_color_binned()
  vdiffr::expect_doppelganger("viridis-cont-colr", viridis_cont_colr)
  vdiffr::expect_doppelganger("viridis-binn-colr", viridis_binn_colr)

  local_theme(font = "Open Sans", continuous = "magma",
              discrete = palette_measr)
  magma_cont_colr <- ggplot(df_c, aes(x, y, color = z)) +
    geom_point(size = 10)
  magma_binn_colr <- ggplot(df_c, aes(x, y, color = z)) +
    geom_point(size = 10) +
    scale_color_binned()
  vdiffr::expect_doppelganger("magma-cont-colr", magma_cont_colr)
  vdiffr::expect_doppelganger("magma-binn-colr", magma_binn_colr)
})

test_that("setting discrete scale works", {
  df_d <- data.frame(x = 1:5, y = 1:5, z = letters[1:5])

  standard_disc_colr <- ggplot(df_d, aes(x, y, color = z)) +
    geom_text(aes(label = z), size = 10)
  standard_disc_fill <- ggplot(df_d, aes(x, y, fill = z)) +
    geom_label(aes(label = z), size = 10)
  vdiffr::expect_doppelganger("standard-disc-colr", standard_disc_colr)
  vdiffr::expect_doppelganger("standard-disc-fill", standard_disc_fill)

  # test okabe ito
  local_theme(font = "Open Sans", continuous = "mako",
              discrete = palette_okabeito)
  okabeito_disc_colr <- ggplot(df_d, aes(x, y, color = z)) +
    geom_text(aes(label = z), size = 10)
  okabeito_disc_fill <- ggplot(df_d, aes(x, y, fill = z)) +
    geom_label(aes(label = z), size = 10)
  vdiffr::expect_doppelganger("okabeito-disc-colr", okabeito_disc_colr)
  vdiffr::expect_doppelganger("okabeito-disc-fill", okabeito_disc_fill)

  # test measr
  local_theme(font = "Open Sans", continuous = "mako",
              discrete = palette_measr)
  measr_disc_colr <- ggplot(df_d, aes(x, y, color = z)) +
    geom_text(aes(label = z), size = 10)
  measr_disc_fill <- ggplot(df_d, aes(x, y, fill = z)) +
    geom_label(aes(label = z), size = 10)
  vdiffr::expect_doppelganger("measr-disc-colr", measr_disc_colr)
  vdiffr::expect_doppelganger("measr-disc-fill", measr_disc_fill)
})
