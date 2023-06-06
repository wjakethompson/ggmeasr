#' Personalized gt theme
#'
#' A custom theme for tables generated with [gt::gt].
#'
#' @param gt_dat A `gt_tbl` object, generated from [gt::gt].
#' @param bg_color The background color of the table cells.
#' @param ... Additional arguments passed to [gt::tab_options].
#'
#' @return An object of class `gt_tbl`.
#' @export
gt_theme_measr <- function(gt_dat, bg_color = "#FFFFFF", ...) {
  gt_dat %>%
    gt::opt_all_caps() %>%
    gt::opt_table_font(font = list(gt::google_font("Open Sans"),
                                   gt::default_fonts())) %>%
    gt::tab_options(column_labels.background.color = bg_color,
                    column_labels.border.top.width = gt::px(3),
                    column_labels.border.top.color = bg_color,
                    table.border.top.width = gt::px(1),
                    table.border.bottom.width = gt::px(1),
                    heading.align = "left",
                    heading.background.color = bg_color,
                    source_notes.padding = gt::px(10),
                    source_notes.border.lr.width = gt::px(0),
                    source_notes.font.size = 12,
                    source_notes.background.color = bg_color,
                    table.font.size = 16,
                    data_row.padding = gt::px(3),
                    table.background.color = bg_color,
                    ...) %>%
    gt::tab_style(style = gt::cell_borders(sides = "bottom",
                                           color = "transparent",
                                           weight = gt::px(2)),
                  locations = gt::cells_body(columns = gt::everything(),
                                             rows = nrow(gt_dat$`_data`))) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold", align = "center",
                                             v_align = "middle"),
                               gt::cell_borders(sides = "bottom",
                                                color = palette_measr[1],
                                                weight = gt::px(3))),
                  locations = gt::cells_column_labels(gt::everything())) %>%
    gt::tab_style(style = gt::cell_fill(color = bg_color),
                  locations = list(gt::cells_body(columns = gt::everything(),
                                                  rows = gt::everything())))
}
