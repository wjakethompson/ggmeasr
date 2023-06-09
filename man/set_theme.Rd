% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme-measr.R
\name{set_theme}
\alias{set_theme}
\title{Set global plot theme options}
\usage{
set_theme(
  base_family = "Open Sans",
  v_scale = "mako",
  d_scale = palette_measr,
  ...
)
}
\arguments{
\item{base_family}{Base font family.}

\item{v_scale}{Viridis scale for continuous variables. See
\link[ggplot2:scale_viridis]{ggplot2::scale_colour_viridis_c}.}

\item{d_scale}{Discrete color scale to use.}

\item{...}{Additional parameters passed to \code{\link[=theme_measr]{theme_measr()}}.}
}
\value{
None. Called for side effects.
}
\description{
Calling this function will set \code{\link[=theme_measr]{theme_measr()}} as the default theme for all
plots created with ggplot2. Additionally, new default color scales are
specified to override the ggplot2 defaults for continuous, binned, and
discrete scales.
}
