### ---------------------------------------------------------------------------
### Calibrating the dynamic-dipLogR overlay against the plot it sits on.
###
### The overlay lets the reviewer hover over the copy-number log-ratio panel and
### read off a dipLogR. Its geometry (where the panel is) and its value axis
### (what a pixel row means) used to be hardcoded for one plot layout. The
### standard suite (2.0.8) and facets-suite-2n draw that panel differently:
###
###   standard  9.75x12 in / 850x999 px, y limits floor/ceiling of the segment
###             medians clamped to at least +/-3, raw cnlr on the axis, orange
###             line at dipLogR
###   2n        9x8 in, axis is cnlr MINUS dipLogR (so the plot is centred),
###             orange line at 0, and the limits are SYMMETRIC: +/-3 by default,
###             widened in integer steps to fit the adjusted segment medians, at
###             most +/-5. Points past the limit are squished to the edge and
###             drawn darkorange2 rather than dropped, so they do not move it.
###
### The 2n rule changed (it used to be an asymmetric floor/ceiling clamped to
### +/-2, computed from UNadjusted medians). Two symmetric conventions cannot be
### told apart from the image alone -- both centre the line -- so PNGs written by
### the older facets-suite-2n will read on the wrong scale until they are
### regenerated. The durable fix is for the plot to record its own limits.
###
### Both use ggplot's default 5% axis expansion and theme_bw's panel border.
### So: find the first panel's border rows in the PNG, take the y limits from
### the run's own segment medians, and use the orange line to decide which
### convention the plot follows. Nothing here guesses from the sample type.
### ---------------------------------------------------------------------------

#' The overlay geometry the app shipped with (standard plot, +/-3).
#' @export cnlr_calibration_legacy
cnlr_calibration_legacy <- function() {
  list(ok = FALSE, top = 0.031, height = 0.1835, v_top = 3, v_bottom = -3,
       offset = 0, adjusted = FALSE, method = "legacy")
}

#' Locate the first panel of a FACETS plot in its PNG.
#'
#' Ink (grey/black, low saturation) rows that span most of the image width are
#' panel borders; the first two bound the copy-number panel. The orange
#' reference line is looked for inside it.
#'
#' @param png_path a .png file
#' @return list(h, w, top_row, bottom_row, orange_row) in 1-based pixel rows
#'   (interior rows are top_row..bottom_row), or NULL when nothing is found
#' @export cnlr_panel_geometry
cnlr_panel_geometry <- function(png_path) {
  if (!requireNamespace("png", quietly = TRUE)) return(NULL)
  if (is.null(png_path) || length(png_path) != 1 || is.na(png_path) ||
      !file.exists(png_path) || dir.exists(png_path)) return(NULL)
  a <- tryCatch(png::readPNG(png_path), error = function(e) NULL)
  if (is.null(a)) return(NULL)
  if (length(dim(a)) == 2) a <- array(a, c(dim(a), 3))
  h <- dim(a)[1]; w <- dim(a)[2]
  R <- a[, , 1]; G <- a[, , 2]; B <- a[, , 3]
  lum <- (R + G + B) / 3
  sat <- pmax(R, G, B) - pmin(R, G, B)
  xs  <- round(w * 0.25):round(w * 0.75)

  # Panel borders: neutral ink across most of the middle half. Antialiasing
  # lightens a 1 px border to ~0.7 at 96 dpi, hence the loose luminance cut;
  # gridlines (grey92) stay above it, points and segments are saturated.
  ink <- lum[, xs] < 0.8 & sat[, xs] < 0.12
  rows <- which(rowMeans(ink) > 0.8)
  if (length(rows) < 2) return(NULL)
  grp <- split(rows, cumsum(c(1, diff(rows) > 1)))
  if (length(grp) < 2) return(NULL)
  top_row    <- max(grp[[1]]) + 1
  bottom_row <- min(grp[[2]]) - 1
  if (bottom_row - top_row < 10) return(NULL)

  # The sandybrown reference line (244,164,96), if any, inside the panel.
  ir <- top_row:bottom_row
  orange <- R[ir, xs] > 0.8 & G[ir, xs] > 0.5 & G[ir, xs] < 0.8 &
            B[ir, xs] < 0.55 & (R[ir, xs] - B[ir, xs]) > 0.3
  orows <- ir[rowMeans(orange) > 0.3]
  orange_row <- if (length(orows) > 0) {
    og <- split(orows, cumsum(c(1, diff(orows) > 1)))
    lens <- vapply(og, length, integer(1))
    mean(og[[which.max(lens)]])
  } else NA_real_

  list(h = h, w = w, top_row = top_row, bottom_row = bottom_row, orange_row = orange_row)
}

#' Calibrate the dynamic-dipLogR overlay for one plot.
#'
#' @param png_path the plot being displayed
#' @param cnlr_median the run's segment cnlr.median values (from its cncf); the
#'   plot's y limits are floor/ceiling of these, clamped per suite
#' @param dipLogR the run's dipLogR (needed to place the orange line for the
#'   unadjusted convention and to convert an adjusted axis back to a dipLogR)
#' @return list(ok, top, height, v_top, v_bottom, offset, adjusted, method):
#'   top/height are the panel interior as fractions of the image height;
#'   v_top/v_bottom the axis values at its top and bottom edges; the dipLogR a
#'   hovered row means is  v_top - frac * (v_top - v_bottom) + offset.
#' @export cnlr_plot_calibration
cnlr_plot_calibration <- function(png_path, cnlr_median = NULL, dipLogR = NA_real_) {
  geo <- cnlr_panel_geometry(png_path)
  if (is.null(geo)) return(cnlr_calibration_legacy())

  H   <- geo$bottom_row - geo$top_row + 1
  top <- (geo$top_row - 1) / geo$h
  height <- H / geo$h
  landscape <- geo$w > geo$h
  dip_known <- length(dipLogR) == 1 && !is.na(dipLogR) && is.finite(dipLogR)

  cm <- suppressWarnings(as.numeric(cnlr_median))
  cm <- cm[is.finite(cm)]
  have_data <- length(cm) > 0

  # The symmetric convention reads the medians in the space the plot draws them,
  # i.e. after the dipLogR shift; the asymmetric one uses them raw.
  cm_adj <- if (dip_known) cm - dipLogR else cm

  limits_for <- function(hp) {
    if (isTRUE(hp$symmetric)) {
      lim  <- if (have_data) min(hp$cap, max(hp$clamp, ceiling(max(abs(cm_adj))))) else hp$clamp
      ymin <- -lim
      ymax <-  lim
    } else {
      ymin <- if (have_data) min(floor(min(cm)), -hp$clamp) else -hp$clamp
      ymax <- if (have_data) max(ceiling(max(cm)), hp$clamp) else hp$clamp
    }
    r <- ymax - ymin
    c(ymax + 0.05 * r, ymin - 0.05 * r)   # ggplot's default 5% expansion
  }
  # The two conventions, preferred order set by the plot's shape.
  hyps <- list(
    list(name = "2n",       clamp = 3, cap = 5, symmetric = TRUE,  adjusted = TRUE),
    list(name = "standard", clamp = 3,          symmetric = FALSE, adjusted = FALSE))
  if (!landscape) hyps <- rev(hyps)

  result_for <- function(hp, method) {
    lim <- limits_for(hp)
    list(ok = TRUE, top = top, height = height, v_top = lim[1], v_bottom = lim[2],
         offset = if (hp$adjusted && dip_known) dipLogR else 0,
         adjusted = hp$adjusted, method = method)
  }

  # With an orange line we can test each convention: where would it be?
  if (!is.na(geo$orange_row)) {
    f0 <- (geo$orange_row - geo$top_row + 0.5) / H
    errs <- vapply(hyps, function(hp) {
      lim <- limits_for(hp)
      line_value <- if (hp$adjusted) 0 else if (dip_known) dipLogR else NA_real_
      if (is.na(line_value)) return(Inf)
      abs((lim[1] - line_value) / (lim[1] - lim[2]) - f0)
    }, numeric(1))
    # Take the FIRST convention (preferred by the plot's shape) that explains
    # the line, not the numerically closest one: near dipLogR = 0 both put the
    # line at the centre and pixel noise would otherwise decide, flipping a
    # standard plot onto the adjusted axis (or a 2n plot off it).
    fits <- which(is.finite(errs) & errs < 0.03)
    if (length(fits) > 0) {
      return(result_for(hyps[[fits[1]]], "orange-line"))
    }
    # No convention explains the line with these limits: trust the line for
    # the zero/dipLogR anchor and the panel for the scale of the preferred
    # convention, i.e. shift the limits so the line lands where it is drawn.
    hp  <- hyps[[1]]
    lim <- limits_for(hp)
    line_value <- if (hp$adjusted || !dip_known) 0 else dipLogR
    span  <- lim[1] - lim[2]
    v_top <- line_value + f0 * span
    return(list(ok = TRUE, top = top, height = height, v_top = v_top, v_bottom = v_top - span,
                offset = if (hp$adjusted && dip_known) dipLogR else 0,
                adjusted = hp$adjusted, method = "orange-line-shifted"))
  }

  # No reference line found: geometry from the image, values by convention.
  result_for(hyps[[1]], "borders-only")
}
