#!/usr/bin/env Rscript
### Fixture tests for the dynamic-dipLogR overlay calibration: synthetic
### ggplot PNGs drawn the way each suite draws its copy-number panel.
###
### Run: Rscript tests/test_cnlr_calibration.R

suppressPackageStartupMessages(suppressWarnings(library(ggplot2)))
repo <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
source(file.path(repo, "R", "plot_calibration.R"))

n_pass <- 0; n_fail <- 0
check <- function(label, cond) {
  if (isTRUE(cond)) { n_pass <<- n_pass + 1; cat("PASS:", label, "\n") }
  else { n_fail <<- n_fail + 1; cat("FAIL:", label, "\n") }
}

# A single copy-number panel drawn as the suites draw it: theme_bw, pretty
# breaks over floor/ceiling limits with a clamp, sandybrown reference line.
draw <- function(path, w, h, units, res, clamp, adjusted, dip, segs, line = TRUE,
                 symmetric = adjusted, cap = 5) {
  y <- if (adjusted) segs - dip else segs
  if (symmetric) {                      # the 2n rule: symmetric, default +/-3, capped
    lim <- min(cap, max(clamp, ceiling(max(abs(y)))))
    ymin <- -lim; ymax <- lim
  } else {
    ymin <- min(floor(min(segs)), -clamp); ymax <- max(ceiling(max(segs)), clamp)
  }
  p <- ggplot(data.frame(x = seq_along(y), y = y)) +
    geom_point(aes(x, y), col = "#0080FF", size = .4) +
    scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(ymin, ymax)) +
    labs(x = NULL, y = "Copy number\nlog ratio") + theme_bw() + ggtitle("T | cval=100")
  if (line) p <- p + geom_hline(yintercept = if (adjusted) 0 else dip, color = "sandybrown", linewidth = .8)
  suppressWarnings({ png(path, width = w, height = h, units = units, res = res); print(p); dev.off() })
  path
}
value_at_orange <- function(calib, path) {
  g <- cnlr_panel_geometry(path)
  fr <- (g$orange_row - g$top_row + 0.5) / (g$bottom_row - g$top_row + 1)
  calib$v_top - fr * (calib$v_top - calib$v_bottom) + calib$offset
}

segs <- c(-0.4, 0.1, 0.6, 1.2, -1.1)

std <- draw(tempfile(fileext = ".png"), 850, 999, "px", 96, 3, FALSE, -0.09, segs)
g <- cnlr_panel_geometry(std)
check("geometry: panel borders found in a 96 dpi plot", !is.null(g) && g$top_row < g$bottom_row)
check("geometry: the reference line is found inside the panel",
      !is.na(g$orange_row) && g$orange_row > g$top_row && g$orange_row < g$bottom_row)
cs <- cnlr_plot_calibration(std, segs, -0.09)
check("standard: limits are +/-3 with ggplot's 5% expansion",
      cs$ok && isTRUE(all.equal(cs$v_top, 3.3)) && isTRUE(all.equal(cs$v_bottom, -3.3)))
check("standard: raw cnlr axis, no offset", !cs$adjusted && cs$offset == 0)
check("standard: hovering the reference line reads the dipLogR",
      abs(value_at_orange(cs, std) - (-0.09)) < 0.03)

n2 <- draw(tempfile(fileext = ".png"), 9, 8, "in", 300, 3, TRUE, -0.29, segs)
cn <- cnlr_plot_calibration(n2, segs, -0.29)
check("2n: limits are a symmetric +/-3 with expansion",
      cn$ok && isTRUE(all.equal(cn$v_top, 3.3)) && isTRUE(all.equal(cn$v_bottom, -3.3)))
check("2n: adjusted axis, offset is the run's dipLogR", cn$adjusted && cn$offset == -0.29)
check("2n: hovering the reference line reads the dipLogR", abs(value_at_orange(cn, n2) - (-0.29)) < 0.03)
check("2n: geometry differs from the standard plot's", abs(cn$top - cs$top) > 0.001 || abs(cn$height - cs$height) > 0.001)

wide <- draw(tempfile(fileext = ".png"), 9, 8, "in", 300, 3, TRUE, 0.15, c(-3.4, 0.2, 2.7))
cw <- cnlr_plot_calibration(wide, c(-3.4, 0.2, 2.7), 0.15)
check("2n: a data range beyond the default widens the limits symmetrically (+/-4)",
      isTRUE(all.equal(cw$v_top, 4.4)) && isTRUE(all.equal(cw$v_bottom, -4.4)))
check("2n: ...and the reference line still reads the dipLogR", abs(value_at_orange(cw, wide) - 0.15) < 0.03)

capped <- draw(tempfile(fileext = ".png"), 9, 8, "in", 300, 3, TRUE, 0, c(-0.3, 9.1))
cc <- cnlr_plot_calibration(capped, c(-0.3, 9.1), 0)
check("2n: the limits stop widening at the +/-5 cap",
      isTRUE(all.equal(cc$v_top, 5.5)) && isTRUE(all.equal(cc$v_bottom, -5.5)))

near0 <- draw(tempfile(fileext = ".png"), 850, 999, "px", 96, 3, FALSE, -0.02, segs)
cz <- cnlr_plot_calibration(near0, segs, -0.02)
check("standard with dipLogR near 0: stays on the raw axis (shape breaks the tie, not pixel noise)",
      cz$method == "orange-line" && !cz$adjusted && cz$offset == 0)
czn <- cnlr_plot_calibration(draw(tempfile(fileext = ".png"), 9, 8, "in", 300, 3, TRUE, 0.02, segs), segs, 0.02)
check("2n with dipLogR near 0: stays on the adjusted axis", czn$method == "orange-line" && czn$adjusted && czn$offset == 0.02)

noline <- draw(tempfile(fileext = ".png"), 850, 999, "px", 96, 3, FALSE, -0.09, segs, line = FALSE)
cl <- cnlr_plot_calibration(noline, segs, -0.09)
check("no reference line: borders still calibrate, convention by shape (portrait -> standard)",
      cl$ok && cl$method == "borders-only" && !cl$adjusted && cl$v_top == 3.3)
cl2 <- cnlr_plot_calibration(draw(tempfile(fileext = ".png"), 9, 8, "in", 300, 3, TRUE, -0.29, segs, line = FALSE), segs, -0.29)
check("no reference line: landscape -> 2n convention", cl2$adjusted && cl2$v_top == 3.3 && cl2$offset == -0.29)

check("no data: limits fall back to the default",
      cnlr_plot_calibration(n2, NULL, -0.29)$v_top == 3.3)
leg <- cnlr_plot_calibration(file.path(tempdir(), "nope.png"), segs, 0)
check("missing file: the classic geometry, flagged not ok", !leg$ok && leg$method == "legacy" && leg$v_top == 3)

cat("\n", n_pass, "passed,", n_fail, "failed\n")
if (n_fail > 0) quit(status = 1)
