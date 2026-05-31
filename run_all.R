# Reproduce the full Longtan Quina lithic analysis in a single step.
#
#   - Statistical results (MANOVA, Welch's ANOVA) and the descriptive-statistics
#     tables are printed to the console / log.
#   - Every plot-pane figure is captured, one page per plot, in
#     output/figures.pdf.
#   - The two explicitly saved 3-D plots from LT_CODE_SI.R are moved into output/.
#
# Run from the repository root:  Rscript run_all.R
# (this is also the default command of the Docker image)

dir.create("output", showWarnings = FALSE)

# Capture all printed figures into one PDF instead of the plot pane.
pdf(file.path("output", "figures.pdf"), width = 10, height = 8, onefile = TRUE)

scripts <- c("LT_CODE_MAIN_TEXT.R", "LT_CODE_SI.R", "LT_CODE_DS.R")
for (s in scripts) {
  message("\n================  Running ", s, "  ================")
  tryCatch(
    source(s, echo = TRUE, max.deparse.length = Inf),
    error = function(e) message("ERROR while running ", s, ": ", conditionMessage(e))
  )
}

# Close any graphics devices the scripts may have left open, plus our PDF.
while (dev.cur() > 1L) dev.off()

# Collect the PNG files written to the working directory by LT_CODE_SI.R.
for (f in c("3d_plot.png", "3d_plot_filtered_technological_types.png")) {
  if (file.exists(f)) file.rename(f, file.path("output", f))
}

message("\nDone. See the output/ directory for figures.")
