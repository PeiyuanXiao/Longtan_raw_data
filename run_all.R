# Reproduce the full Longtan Quina lithic analysis in a single step.
#
#   - Statistical results (MANOVA, Welch's ANOVA) and the descriptive-statistics
#     tables are printed to the console / log.
#   - Every plot-pane figure is captured, one page per plot, in
#     analysis/output/figures.pdf.
#   - The two 3-D plots from LT_CODE_SI.R are written to analysis/output/.
#
# Run from the repository root:  Rscript run_all.R
# (this is also the default command of the Docker image)

out_dir <- "analysis/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Capture all printed figures into one PDF instead of the plot pane.
pdf(file.path(out_dir, "figures.pdf"), width = 10, height = 8, onefile = TRUE)

scripts <- file.path("analysis", "scripts",
                     c("LT_CODE_MAIN_TEXT.R", "LT_CODE_SI.R", "LT_CODE_DS.R"))
for (s in scripts) {
  message("\n================  Running ", s, "  ================")
  tryCatch(
    source(s, echo = TRUE, max.deparse.length = Inf),
    error = function(e) message("ERROR while running ", s, ": ", conditionMessage(e))
  )
}

# Close any graphics devices the scripts may have left open, plus our PDF.
while (dev.cur() > 1L) dev.off()

message("\nDone. See ", out_dir, "/ for the figures.")
