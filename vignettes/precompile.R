
# Precompiled vignettes that are long-running (requiring data downloads)

library(knitr)
knit("vignettes/cst-intro.Rmd.orig", "vignettes/cst-intro.Rmd")
figs <- list.files(pattern = ".png")
file.copy(figs, "vignettes/", overwrite = TRUE)
unlink(figs)
