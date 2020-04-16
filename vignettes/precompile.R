
# Precompiled vignettes that are long-running (requiring data downloads)

library(knitr)
knit("vignettes/cft-intro.Rmd.orig", "vignettes/cft-intro.Rmd")
figs <- list.files(pattern = ".png")
file.copy(figs, "vignettes/", overwrite = TRUE)
unlink(figs)
