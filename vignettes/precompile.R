
# Precompiled vignettes that are long-running (requiring data downloads)

library(knitr)
knit( "vignettes/firehose.Rmd")
figs <- list.files(pattern = ".png")
file.copy(figs, "vignettes/", overwrite = TRUE)
unlink(figs)

