
all: manual vignettes/cft-intro.Rmd README.md

manual: 
	Rscript -e "devtools::document()"

vignettes/cft-intro.Rmd: vignettes/cft-intro.Rmd.orig
	Rscript --vanilla vignettes/precompile.R

README.md: README.Rmd
	Rscript -e "knitr::knit('README.Rmd')"
