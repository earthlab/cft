
all: manual vignettes/cst-intro.Rmd README.md

manual: 
	Rscript -e "devtools::document()"

vignettes/cst-intro.Rmd: vignettes/cst-intro.Rmd.orig
	Rscript --vanilla vignettes/precompile.R

README.md: README.Rmd
	Rscript -e "knitr::knit('README.Rmd')"
