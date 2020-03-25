
all: manual vignette README.md website

manual: 
	Rscript -e "devtools::document()"

vignette: vignettes/cst-intro.Rmd.orig
	Rscript --vanilla vignettes/precompile.R

README.md: README.Rmd
	Rscript -e "knitr::knit('README.Rmd')"

website: 
	Rscript -e "pkgdown::build_site()"