FROM rocker/geospatial

MAINTAINER Max Joseph maxwell.b.joseph@colorado.edu

RUN install2.r ggrepel

COPY . cft

WORKDIR cft

RUN R -e "devtools::install('.')"

# Install miniconda and python stuff for the user
USER rstudio

RUN R -e "reticulate::install_miniconda()"

RUN R -e "cft::install_py_deps(method='conda', python_version = 3)"

RUN R -e "reticulate::use_condaenv('cft', required=TRUE);xarray <- reticulate::import('xarray')"

RUN whoami

# switch back to root so that the rocker container still works
USER root

RUN whoami
