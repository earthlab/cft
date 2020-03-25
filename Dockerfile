FROM rocker/geospatial

MAINTAINER Max Joseph maxwell.b.joseph@colorado.edu

RUN wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh \
  && bash miniconda.sh -b -p $HOME/miniconda \
  && . ~/miniconda/etc/profile.d/conda.sh \
  && hash -r \
  && conda config --set always_yes yes --set changeps1 no \
  && conda update -q conda \
  && conda info -a \
  && rm miniconda.sh

RUN R -e "devtools::install_github('earthlab/cst')"

RUN R -e "cst::install_py_deps(method='conda', python_version = 3)"