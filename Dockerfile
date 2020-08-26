FROM rocker/geospatial

MAINTAINER Max Joseph maxwell.b.joseph@colorado.edu

RUN install2.r ggrepel pbapply

COPY . cft

WORKDIR cft

RUN R -e "devtools::install('.')"
