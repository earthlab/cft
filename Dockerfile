FROM rocker/geospatial

MAINTAINER Ty Tuff ty.tuff@colorado.edu

RUN install2.r ggrepel pbapply

COPY . cft

WORKDIR cft

RUN R -e "devtools::install('.')"
