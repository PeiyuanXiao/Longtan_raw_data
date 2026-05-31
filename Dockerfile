# Reproducible computational environment for:
#   Ruan et al. (2025) "Quina lithic technology indicates diverse Late
#   Pleistocene human dynamics in East Asia", PNAS 122(14):e2418029122.
#
# The rocker/r-ver base pins R 4.4.2 and a dated CRAN snapshot (served as
# Linux binaries via the Posit Public Package Manager), so package versions
# are reproducible without a local R installation.
#
#   docker build -t longtan-quina .
#   docker run --rm -v "${PWD}/output:/home/project/output" longtan-quina
#
FROM rocker/r-ver:4.4.2

# System libraries required to build/run the R packages used here
# (tidyverse, ggtext/systemfonts, ragg, xml2, curl, plot3D, …).
RUN apt-get update && apt-get install -y --no-install-recommends \
        libxml2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install the exact R packages used by the analysis scripts.
RUN install2.r --error --skipinstalled --ncpus -1 \
        readxl \
        tidyverse \
        cowplot \
        ggdist \
        ggpmisc \
        ggtext \
        plot3D

# Copy the compendium (raw data + code) into the image.
WORKDIR /home/project
COPY . /home/project

# Default: run the whole analysis end-to-end. Console output holds the
# statistical results and descriptive tables; figures are written to output/.
CMD ["Rscript", "run_all.R"]
