# Reproducible computational environment for:
#   Ruan et al. (2025) "Quina lithic technology indicates diverse Late
#   Pleistocene human dynamics in East Asia", PNAS 122(14):e2418029122.
#
# The R version and every package version are pinned by renv.lock and restored
# with renv::restore(), so the environment is reproducible independently of
# your local setup.
#
#   docker build -t longtan-quina .
#   docker run --rm -v "${PWD}/analysis/output:/home/project/analysis/output" longtan-quina
#
FROM rocker/r-ver:4.5.2

# System libraries needed to install/run the R packages used here
# (tidyverse, ggtext/systemfonts/ragg, xml2, curl, plot3D, …).
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

WORKDIR /home/project

# Restore the exact package versions recorded in renv.lock. Copying only the
# renv metadata first lets Docker cache this (slow) layer across code changes.
# Binaries are pulled from the Posit Public Package Manager for the image's own
# Ubuntu release; renv falls back to source if a binary is unavailable (hence
# the -dev system libraries installed above).
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
RUN . /etc/os-release \
 && export RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/${VERSION_CODENAME}/latest" \
 && R -e "renv::restore()"

# Copy the rest of the compendium (raw data + scripts + run_all.R).
COPY . /home/project

# Default: run the whole analysis end-to-end. Console output holds the
# statistical results and tables; figures are written to analysis/output/.
CMD ["Rscript", "run_all.R"]
