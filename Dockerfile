# Reproducible computational environment for:
#   Ruan et al. (2025) "Quina lithic technology indicates diverse Late
#   Pleistocene human dynamics in East Asia", PNAS 122(14):e2418029122.
#
# renv.lock pins the exact version of every package. They are installed into the
# image's system library so the analysis runs with a single `docker run` — no
# renv activation or network access is needed at runtime.
#
#   docker build -t longtan-quina .
#   docker run --rm -v "${PWD}/analysis/output:/home/project/analysis/output" longtan-quina
#
FROM rocker/r-ver:4.5.2

# System libraries needed to install/run the R packages used here (tidyverse,
# ggtext/systemfonts/ragg, xml2, curl, …). Tcl/Tk is required by tcltk, a
# dependency of misc3d (pulled in by plot3D).
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
        libtcl8.6 \
        libtk8.6 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/project

# Install the exact package versions from renv.lock into the system library.
# Copying only the lockfile first lets Docker cache this (slow) layer across
# code changes. Binaries come from the Posit Public Package Manager for the
# image's own Ubuntu release; renv falls back to source if a binary is missing.
# The cache is disabled so the system library holds real, self-contained copies.
COPY renv.lock renv.lock
RUN . /etc/os-release \
 && PPM="https://packagemanager.posit.co/cran/__linux__/${VERSION_CODENAME}/latest" \
 && export RENV_CONFIG_REPOS_OVERRIDE="$PPM" RENV_CONFIG_CACHE_ENABLED=FALSE \
 && R --vanilla -e "install.packages('renv', repos='${PPM}'); renv::restore(lockfile = 'renv.lock', library = '/usr/local/lib/R/site-library', prompt = FALSE)"

# Copy the rest of the compendium (raw data + scripts + run_all.R), then drop the
# renv auto-activation profile so the runtime R simply uses the system library.
COPY . /home/project
RUN rm -f /home/project/.Rprofile

# Default: run the whole analysis end-to-end. Console output holds the
# statistical results and tables; figures are written to analysis/output/.
CMD ["Rscript", "run_all.R"]
