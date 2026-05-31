<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>Quina lithic technology indicates diverse late-Pleistocene human dynamics in East Asia</i></b></h1>

<hr />

</p>

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![R >= 4.1](https://img.shields.io/badge/R-%3E%3D4.1-blue.svg)](https://www.r-project.org/)

This repository contains the data and code for our paper published in **PNAS**:

> **Ruan, Q. J., Li, H., Xiao, P. Y., Li, B., Monod, H., Sumner, A., ... & Delpiano, D. (2025). Quina lithic technology indicates diverse Late Pleistocene human dynamics in East Asia. *Proceedings of the National Academy of Sciences*, 122(14), e2418029122.**

------------------------------------------------------------------------

### 👥 Authors and Affiliations

**Qi-Jun Ruan**<sup>a,b†</sup>, **Hao Li**<sup>a,c†</sup>✉, \***Pei-Yuan Xiao**<sup>a,c</sup>[<img src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" alt="ORCID iD" width="16" height="16"/>](https://orcid.org/0009-0000-9733-5875)✉,**Bo Li**<sup>d</sup>✉, **Hélène Monod**<sup>e,f</sup>, **Alexandra Sumner**<sup>g</sup>, **Ke-Liang Zhao**<sup>h</sup>, **Jian-Hui Liu**<sup>b</sup>, **Zhen-Xiu Jia**<sup>a</sup>, **Chun-Xin Wang**<sup>i</sup>, **An-Chuan Fan**<sup>i</sup>, **Marie-Hélène Moncel**<sup>j</sup>, **Ben Marwick**<sup>k</sup>, **Marco Peresani**<sup>l,m</sup>, **You-Ping Wang**<sup>n,o</sup>, **Fa-Hu Chen**<sup>a,c</sup>, **Davide Delpiano**<sup>l</sup>✉

-   <sup>a</sup> *State Key Laboratory of Tibetan Plateau Earth System, Resources and Environment (TPESER), Institute of Tibetan Plateau Research, CAS, Beijing, China.*
-   <sup>b</sup> *Yunnan Provincial Institute of Cultural Relics and Archaeology, Kunming, China.*
-   <sup>c</sup> *University of Chinese Academy of Sciences, Beijing, China.*
-   <sup>d</sup> *Centre for Archaeological Science, University of Wollongong, NSW, Australia.*
-   <sup>e</sup> *Universitat Rovira i Virgili, Department of History and Art History, Tarragona, Spain.*
-   <sup>f</sup> *UMR7194, Natural History of Prehistoric Man, CNRS, National Museum of Natural History, Paris.*
-   <sup>g</sup> *Department of Anthropology, DePaul University, Chicago, IL, USA.*
-   <sup>h</sup> *Institute of Vertebrate Paleontology and Paleoanthropology, CAS, Beijing, China.*
-   <sup>i</sup> *Department for the History of Science and Scientific Archaeology, USTC, Hefei, China.*
-   <sup>j</sup> *UMR 7194 CNRS-National Museum of Natural History, Paris, France.*
-   <sup>k</sup> *Department of Anthropology, University of Washington, Seattle, USA.*
-   <sup>l</sup> *Department of Human Studies, Prehistoric and Anthropological Science Unit, University of Ferrara, Italy.*
-   <sup>m</sup> *CNR-Institute of Environmental Geology and Geoengineering, Milan, Italy.*
-   <sup>n</sup> *School of Archaeology and Museology, Peking University, Beijing, China.*

**✉ Corresponding Authors:** \* Hao Li ([lihao\@itpcas.ac.cn](mailto:lihao@itpcas.ac.cn)) \* Pei-yuan Xiao ([xiaopeiyuan\@itpcas.ac.cn](mailto:xiaopeiyuan@itpcas.ac.cn)) \* Bo Li ([bli\@uow.edu.au](mailto:bli@uow.edu.au)) \* Davide Delpiano ([dlpdvd\@unife.it](mailto:dlpdvd@unife.it))

🔧 **Maintainers:** [Pei-yuan Xiao](mailto:xiaopeiyuan@itpcas.ac.cn) & [Ben Marwick](mailto:bmarwick@uw.edu)

------------------------------------------------------------------------

### 📝 Abstract

The Late Pleistocene of Eurasia is key for understanding interactions between early modern humans and different types of archaic human groups. During this period, lithic technology shows more diversity and complexity, likely indicating flexible adaptative strategies. However, cultural variability as expressed by technological types remains vague in large parts of eastern Eurasia, like in China. Here we report a complete Quina technological system identified from the study of the Longtan site in Southwest China. This site has been securely dated to ca. 60–50 thousand years ago (ka), with compelling evidence of core exploitation, production of large and thick flakes, shaping and maintenance of scrapers exhibiting the whole Quina concept, typical of contemporary European Middle Paleolithic technologies developed by Neanderthal groups adapted to climatic oscillations during Marine Isotope Stage (MIS) 4 and early MIS 3. The finding of a Quina lithic assemblage in China not only demonstrates the existence of a Middle Paleolithic technology in the region, but also shows large-scale analogies with Neanderthal behaviors in western Europe. Longtan substantially extends the geographic distribution of this technical behavior in East Asia. Although its origin remains unclear, implications for Pleistocene hominin dispersal and adaptation to diverse ecological settings are considered. The Longtan lithic evidence also provides perspectives for understanding the cultural evolutionary situation before the large-scale arrivals of early modern humans in East Asia predating \~45 ka.

### 🔑 Keywords

Middle Paleolithic; fluvial terrace; early MIS 3; Late Pleistocene; hominins

------------------------------------------------------------------------

### 🗂️ Repository structure

| File | Description |
|---|---|
| `LT_CODE_MAIN_TEXT.R` | Statistical tests (MANOVA, Welch's ANOVA) and figures reported in the **main text**. |
| `LT_CODE_SI.R` | Analyses and figures for the **Supplementary Information** (cores, flakes, tools, size scatterplots, site-to-river distance, taphonomy, 3-D artefact coordinates). |
| `LT_CODE_DS.R` | Descriptive-statistics tables for the **supplementary datasets** (cores, flakes, tools, chunks & debris). |
| `Longtan_lithic_tools.xlsx` | Retouched tools — sheet 1 Quina scrapers, sheet 2 other scrapers, sheet 3 notches/denticulates. |
| `Longtan_lithic_data_flakes.xlsx` | Flakes — sheet 1 technological flakes, sheet 2 resharpening flakes, sheet 3 other flakes. |
| `Longtan_lithic_data_cores.xlsx` | Core attributes. |
| `Longtan_lithic_data_waste_products.xlsx` | Chunks and debris (knapping waste). |
| `Longtan_lithic_data_coordinates.xlsx` | 3-D piece-plotted coordinates of artefacts. |
| `Site_river_distance.xlsx` | Distances from comparative sites to the nearest river. |
| `LT_taphonomic_information.xlsx` | Taphonomic observations on the assemblage. |
| `Lithic_Raw_Data_of_Longtan.Rproj` | RStudio project file (sets the working directory to the repo root). |
| `DESCRIPTION` | Machine-readable list of R package dependencies. |
| `Dockerfile` | Recipe for a fully reproducible computational environment. |

------------------------------------------------------------------------

### 🚀 How to reproduce

The files hosted at <https://github.com/PeiyuanXiao/Longtan_raw_data> are the development version. Choose **one** of the three routes below. All scripts read the `.xlsx` files using **relative paths**, so they must be run with the repository root as the working directory — opening the `.Rproj` (Option A) or using Docker (Option C) guarantees this.

#### Option A — RStudio (recommended for most users)

1.  Clone the repository:
    ```sh
    git clone https://github.com/PeiyuanXiao/Longtan_raw_data.git
    cd Longtan_raw_data
    ```
2.  Open `Lithic_Raw_Data_of_Longtan.Rproj` in RStudio (this sets the working directory automatically).
3.  Install the dependencies once:
    ```r
    install.packages(c("readxl", "tidyverse", "cowplot", "ggdist",
                       "ggpmisc", "ggtext", "plot3D"))
    ```
4.  Run the scripts:
    -   `LT_CODE_MAIN_TEXT.R` — main-text statistics and figures.
    -   `LT_CODE_SI.R` — supplementary analyses and figures.
    -   `LT_CODE_DS.R` — supplementary descriptive-statistics tables.

#### Option B — `renv` (recommended for exact version pinning)

A lockfile pins every package to the version used for the paper.

```r
install.packages("renv")
renv::restore()   # installs the exact versions recorded in renv.lock
```

> **Note:** `renv.lock` is generated by the maintainers on the analysis machine with
> `renv::init()` followed by `renv::snapshot()`. If `renv.lock` is not yet present in
> the repository, fall back to Option A or build the Docker image (Option C).

#### Option C — Docker (recommended for a guaranteed-clean environment)

The [`Dockerfile`](Dockerfile) pins the R version (4.4.2) *and* the package
versions, independent of your local setup.

```sh
# 1. Build the image (R + all packages baked in)
docker build -t longtan-quina .

# 2. Run the full analysis end-to-end via run_all.R.
#    Figures are written to ./output on the host (figures.pdf + the 3-D PNGs).
docker run --rm -v "${PWD}/output:/home/project/output" longtan-quina
```

`${PWD}` works in bash and PowerShell; in Windows `cmd` use `%cd%` instead.
For interactive work, use Option A (RStudio) locally.

------------------------------------------------------------------------

### 📤 Outputs

-   **Console:** results of the statistical tests (MANOVA, Welch's ANOVA) and the descriptive-statistics tables.
-   **Plot pane / `output/`:** the main-text and SI figures.
-   **Files written to the working directory:** `3d_plot.png` and `3d_plot_filtered_technological_types.png` (from `LT_CODE_SI.R`). These generated images are git-ignored.

------------------------------------------------------------------------

### 💻 Computational environment

-   **R:** developed and tested under R ≥ 4.1 (the Docker image pins R 4.4.x).
-   **R packages:**

    | Package | Role |
    |---|---|
    | `tidyverse` | data wrangling and `ggplot2` graphics |
    | `readxl` | reading the `.xlsx` raw-data files |
    | `cowplot` | composing multi-panel figures |
    | `ggdist` | distribution / uncertainty visualisations |
    | `ggpmisc` | regression annotations on plots |
    | `ggtext` | rich-text (markdown/HTML) plot labels |
    | `plot3D` | 3-D scatter plots of artefact coordinates |

To capture your own session for the record, run `sessionInfo()` after sourcing the scripts.

------------------------------------------------------------------------

### 📄 License

Code and data in this repository are released under the **Creative Commons Attribution 4.0 International (CC BY 4.0)** license — see the [`LICENSE`](LICENSE) file. You are free to share and adapt the material for any purpose, provided you give appropriate credit by citing the paper above.

------------------------------------------------------------------------

### 🐛 Issues

Found a problem reproducing the analysis? Please open an issue at
<https://github.com/PeiyuanXiao/Longtan_raw_data/issues> or contact the maintainers.
