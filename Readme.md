---
title: "Readme"
author: "PeiyuanXiao"
date: "2024-09-14"
output: github_markdown
---

<samp>RESEARCH COMPENDIUM</samp>

<h1><b><i>Quina lithic technology indicates diverse late-Pleistocene human dynamics in East Asia</i></b></h1>

<hr />

</p>

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

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

### 🚀 Data & Code Reproduction

The files hosted at <https://github.com/PeiyuanXiao/Longtan_raw_data> are the development versions. You can reproduce the analysis by following these steps:

1.  Clone the repository: `git clone https://github.com/PeiyuanXiao/Longtan_raw_data.git`
2.  Open `Lithic_Raw_Data_of_Longtan.Rproj` in RStudio.
3.  The main scripts are:
    -   `LT_CODE_MAIN_TEXT.R`: Main analysis and figures.
    -   `LT_CODE_SI.R`: Supplementary analysis.

### 💻 Computational Environment

-   **R Packages:** `tidyverse`, `readxl`, `ggpmisc`, `ggdist`, `cowplot`, `ggtext`, `plot3D`.
