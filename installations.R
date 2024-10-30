# installations ----
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.19")
packages <- c("shiny", "Cairo", "shinycssloaders", "shinybusy",
              "bslib", "colorRamp2", "dendextend",
              "readr", "dplyr", "cluster",
              "ggplot2", "ggrepel", "ggthemes", "ggradar", "gridExtra", "colorRamp2", "factoextra",
              "VennDiagram", "remotes", "scales", "ComplexHeatmap", "viridis", "bslib", "httr", "jsonlite",
              "ggvenn", "UpSetR", "ggVennDiagram", "ontologyIndex", "tidyverse", "dendextend")
BiocManager::install(packages)
remotes::install_github("ricardo-bion/ggradar")
renv::snapshot()
