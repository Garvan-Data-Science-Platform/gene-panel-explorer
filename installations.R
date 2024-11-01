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


# download public resources ----

## STRING DB ----

STRING_URL <- "https://stringdb-downloads.org/download/protein.links.v12.0/9606.protein.links.v12.0.txt.gz"
temp <- tempfile()
download.file(STRING_URL,temp)
unz(temp, "Data/STRING/9606.protein.links.v12.0.txt")
unlink(temp)

## Gene Ontology (GO) ----
GOA_URL <- "https://current.geneontology.org/annotations/goa_human.gaf.gz"
GO_BASIC_OBO_URL <- "http://current.geneontology.org/ontology/go-basic.obo"
temp <- tempfile()
download.file(GOA_URL,temp)
unz(temp, "Data/GO/goa_human.gaf")
unlink(temp)
temp <- tempfile()
download.file(GO_BASIC_OBO_URL,temp)
unlink(temp)

## HPO ----
#"Data/HPO/HPO_genes.tsv"

## OMIM ----
# Data/OMIM/mim2geneDedup.tsv"

## ClinVar ----
# CLINVAR_URL <- ""
# temp <- tempfile()
# download.file(CLINVAR_URL,temp)
# unz(temp, "Data/ClinVar/goa_human.gaf")
# unlink(temp)


