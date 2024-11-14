# installations ----

# This project uses renv to manage package versions.
# Restore the packages from renv lockfile

# check library status
renv::status()

# restore lockfile
renv::restore()

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.19")
# packages <- c("shiny", "Cairo", "shinycssloaders", "shinybusy",
#               "bslib", "colorRamp2", "dendextend",
#               "readr", "dplyr", "cluster",
#               "ggplot2", "ggrepel", "ggthemes", "ggradar", "gridExtra", "colorRamp2", "factoextra",
#               "VennDiagram", "remotes", "scales", "ComplexHeatmap", "viridis", "bslib", "httr", "jsonlite",
#               "ggvenn", "UpSetR", "ggVennDiagram", "ontologyIndex", "tidyverse", "dendextend")
# BiocManager::install(packages)
# remotes::install_github("ricardo-bion/ggradar")
# #renv::snapshot()

library(httr)


# download public resources ----

## STRING DB ----

# STRING_URL <- "https://stringdb-downloads.org/download/protein.links.v12.0/9606.protein.links.v12.0.txt.gz"
# temp <- tempfile()
# download.file(STRING_URL,temp)
# unz(temp, "Data/STRING/9606.protein.links.v12.0.txt")
# unlink(temp)

STRING_URL <- "https://stringdb-downloads.org/download/protein.physical.links.detailed.v12.0/9606.protein.physical.links.detailed.v12.0.txt.gz"
temp <- tempfile()
download.file(STRING_URL,temp)
unz(temp, "Data/STRING/9606.protein.physical.links.detailed.v12.0.txt") # not working? did manually
unlink(temp)

STRING_INFO_URL <- "https://stringdb-downloads.org/download/protein.info.v12.0/9606.protein.info.v12.0.txt.gz"
temp <- tempfile()
download.file(STRING_INFO_URL,temp)
unz(temp, "Data/STRING/9606.protein.info.v12.0.txt")
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
# documentation: https://obophenotype.github.io/human-phenotype-ontology/annotations/genes_to_disease/

# (if the links expire) Manual method: download these reference files from the HPO website and
# place them in Data/HPO
# from here: https://hpo.jax.org/data/annotations
# "Genes to Disease" --> "Data/HPO/genes_to_disease.txt"
# "Phenotype to Genes" --> "Data/HPO/phenotype_to_genes.txt"

#url <- "https://hpo.jax.org/data/annotations#:~:text=GENES%20TO-,DISEASE,-License"
url <- "https://objects.githubusercontent.com/github-production-release-asset-2e65be/41063438/4ba584a4-c65a-48a0-a504-fcde80a4eaf8?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=releaseassetproduction%2F20241114%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20241114T231730Z&X-Amz-Expires=300&X-Amz-Signature=86f310c6abb88d76f9856f1adb7ae6731974e285ab029f6abaa5653ec8fa8fe8&X-Amz-SignedHeaders=host&response-content-disposition=attachment%3B%20filename%3Dgenes_to_disease.txt&response-content-type=application%2Foctet-stream"
destfile <- "Data/HPO/genes_to_disease_test.txt"
response <- GET(url, write_disk(destfile, overwrite = TRUE))

if (response$status_code == 200) {
  message("File downloaded successfully.")
} else {
  message("Failed to download the file. Status code: ", response$status_code)
}

# https://hpo.jax.org/data/annotations#:~:text=download-,PHENOTYPE,-TO%20GENES
url <- "https://objects.githubusercontent.com/github-production-release-asset-2e65be/41063438/ea6aa23c-c1da-4491-9c83-a9eb2e070791?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=releaseassetproduction%2F20241114%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20241114T232059Z&X-Amz-Expires=300&X-Amz-Signature=9d69120ee83bbe78024ae1c38c6873714dc0eb621c8c2627c00f33a2bce6eeb2&X-Amz-SignedHeaders=host&response-content-disposition=attachment%3B%20filename%3Dphenotype_to_genes.txt&response-content-type=application%2Foctet-stream"
destfile <- "Data/HPO/phenotype_to_genes.txt"
response <- GET(url, write_disk(destfile, overwrite = TRUE))

if (response$status_code == 200) {
  message("File downloaded successfully.")
} else {
  message("Failed to download the file. Status code: ", response$status_code)
}



## OMIM ----
# Data/OMIM/mim2geneDedup.tsv"
OMIM_URL <- "https://www.omim.org/static/omim/data/mim2gene.txt"
temp <- tempfile()
download.file(OMIM_URL,temp)
file.copy(temp, "Data/OMIM/mim2gene.txt")
unlink(temp)

## ClinVar ----
# CLINVAR_URL <- ""
# temp <- tempfile()
# download.file(CLINVAR_URL,temp)
# unz(temp, "Data/ClinVar/goa_human.gaf")
# unlink(temp)


