# installations ----

# This project uses renv to manage package versions.
# Restore the packages from renv lockfile



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

# not yet working automatically,
# have to manually download these reference files from the HPO website and
# place them in Data/HPO
# from here: https://hpo.jax.org/data/annotations
# "Genes to Disease" --> "Data/HPO/genes_to_disease.txt"
# "Phenotype to Genes" --> "Data/HPO/phenotype_to_genes.txt"



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


