# William Figgett (w.figgett@garvan.org.au).
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio.


# installations ----
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.19")
# packages <- c("shiny", "Cairo", "shinycssloaders", "shinybusy", "readr", "dplyr", "cluster",
#               "ggplot2", "ggrepel", "ggthemes", "ggradar", "gridExtra", "colorRamp2", "factoextra",
#               "VennDiagram", "remotes", "scales", "ComplexHeatmap", "viridis", "bslib", "httr", "jsonlite",
#               "ggvenn", "UpSetR", "ggVennDiagram", "ontologyIndex", "tidyverse", "dendextend")
# BiocManager::install(packages)
# remotes::install_github("ricardo-bion/ggradar")
# renv::snapshot()

# load dependencies
library(shiny)
library(readr)
library(dplyr)
library(ontologyIndex)
library(UpSetR)
library(ggvenn)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(ggradar)
library(gridExtra)
library(VennDiagram)
library(scales)
library(ComplexHeatmap)
library(viridis)
library(bslib)
library(colorRamp2)
library(cluster)
library(factoextra)
library(dendextend)
library(httr)
library(jsonlite)




# Check whether STRING-db can be updated
current_version_STRING <- "12.0"
response <- GET("https://string-db.org/api/json/version")
data <- fromJSON(rawToChar(response$content))
if (data$string_version != current_version_STRING) {
  print(paste("Local STRING-DB data can be updated from version ", current_version_STRING, " to ", data$string_version, sep=""))
} else {
  print(paste("Current STRING-DB local data is up to date (version ", current_version_STRING, ").", sep=""))
}


# Print current gtex datasetIds to check if there is an updated version
current_version_GTEx <- "gtex_v8"
response <- GET("https://gtexportal.org/api/v2/metadata/dataset")
data <- fromJSON(rawToChar(response$content))
print(paste("Current GTEx dataset: ", current_version_GTEx, ".", sep=""))
print(paste("Available GTEx datasets: ", paste(data$datasetId, collapse=", "), ".", sep=""))


# Load initial startup data
startup_genes <- read_csv(file = "Data/startup.csv", show_col_types = FALSE)
startup_panels <- read_csv(file = "Data/startup_panels.csv", show_col_types = FALSE)


# Load HPO genes - i think this is redundant since integrating HPO gene panels into "Curated Panels"
hpo <- read_tsv(file = "Data/new/HPO/HPO_genes.tsv", show_col_types = FALSE)


# OMIM gene identifiers (a few genes have multiple omim numbers, which are given comma-separated) )
# All genes which have an OMIM number are in mim2geneDedup$Gene
mim2geneDedup <- read_delim("Data/mim2geneDedup.tsv",
                            delim = "\t", escape_double = FALSE,
                            col_types = cols(OMIM = col_character(),
                                             Type= col_character(),
                                             Entrez = col_integer(),
                                             Gene = col_character(),
                                             Ensembl= col_character()),
                            trim_ws = TRUE)


# Human Phenotype Ontology data
hpo_p2g <- read_delim("Data/hpo_phenotype_to_genes.txt",
                      delim = "\t", escape_double = FALSE,
                      col_names = c("HPO_ID","Phenotype","EntrezID","Gene","Qualifier","DB_Name","DB_Reference"),
                      na = "NA", comment = "#", trim_ws = TRUE, show_col_types = FALSE)
phenotypes <- unique(hpo_p2g$Phenotype)
dystonia_hpo_phenotypes <- phenotypes[grepl("DYSTONIA", toupper(phenotypes), fixed = TRUE)]
hpo_p2g_dystonia <- hpo_p2g[hpo_p2g$Phenotype %in% dystonia_hpo_phenotypes,]
#print(paste0(length(phenotypes)," HPO phenotypes loaded (",
#             length(dystonia_hpo_phenotypes)," Dystonia phenotypes)"))



# Load interaction data from STRING-DB, pre-filtered on human proteins ("9606")
# Index file defines the protein-id with protein names
stringIndex <- read.csv("Data/9606.protein.info.v11.5.txt", header = TRUE,
                        encoding= "utf-8", quote = "", sep = "\t")
colnames(stringIndex) <- c("string_protein_id", "preferred_name",
                           "protein_size","annotation")
stringLinks <- read.csv("Data/STRING/9606.protein.physical.links.detailed.v12.0.txt",
                        header = TRUE,
                        encoding= "utf-8", quote = "", sep = " ")
#print(paste(dim(stringIndex)[1],"proteins loaded.")) # 19,566 proteins
#print(paste(dim(stringLinks)[1],"protein-protein links loaded.")) # 1,991,832 pair-scores



# Define count_n_panels()
# Takes the application UI's input
# Returns the number of panels inputted by the user
count_n_panels <- function(input) {

  n_panels <- 0

  if (!is.null(input$selectedPanels)) {
    n_panels <- n_panels + length(input$selectedPanels)
  }

  if (!is.null(input$selectedPanelsCurated)) {
    n_panels <- n_panels + length(input$selectedPanelsCurated)
  }

  if (!is.null(input$selectedPanelsExperimental)) {
    n_panels <- n_panels + length(input$selectedPanelsExperimental)
  }

  if (input$addGenes != "") {
    n_panels <- n_panels + 1

    }


  return(n_panels)
}


# Define get_input_genes()
# Takes the dataframe of all genes in the app's memory, the app's UI input and a logical argument 'filtered'
# Returns a dataframe of the genes resulting from the user's input (filters applied if 'filtered' is TRUE)
get_input_genes <- function(all_genes, input, filtered) {

  # Get genes from selected panels
  input_genes <- filter(all_genes, panel_name %in% input$selectedPanels | panel_name %in% input$selectedPanelsCurated | panel_name %in% input$selectedPanelsExperimental)

  # Get genes from custom input
  if (input$addGenes != "") {
    for (i in 1:length(unlist(strsplit(input$addGenes, split=" ")))) {
      input_genes[nrow(input_genes) + 1,] = list(unlist(strsplit(input$addGenes, split=" "))[i], "", "", "Custom input", NA)
    }
  }

  # Apply filters if input_genes has >0 entries and filtered argument is TRUE
  if (nrow(input_genes) > 0 & filtered == TRUE) {

    # Apply OMIM filter if "yes"
    if (input$useOMIM == "Yes") {
      input_genes <- filter(input_genes, symbol %in% mim2geneDedup$Gene)
    }

    # Apply "exclude genes" filter
    input_genes <- filter(input_genes, ! symbol %in% input$excludeGenes)

    # Apply evidence level filter
    if (length(input$filterEL) > 0) {
      evidence_levels <- na_if(input$filterEL, "")
      evidence_levels <- na_if(input$filterEL, "NA")
      genes_to_keep <- input_genes$symbol[input_genes$evidence_level %in% evidence_levels]
      input_genes <- filter(input_genes, symbol %in% genes_to_keep)
    }

    return(input_genes)

  } else { # Return unfiltered input_genes if filtered == FALSE

    return(input_genes)

  }
}


# Define insert_missing_cols()
# When making an adjacency matrix to display 'view input' plots/table, if a column (panel) contains 0's only then it is omitted
# We want to display this column (panel) regardless / prevent any errors, so need to insert it back in - this func does that
insert_missing_cols <- function(df, input, logical=FALSE) {

  # Compute what panels are supposed to be in df
  expected_panels <- c()
  if (!is.null(input$selectedPanels)) {
    expected_panels <- append(expected_panels, input$selectedPanels)
  }
  if (!is.null(input$selectedPanelsCurated)) {
    expected_panels <- append(expected_panels, input$selectedPanelsCurated)
  }
  if (!is.null(input$selectedPanelsExperimental)) {
    expected_panels <- append(expected_panels, input$selectedPanelsExperimental)
  }
  if (input$addGenes != "") {
    expected_panels <- append(expected_panels, "Custom input")
  }


  # Compute which panels are missing from df
  missing_panels <- expected_panels
  for (i in 1:ncol(df)) {
    missing_panels <- missing_panels[missing_panels != colnames(df)[i]]
  }

  # For each missing panel, add a column to df = 0 (or = FALSE if 'logical' == TRUE)
  if (length(missing_panels) > 0) {
    for (i in 1:length(missing_panels)) {
      if (logical == TRUE) {
        df <- add_column(df, new_col = FALSE)
      } else {
        df <- add_column(df, new_col = 0)
      }
      colnames(df)[length(df)] <- missing_panels[i]
    }
  }

  return(df)
}


# StringDB match function
# inputProtein is the protein symbol (eg. "ANO3")
# cutoff is the confidence limit 0-1000 (eg. 400 for medium confidence, 700 for high confidence).
stringDBMatches <- function(inputProtein, cutoff=400) {

  # get the protein-id for this input protein name
  i<-which(stringIndex$preferred_name %in% inputProtein, arr.ind = TRUE)

  # find links with this protein id mentioned as either protein1 or protein2
  j<-which(stringLinks$protein1 %in% stringIndex[i,]$string_protein_id |
             stringLinks$protein2 %in% stringIndex[i,]$string_protein_id, arr.ind = TRUE)
  linkResults <- stringLinks[j,]

  # remove the input protein from the results,
  linkResults$protein1[linkResults$protein1 %in% stringIndex[i,]$string_protein_id] <-""
  linkResults$protein2[linkResults$protein2 %in% stringIndex[i,]$string_protein_id] <-""
  linkResults$string_protein_id <- paste(linkResults$protein1,linkResults$protein2, sep = "")

  linkResults <- linkResults[,-c(1,2)] # remove first 2 columns (protein1, protein2)

  # sort by descending score
  linkResults <- linkResults[order(linkResults$combined_score, decreasing = TRUE),]

  # get a list of all protein id involved in any result pairing
  linkResultsCutoff <- linkResults[linkResults$combined_score >= cutoff,]

  resultdf <- merge(linkResultsCutoff, stringIndex, by="string_protein_id")
  resultdf <- resultdf[order(resultdf$combined_score, decreasing = TRUE),]

  # name and score
  if (length(resultdf$preferred_name)>0) {
    resultdf$nameScore <- paste(resultdf$preferred_name," [",resultdf$combined_score,"]", sep = "")
  }

  resultdf <- resultdf[!duplicated(resultdf$nameScore),]

  return(resultdf)
}



# load GTEx tissue expression data
gtex <- read.csv(file="Data/GTExMedianTPM_fixed.csv")
dim(gtex)
names(gtex)

# separate the numeric expression values
gtexmat <- data.matrix(gtex[,7:60])
# set gene symbols as the row names
row.names(gtexmat) <- gtex$FixedGeneName

gtexbraincol <- c(rep(FALSE,7), rep(TRUE,13), rep(FALSE,34))
dim(gtexmat)
length(gtexbraincol)
cbind(gtexbraincol, names(gtex)[7:60])

gteximmunecol <- c(1:length(colnames(gtexmat)) %in% c(23,48,51,54))

# remove zero-variance genes... (null expression) in either brain or non-brain
geneVariance.brain <- apply(gtexmat[,gtexbraincol], MARGIN = 1, FUN = var)
geneVariance.nonbrain <- apply(gtexmat[,!gtexbraincol], MARGIN = 1, FUN = var)
geneVariance.immune <- apply(gtexmat[,gteximmunecol], MARGIN = 1, FUN = var)
head(geneVariance.nonbrain)
keepGenes <- geneVariance.brain>0 & geneVariance.nonbrain>0 & geneVariance.immune>0
sum(keepGenes) #33195
gtexmatFiltered <- gtexmat[keepGenes,]
dim(gtexmatFiltered) #33195

#reduce the GTEx data matrix to only the Brain tissues and non-brain
gtexmat.brain <- gtexmatFiltered[,gtexbraincol]
gtexmat.nonbrain <- gtexmatFiltered[,!gtexbraincol]
gtexmat.immune <- gtexmatFiltered[,gteximmunecol]


# Function to get similar genes to a single query gene,
# ranked by maximising covariance across tissues provided in referenceExpression
getSimilarGTEx <- function(queryGene, referenceExpression) {

  if (! queryGene %in% row.names(referenceExpression)) {
    print("Gene '", paste(queryGene), "' not in GTEx dataset - omitted from results.", sep="")
    return(NULL)
  }

  # get the expression row for the queryGene
  queryExpression <- referenceExpression[row.names(referenceExpression)==queryGene,]

  # get correlation of the gene against every gene
  geneCorrelations <- apply( referenceExpression , 1 , cor , y = queryExpression )



  # added p-value21/7
  # COMMENTED OUT TO IMPROVE SPEED.. WASN'T BEING USED
  #geneCorTest <- apply( referenceExpression , 1, cor.test, y=queryExpression,
  #                      method="spearman", exact=FALSE)

  # COMMENTED OUT TO IMPROVE SPEED.. WASN'T BEING USED
  pval <- numeric(length = length(geneCorrelations))
  #for (i in 1:length(geneCorTest)) {
    #pval[i] <- geneCorTest[[i]]$p.value
  #}


  # COMMENTED OUT TO IMPROVE SPEED.. WASN'T BEING USED
  #geneCorTest[[1]]$p.value
  geneCorrelations.df <- data.frame(cbind(geneCorrelations, pval))
  colnames(geneCorrelations.df) <- c("rho", "p.value")


  return(geneCorrelations.df)
  # output is a dataframe with rho and p.val columns, all genes as rows
}


inspectGTExPanel <- function(queryGenes, tissueSelection) {
  if (length(tissueSelection) > 0) {
    referenceExpression <- gtexmatFiltered[,colnames(gtexmatFiltered) %in% tissueSelection]
  } else {
    referenceExpression <- gtexmatFiltered
  }

  gtexCorr <- matrix(nrow=nrow(referenceExpression), ncol = length(queryGenes))
  colnames(gtexCorr) <- queryGenes
  row.names(gtexCorr) <- row.names(referenceExpression)
  # COMMENTED OUT TO INCREASE SPEED.. WASN'T BEING USED
  #gtexCorr.P <- gtexCorr
  for (i in 1:length(queryGenes)) {
    incProgress(1/(1+length(queryGenes)),
                detail = paste0("Comparing gene #",i," of ",length(queryGenes)))


    test <- getSimilarGTEx(queryGenes[i], referenceExpression)
    ##
    #if queryGene not in refExp matrix then skip that gene (column in gtexCorr takes NA values)
    ##
    if (!is.null(test)) {
      gtexCorr[,i]<- test$rho
    }

    # COMMENTED OUT TO INCREASE SPEED.. WASN'T BEING USED
    #gtexCorr.P[,i]<- test$p.value
  }

  #filter out na values from gtexCorr
  gtexCorr <- gtexCorr[,colSums(is.na(gtexCorr))==0]

  # COMMENTED OUT TO INCREASE SPEED.. WASN'T BEING USED
  #min.p <- apply(gtexCorr.P, MARGIN = 1, FUN = min)

  rankdf <- data.frame(Gene=row.names(gtexCorr),
                       Sum=rowSums(gtexCorr),
                       gtexCorr)
  rankdf <- rankdf[order(rankdf$Sum, decreasing = TRUE),]

  # medianRank <- apply(gtexCorrRank, MARGIN = 1, FUN=median)
  # quartileRank <- apply(gtexCorrRank, MARGIN = 1, FUN=quantile, probs=0.25)
  # scaledRank <- apply(gtexCorrRank, MARGIN=1, FUN=quantile, probs=(1/length(queryGenes)))
  # rankdf <- data.frame(Gene = names(medianRank), scaledRank, medianRank, quartileRank, gtexCorr)
  # colnames(rankdf) <- c("Gene","Scaled Rank", "Median Rank", "Quartile Rank", colnames(gtexCorr))
  # rankdf <- rankdf[order(rankdf$`Scaled Rank`, decreasing = FALSE),]
  return(rankdf) # return a data frame with the panel correlation sorted by quartile rank
}



# function analysis all HPO pheontypes against the input gene(s)
# output is a data frame hpo_df, $phenotypes, $count, $overlap,
#                             $overlapPercent, $overlapPercentLabel
#                             (ordered by descending $overlap)
# Progress bar detail is incremented with each HPO phenotype.
hpoOverlap <- function (testGenes) {
  count<- integer(length = length(phenotypes))
  overlap <- integer(length = length(phenotypes))
  overlapPercent <- numeric(length = length(phenotypes))
  i=0
  for (p in 1:length(phenotypes)) {
    matches <- hpo_p2g$Phenotype==phenotypes[p]
    count[p]=length(unique(hpo_p2g$Gene[matches]))

    pGenes = hpo_p2g$Gene[hpo_p2g$Phenotype==phenotypes[p]]
    tst <- c(unique(pGenes),unique(testGenes))
    overlap[p]=sum(duplicated(tst))

    # increment the progress bar
    i=i+1
    incProgress(1/(1+length(phenotypes)),
                detail = paste0("Comparing phenotype #",i," of ",length(phenotypes)))
  }
  overlapPercent <- round(overlap / count * 100, digits = 0)
  overlapPercentLabel <- paste0(overlapPercent,"%")
  overlapPercentLabel
  hpo_df <- data.frame(phenotypes,count, overlap, overlapPercent, overlapPercentLabel)
  hpo_df <- hpo_df[order(hpo_df$overlap, decreasing = TRUE),]
  return(hpo_df)
}



textblock2genes <- function(textblock, useOMIM) {
  #replace newline or comma characters with a space
  textblock <- gsub(","," ", textblock, fixed = TRUE)
  textblock <- gsub("\\n"," ", textblock)
  textblock <- gsub("\\s+"," ",textblock) # trim excess whitespace between genes

  gp <- data.frame(strsplit(as.character(textblock),' '))
  colnames(gp) <- "Gene"
  # force capital letters
  # this is causing some genes to be lost i think
  #(e.g. we have C19orf12 in input and OMIM df, convert input to capital != the entry in OMIM)
  gp$Gene <- toupper(gp$Gene)
  genelist <- unique(gp$Gene) # vector of unique gene names
  # remove non-matching gene names

  ##KIRBY - COMMENTED BELOW LINE OUT TO CHECK GENE COUNT
  #genelist <- genelist[genelist %in% row.names(gtexmatFiltered)]
  if(useOMIM==TRUE) {
    #toupper(omim) because we did it above
    genelist <- genelist[genelist %in% toupper(mim2geneDedup$Gene)]
  }
  return(genelist)
}



# Load Gene Ontology data
col_names = c("DB", "DB Object ID", "DB Object Symbol", "Qualifier", "GO ID", "DB:Reference (|DB:Reference)", "Evidence Code",
              "With (or) From", "Aspect", "DB Object Name", "DB Object Synonym (|Synonym)", "DB Object Type", "Taxon (|taxon)",
              "Date", "Assigned By", "Annotation Extension", "Gene Product Form ID")
GO_annotations <- read_tsv(file = "Data/new/GO/goa_human.gaf", show_col_types = FALSE, skip = 41, col_names = col_names)

GO_terms <- as.data.frame(get_ontology(file = "Data/new/GO/go-basic.obo"))
rownames(GO_terms) <- NULL




######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


# Server function

function(input, output, session) {

  # Startup genes and startup panels are loaded into reactive values in the server
  genesRV <- reactiveVal(startup_genes)

  panels <- reactiveVal(startup_panels)


################################################################################
# PANEL INPUT FUNCTIONALITY
################################################################################

  # Define reactive value "errormsg" that can be updated to display different errors to users
  errormsg <- reactiveVal("")


  #update disease choices on load-in
  updateCheckboxGroupInput(inputId = "disease",
                           choices = sort(unique(startup_genes$disease[startup_genes$panel != "HPO"])))


  # Define expected UI when user clicks "Create Gene Panel"
  observeEvent(input$addPanel, {
    errormsg("")
    showModal(modalDialog(
      title = "Create Gene Panel",
      textInput("addPanelDisease",
                label = "Disease/Phenotype:",
                placeholder = "Enter an associated disease/phenotype"),
      textInput("addPanelName",
                label = "Panel ID:",
                placeholder = "Enter a descriptive panel ID"),
      tags$b("Panel Name: "),
      textOutput("panelName"),
      tags$br(),
      radioButtons("addPanelGenesMethod",
                   "Add Genes Method:",
                   choices = c("Text block",
                               "Source & url"),
                   selected = "Text block"),
      conditionalPanel(condition = 'input.addPanelGenesMethod == "Text block"',
                       textAreaInput("addPanelGenes",
                                     label = "Panel Genes:",
                                     resize = "vertical",
                                     placeholder = "Enter the panel's genes (space-separated)")
                       ),
      conditionalPanel(condition = 'input.addPanelGenesMethod == "Source & url"',
                       selectizeInput("addPanelGenesSource",
                                      "Source:",
                                      choices = c("PanelApp Australia",
                                                  "Genomics England PanelApp",
                                                  "Human Phenotype Ontology")
                                                  #"DisGeNET", #not yet supported due to issues accessing api, commented out for now
                                      ),
                       textInput("addPanelGenesUrl",
                                 "URL:",
                                 value = "")
      ),

      # Could add a preview gene count for input?... Not super necessary

      radioButtons(inputId = "addPanelType",
                   label = "Panel Type:",
                   choices = c("Commercial",
                               "Curated",
                               "Experimental"),
                   selected = "Experimental"
                   ),

      radioButtons(inputId = "addPanelProtected",
                   label = "Protected (cannot be deleted)?",
                   choices = c("Yes", "No"),
                   selected = "No"),
      dateInput(inputId = "addPanelDate",
                label = "Panel Date:",
                value = Sys.Date()),
      span(textOutput("errormsg"), style="color:red"),
      footer = tagList(
        actionButton("createPanel",
                     label = "Submit"),
        modalButton("Cancel")
      ),
      easyClose = TRUE,
      fade = FALSE
    ))
  })


  # Render the preview of panel name in create panel UI popup
  output$panelName <- renderText({
    if (input$addPanelDisease == "" | input$addPanelName == "") {
      "(invalid input)"
    } else {
      paste(input$addPanelDisease, " (", input$addPanelName, ")", sep="")
    }
  })


  # Render the reactive error msg to users
  output$errormsg <- renderText({ errormsg() })


  # Define functionality for when user tries to submit a new panel to be created
  observeEvent(input$createPanel, {

    #check the inputted fields are not null
    fields <- c()
    count <- 0
    if (input$addPanelDisease == "") {
      fields <- append(fields, 'Disease/phenotype')
      count <- count + 1
    }
    if (input$addPanelName == "") {
      fields <- append(fields, 'Panel name')
      count <- count + 1
    }
    if (input$addPanelGenes == "" & input$addPanelGenesMethod == "Text block") {
      fields <- append(fields, 'Panel genes')
      count <- count + 1
    }
    #probably need a more robust way of validating url / source combination
    if (input$addPanelGenesUrl == "" & input$addPanelGenesMethod == "Source & url") {
      fields <- append(fields, 'URL')
      count <- count + 1
    }

    #more than 1 field is null, return relevant error msg
    if (count > 0) {
      errormsg(paste("The following fields cannot be empty: ", paste(fields, collapse = ", "), sep = ""))
      return()
    }

    #check if the given panel name already exists
    if (paste(input$addPanelDisease, " (", input$addPanelName, ")", sep='') %in% genesRV()$panel_name) {
      errormsg("Panel name already exists for the given disease.")
      return()
    }

    #input is valid, get genes_to_add

    if (input$addPanelGenesMethod == "Text block") {

      genes_to_add <- unlist(strsplit(input$addPanelGenes, split=" "))

    } else {
      #if add genes method == source & url

      if (input$addPanelGenesSource == "PanelApp Australia") {
        #PanelApp Australia
        #panel urls are in the form "https://panelapp.agha.umccr.org/panels/{id}/"
        #retrieve 'id' so we can request the api for the genes
        panel_id <- tail(unlist(strsplit(input$addPanelGenesUrl, split="/")), 1)
        base_url <- "https://panelapp.agha.umccr.org/api/v1"
        url <- paste(base_url, "/panels/", panel_id, "/genes/", sep="")
        response <- GET(url)
        data <- fromJSON(rawToChar(response$content))

        genes_to_add <- c()
        genes_to_add <- append(genes_to_add, data$results$gene_data$gene_symbol)

        # I think max ~100 genes per page, so some requests may span multiple pages
        # Iterate over each page until next page is null
        i <- 1
        while (!is.null(data$`next`)) {
          i <- i + 1
          url <- paste(base_url, "/panels/", panel_id, "/genes/?page=", i, sep="")
          response <- GET(url)
          data <- fromJSON(rawToChar(response$content))
          genes_to_add <- append(genes_to_add, data$results$gene_data$gene_symbol)
        }

      } else if (input$addPanelGenesSource == "Genomics England PanelApp") {
        #Genomics England PanelApp
        #panel urls are in the form "https://panelapp.genomicsengland.co.uk/panels/{id}/"
        #retrieve 'id' so we can request the api for the genes
        panel_id <- tail(unlist(strsplit(input$addPanelGenesUrl, split="/")), 1)
        base_url <- "https://panelapp.genomicsengland.co.uk/api/v1"
        url <- paste(base_url, "/panels/", panel_id, "/genes/", sep="")
        response <- GET(url)
        data <- fromJSON(rawToChar(response$content))

        genes_to_add <- c()
        genes_to_add <- append(genes_to_add, data$results$gene_data$gene_symbol)

        # I think max ~100 genes per page, so some requests may span multiple pages
        # Iterate over each page until next page is null
        i <- 1
        while (!is.null(data$`next`)) {
          i <- i + 1
          url <- paste(base_url, "/panels/", panel_id, "/genes/?page=", i, sep="")
          response <- GET(url)
          data <- fromJSON(rawToChar(response$content))
          genes_to_add <- append(genes_to_add, data$results$gene_data$gene_symbol)
        }

      } else if (input$addPanelGenesSource == "Human Phenotype Ontology") {
        #HPO
        #search a phenotype on HPO site
        #url should be of the following form "https://hpo.jax.org/app/browse/term/HP:0001300" where "HP:0001300" is a valid hpo id
        hpo_id <- tail(unlist(strsplit(input$addPanelGenesUrl, split="/")), 1)
        base_url <- "https://hpo.jax.org/api/hpo/"
        # Can provide parameter "max=-1" which displays all genes in the request rather than a default max of 20
        # no need to iterate through pages
        url <- paste(base_url, "term/", hpo_id, "/genes/?max=-1", sep="")
        response <- GET(url)
        data <- fromJSON(rawToChar(response$content))

        genes_to_add <- data$genes$geneSymbol

      } else if (input$addPanelGenesSource == "DisGeNET") {
        #DisGeNET - NOT FUNCTIONAL YET
        #search a disease then select summary of gene-disease associations
        #url is of the following form "https://www.disgenet.org/browser/0/1/0/{disease_UMLS_CUI}/"
        errormsg("DisGeNET not supported yet.")
        return()
        disease_cui <- tail(unlist(strsplit(input$addPanelGenesUrl, split="/")), 1)
        api_host <- "https://www.disgenet.org/api"
        #need authentication => post to /auth/ with auth_params (email + password of a registered account)
        #then include authorization_headers in future requests for access to api

      }

    }

    #insert genes_to_add into genesRV() which is accessed by current sessions

    genes <- genesRV()
    for (i in 1:length(genes_to_add)) {
      genes[nrow(genes)+1,] = list(genes_to_add[i], input$addPanelDisease, input$addPanelName, paste(input$addPanelDisease, " (", input$addPanelName, ")", sep=''), NA)
    }
    genesRV(genes)

    panels <- panels()
    panels[nrow(panels)+1,] = list(paste(input$addPanelDisease, " (", input$addPanelName, ")", sep=''), input$addPanelDate, input$addPanelProtected == "Yes", input$addPanelType)
    panels(panels)

    #update startup.csv file so that data will be remembered next session
    write.csv(genesRV(), file = "Data/startup.csv", quote = FALSE, row.names = FALSE)
    write.csv(panels(), file = "Data/startup_panels.csv", quote = FALSE, row.names = FALSE)

    #update disease choices in case a new disease was inputted
    updateCheckboxGroupInput(inputId = "disease",
                             choices = sort(unique(genesRV()$disease[genesRV()$panel != "HPO"])),
                             selected = input$disease)

    #maybe give a message saying panel has been created successfully? not done yet, not crucial


    #remove the ui popup
    removeModal()
  })


  #Define expected UI for when the user clicks "Edit Panel"
  observeEvent(input$editPanel, {
    #Reset errormsg to an empty string
    errormsg("")
    showModal(modalDialog(
      title = "Edit Gene Panel",
      selectInput("editPanelName",
                  label = "Panel:",
                  choices = c("", sort(unique(panels()$panel_name))),
                  selected = NULL),


      #display panel info
      conditionalPanel(condition = 'input.editPanelName.length > 0',
                       #panel genes
                       #add / remove
                       textAreaInput(inputId = "editPanelPreview",
                                     label = "Panel Genes:",
                                     resize = "vertical"),
                       #Panel date
                       dateInput(inputId = "editPanelDate",
                                 label = "Panel Date:"),
                       #panel type
                       radioButtons(inputId = "editPanelType",
                                    label = "Panel Type:",
                                    choices = c("Commercial",
                                                "Curated",
                                                "Experimental"),
                                    selected = NULL),
                       #protected?
                       radioButtons(inputId = "editPanelProtected",
                                    label = "Protected (cannot be deleted)?",
                                    choices = c("Yes", "No"),
                                    selected = NULL)
                       ),
      #display errormsg
      span(textOutput("errormsg"), style="color:red"),
      footer = tagList(
        actionButton("submitEditPanel",
                     label = "Submit"),
        modalButton("Cancel")
      ),
      easyClose = TRUE,
      fade = FALSE
    ))
  })

  # make UI reactive and updates with relevant fields when user chooses a new panel to edit
  observeEvent(input$editPanelName, {

    #no change if panelname is null
    if (input$editPanelName == "") {
      return()
    }

    #display preview of selected panel's genes
    updateTextAreaInput(inputId = "editPanelPreview",
                        value = paste(genesRV()$symbol[genesRV()$panel_name == input$editPanelName], collapse = " "))

    #panel date
    updateDateInput(inputId = "editPanelDate",
                    value = panels()$date[panels()$panel_name == input$editPanelName])

    #panel type
    updateRadioButtons(inputId = "editPanelType",
                       selected = panels()$type[panels()$panel_name == input$editPanelName])

    #panel protected
    updateRadioButtons(inputId = "editPanelProtected",
                       selected = c("Yes", "No")[1 + (panels()$protected[panels()$panel_name == input$editPanelName] == FALSE)])

  })

  # Define functionality for when user wants to submit an "Edit Panel" request
  observeEvent(input$submitEditPanel, {

    #check for errors
    if (input$editPanelName == "") {
      errormsg("No panel selected.")
      return()
    }

    if (length(input$editPanelDate) == 0) {
      errormsg("Invalid date.")
      return()
    }

    if (length(input$editPanelPreview) == 0) {
      errormsg("Invalid genes input.")
      return()
    }

    #change panel date, type, protected status in panels() and Data/startup_panels.csv
    panels <- panels()
    panels$type[panels$panel_name == input$editPanelName] <- input$editPanelType
    panels$protected[panels$panel_name == input$editPanelName] <- (input$editPanelProtected == "Yes")
    panels$date[panels$panel_name == input$editPanelName] <- input$editPanelDate
    panels(panels)
    write.csv(panels(), file = "Data/startup_panels.csv", quote = FALSE, row.names = FALSE)

    #change panels genes (stored in genesRV())
    disease <- unique(filter(genesRV(), panel_name == input$editPanelName)$disease)
    panel <- unique(filter(genesRV(), panel_name == input$editPanelName)$panel)
    #remove panel's old genes
    genes <- filter(genesRV(), panel_name != input$editPanelName)
    #get new genes
    genes_to_add <- unlist(strsplit(input$editPanelPreview, split=" "))
    #add new genes
    for (i in 1:length(genes_to_add)) {
      genes[nrow(genes)+1,] = list(genes_to_add[i], disease, panel, paste(disease, " (", panel, ")", sep=''), NA)
    }
    #update reactive value genesRV()
    genesRV(genes)
    #update startup.csv
    write.csv(genesRV(), file = "Data/startup.csv", quote = FALSE, row.names = FALSE)

    #remove UI popup
    removeModal()

  })


  #Define UI for when user wants to delete a panel
  observeEvent(input$removePanel, {
    errormsg("")
    showModal(modalDialog(
      title = "Delete Gene Panel",
      selectInput("removePanelName",
                  label = "Panel:",
                  choices = c("", sort(unique(panels()$panel_name[panels()$protected == FALSE]))),
                  selected = NULL),
      #display selected panel's genes and gene count
      conditionalPanel(condition = 'input.removePanelName != ""',
                       textAreaInput(inputId = "previewRemovePanel",
                                     label = "Preview Selected Panel:")),
      #DISPLAY GENE COUNT


      span(textOutput("errormsg"), style="color:red"),

      footer = tagList(
        actionButton("deletePanel",
                     label = "Submit"),
        modalButton("Cancel")
      ),

      easyClose = TRUE,
      fade = FALSE
    ))
  })


  #When a new panel is chosen in the "Delete Panel" UI, preview the genes of that panel
  observeEvent(input$removePanelName, {
    #display preview of selected panel's genes and gene count
    updateTextAreaInput(inputId = "previewRemovePanel",
                        value = paste(genesRV()$symbol[genesRV()$panel_name == input$removePanelName], collapse = " "))
  })

  #Define functionality for when user wants to submit a "Delete Panel" request
  observeEvent(input$deletePanel, {

    #first check valid input
    if (input$removePanelName == "") {
      errormsg("No panel selected.")
      return()
    }

    #delete panel from genesRV() reactive value
    genes <- genesRV()
    genes <- filter(genes, panel_name != input$removePanelName)
    genesRV(genes)

    #delete panel from panels() reactive value
    panels <- panels()
    panels <- filter(panels, panel_name != input$removePanelName)
    panels(panels)

    #update startup files
    write.csv(genesRV(), file = "Data/startup.csv", quote = FALSE, row.names = FALSE)
    write.csv(panels(), file = "Data/startup_panels.csv", quote = FALSE, row.names = FALSE)

    #update disease checkbox input in case a disease is entirely deleted (has no remaining panels)
    updateCheckboxGroupInput(inputId = "disease",
                             choices = sort(unique(genesRV()$disease[genesRV()$panel != "HPO"])),
                             selected = input$disease)

    #remove UI popup
    removeModal()
  })


  #UPDATE CURRENTLY DISPLAYED PANELS BASED ON DISEASE SELECTIONS/DATE RANGE FILTER
  observeEvent({
    input$disease
    input$createPanel
    input$deletePanel
    input$dateRange
    input$submitEditPanel
  },
  ignoreNULL = FALSE,
  {

    #check for null input, in which case there should be no options
    if (is.null(input$disease)) {
      updateCheckboxGroupInput(inputId = "selectedPanels",
                               selected = NULL,
                               choices = c("null"))
      updateCheckboxGroupInput(inputId = "hpoPhenotype",
                               selected = NULL,
                               choices = c("null"))
      return()
    }

    #update panels
    panels_to_display <- c()

    for (i in 1:length(input$disease)) {
      curr_disease <- input$disease[i]
      curr_disease_panels <- unique(genesRV()$panel_name[genesRV()$disease == curr_disease])
      panels_to_display <- append(panels_to_display, curr_disease_panels)
    }

    panels_to_display <- filter(panels(), panel_name %in% panels_to_display)
    panels_to_display <- filter(panels_to_display, date >= input$dateRange[1] & date <= input$dateRange[2])

    #sort into commercial, curated, experimental
    commercial <- panels_to_display$panel_name[panels_to_display$type == "Commercial"]
    updateCheckboxGroupInput(inputId = "selectedPanels",
                             selected = input$selectedPanels,
                             choices = sort(commercial))

    curated <- panels_to_display$panel_name[panels_to_display$type == "Curated"]
    updateCheckboxGroupInput(inputId = "selectedPanelsCurated",
                             selected = input$selectedPanelsCurated,
                             choices = sort(curated))

    experimental <- panels_to_display$panel_name[panels_to_display$type == "Experimental"]
    updateCheckboxGroupInput(inputId = "selectedPanelsExperimental",
                             selected = input$selectedPanelsExperimental,
                             choices = sort(experimental))
  })


  #update preview of selected genes based on input
  observeEvent({
    input$selectedPanels
    input$selectedPanelsCurated
    input$selectedPanelsExperimental
    input$addGenes
    input$excludeGenes
    input$panelAreaInput
    input$useOMIM
    input$filterEL
    input$submitEditPanel
  },
  ignoreNULL = FALSE,
  {
    input_genes <- unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol)
    updateTextAreaInput(inputId = "panelAreaInput",
                        value = paste(sort(input_genes), collapse = ' '))
    if (input$chooseCluster == "All") {
      updateTextAreaInput(inputId = "gtexClusterPreview",
                          value = paste(unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol), collapse=" "))
    }

  })


  #update "exclude genes" options as other input is received
  observeEvent({
    input$selectedPanels
    input$selectedPanelsCurated
    input$selectedPanelsExperimental
    input$addGenes
    input$useOMIM
  },
  ignoreNULL = FALSE,
  {

    curr_selections <- input$excludeGenes

    #need to filter current selections so that they are valid
    curr_selections <- curr_selections[curr_selections %in% get_input_genes(genesRV(), input, filtered = FALSE)$symbol]

    if (input$useOMIM == "Yes") {
      curr_selections <- curr_selections[curr_selections %in% mim2geneDedup$Gene]
    }

    input_genes <- get_input_genes(genesRV(), input, filtered = TRUE)$symbol

    input_genes <- append(input_genes, curr_selections)

    suppressWarnings(updateSelectizeInput(inputId = 'excludeGenes',
                                          choices = sort(input_genes),
                                          selected = curr_selections))
  })



  # display a count of genes in the input panel
  output$countInput <- renderText({
    paste0("Gene Count: ", length(textblock2genes(input$panelAreaInput, input$useOMIM=="Yes")))
  })



################################################################################
# VIEW INPUT FUNCTIONALITY
################################################################################



  geneTable <- reactive({

    input_genes <- get_input_genes(genesRV(), input, filtered = TRUE)

    #create an adjacency matrix to use as input
    gene_table <- as.data.frame.matrix(with(input_genes, table(symbol, panel_name)) == 1)

    #count number of input sets
    n_panels <- count_n_panels(input)

    #check all selected panels exist in gene_table
    gene_table <- insert_missing_cols(gene_table, input, logical=TRUE)

    #create the final table to display
    final_table <- c()

    final_table$Gene <- rownames(gene_table)

    if (nrow(gene_table) > 0) {
      for (i in 1:length(final_table$Gene)) {
        evidence_levels <- paste(input_genes$evidence_level[input_genes$symbol == final_table$Gene[i]],
                                 " (", input_genes$panel_name[input_genes$symbol == final_table$Gene[i]],
                                 ")", sep = "")
        final_table$`Evidence Level`[i] <- paste(evidence_levels, collapse=";")
      }
    }

    rownames(gene_table) <- NULL
    final_table$`# Input Lists` <- rowSums(gene_table)
    final_table <- cbind(final_table, gene_table)
    df <- as.data.frame.list(final_table)
    colnames(df) <- colnames(final_table)

    df

  })


  output$geneTable <- renderDataTable({
    geneTable()
  })


  output$downloadGeneTable <- downloadHandler(
    filename = function(){"gene_panel.csv"},
    content = function(file) {
      write.csv(geneTable(), file, row.names = FALSE, quote = FALSE)
    }
  )




  upsetPlot <- function(){
    #don't bother rendering if upset plot option isn't selected
    if (! "UpSet Plot" %in% input$inputPlots) {
      return()
    }

    #count number of input sets
    n_panels <- count_n_panels(input)

    #don't bother rendering if less than 2 panels are selected
    if (n_panels < 2) {
      return()
    }

    plot_data <- get_input_genes(genesRV(), input, filtered = TRUE)

    #create an adjacency matrix to use as input
    plot_data <- as.data.frame.matrix(with(plot_data, table(symbol, panel_name)))


    plot_data <- insert_missing_cols(plot_data, input)

    #render upset plot
    upset(plot_data, nsets = n_panels, text.scale = 1.6)
  }


  output$upset <- renderPlot({
    upsetPlot()
  })


  output$downloadUpset <- downloadHandler(
    filename = function() {
      "upset_plot.png"
    },
    content = function(file) {
      n_panels <- count_n_panels(input)
      png(file, type="cairo", width = (480+40*n_panels))
      print(upsetPlot())
      dev.off()
    }
  )


  vennDiagram <- function(){
    #don't bother rendering if venn plot option isn't selected
    if (! "Venn Diagram" %in% input$inputPlots) {
      return()
    }

    #count number of input sets
    n_panels <- count_n_panels(input)

    #don't bother rendering if <2 or >4 panels selected
    if (n_panels < 2 || n_panels > 4) {
      return()
    }


    input_genes <- get_input_genes(genesRV(), input, filtered = TRUE)

    #create an adjacency matrix to use as input
    plot_data <- as.data.frame.matrix(with(input_genes, table(symbol, panel_name)))

    plot_data <- insert_missing_cols(plot_data, input)


    #values need to be logical & as a tibble for ggvenn
    plot_data <- as_tibble(plot_data == 1)

    #suppress warnings as function was giving a seemingly irrelevant/incorrect warning
    suppressWarnings(ggvenn(plot_data, show_outside = "none", set_name_size = 4))
  }


  output$venn <- renderPlot({
    vennDiagram()
  })


  output$downloadVenn <- downloadHandler(
    filename = function() {
      "venn_diagram.png"
    },
    content = function(file) {
      png(file, type="cairo")
      print(vennDiagram())
      dev.off()
    }
  )


  barPlot <- function(){
    #don't bother rendering if bar plot option isn't selected
    if (! "Bar Plot" %in% input$inputPlots) {
      return()
    }

    #count number of input sets
    n_panels <- count_n_panels(input)

    #don't bother rendering if less than 1 panel selected
    if (n_panels == 0) {
      return()
    }

    plot_data <- get_input_genes(genesRV(), input, filtered = TRUE)

    #create an adjacency matrix to use as input
    plot_data <- as.data.frame.matrix(with(plot_data, table(symbol, panel_name)))

    plot_data <- insert_missing_cols(plot_data, input)

    #set x = 1:max(rowSums(plot_data)) before adding another column to plot_data
    x <- 1:max(rowSums(plot_data))

    plot_data$`# Input Lists` <- rowSums(plot_data)

    y <- c()
    for (value in x) {
      count <- 0
      for (i in 1:nrow(plot_data)) {
        if (plot_data[i,]$`# Input Lists` == value) {
          count <- count + 1
        }
      }
      y <- append(y, count)
    }
    data <- cbind(x, y)
    data <- as.data.frame(data)

    #render plot
    suppressWarnings({
      ggplot(data, aes(x=x, y=y)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = 0:max(x+1)) +
      labs(x = "# Input Lists", y = "count")
      })
  }


  output$bar <- renderPlot({
    barPlot()
  })


  output$downloadBar <- downloadHandler(
    filename = function() {
      "bar_plot.png"
    },
    content = function(file) {
      png(file, type="cairo")
      print(barPlot())
      dev.off()
    }
  )



################################################################################
# STRING FUNCTIONALITY
################################################################################

  observeEvent(input$updateSTRING, {
    #make progress bar
    #download files
    #download.file()
    #unzip
    #gunzip()
    #reload required local data

    print("STRING update started...")


    print("STRING update complete...")
  })

  #if slider threshold input is changed, update the numeric threshold input to match
  observeEvent(input$stringConf, {
    updateNumericInput(inputId = "stringConfExact",
                       value = input$stringConf)
  }, ignoreInit = TRUE)


  #if numeric threshold input is changed, check valid and update the slider threshold input to match
  observeEvent(input$stringConfExact, {
    if (is.na(input$stringConfExact)) {
      return()
    }
    if (input$stringConfExact > 999) {
      updateNumericInput(inputId = "stringConfExact",
                         value = 999)
    }
    if (input$stringConfExact < 0) {
      updateNumericInput(inputId = "stringConfExact",
                         value = 0)
    }

    updateSliderInput(inputId = "stringConf",
                      value = input$stringConfExact)

  }, ignoreInit = TRUE)


  # display a count of genes in the STRING results
  output$countString <- renderText({
    paste0("Gene Count: ", length(textblock2genes(input$stringTextOutput, input$useOMIM=="Yes")))
  })


  #define a reactive value to serve as an error msg for STRING section
  STRINGerrormsg <- reactiveVal("")


  output$STRINGerrormsg <- renderText({ STRINGerrormsg() })

  stringResults <- reactiveVal(NULL)

  #update stringResults() once input$renderSTRING is clicked
  observeEvent(input$renderSTRING, {

    #get the input genes
    input_genes <- unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol)
    STRINGerrormsg("")
    if (length(input_genes) == 0) {
      STRINGerrormsg("You haven't inputted any genes yet.")
      stringResults
    }

    #calculate STRING results against input genes
    stringResults <- data.frame(Input=character(),
                                Result=character(),
                                experimental_score=numeric(),
                                database_score=numeric(),
                                textmining_score=numeric(),
                                combined_score=numeric(),
                                stringsAsFactors = FALSE)


    withProgress(message = "Computing STRING Results", value = 0, {
      i = 1
      for (gene in input_genes) {
        stringResult <- stringDBMatches(gene, 0)[,c("preferred_name", "experimental", "database", "textmining", "combined_score")]
        if(dim(stringResult)[1] > 1) {
          stringResult$Input <- gene
          colnames(stringResult) <- c("Result", "experimental_score", "database_score", "textmining_score", "combined_score", "Input")
          stringResult <- stringResult[,c("Input", "Result", "experimental_score", "database_score", "textmining_score", "combined_score")]
          stringResults <- rbind(stringResults, stringResult)
        }
        incProgress(1/length(input_genes), detail = paste("Gene #", i, " of ", length(input_genes), sep=""))
        i <- i + 1
      }
    })

    stringResults$wasInput <- c("no","yes")[1+stringResults$Result %in% stringResults$Input]

    stringResults(stringResults)

  })


  stringHistogram <- eventReactive(c(input$renderSTRING, input$stringConf, input$wasInputSTRING), {
    if (is.null(stringResults())) {
      return()
    }

    data <- filter(stringResults(), combined_score >= input$stringConf)

    if (length(input$wasInputSTRING) == 1) {
      if (input$wasInputSTRING == "yes") {
        data <- filter(data, wasInput == "yes")
      } else {
        data <- filter(data, wasInput == "no")
      }
    }

    if (nrow(data) == 0) {
      return()
    }

    #generate histogram of STRING scores
    ggplot(data = data,aes(x=combined_score)) +
      geom_histogram(binwidth = 25, color=1, fill="white", na.rm = TRUE) +
      xlim(0,1000) +
      geom_vline(xintercept = input$stringConf, color="red") +
      xlab("combined_score")
  })



  output$stringDBhistogram <- renderPlot({
    stringHistogram()
  })



  stringPlot <- eventReactive(c(input$renderSTRING, input$stringConf, input$wasInputSTRING), {

    if (is.null(stringResults())) {
      return()
    }

    data <- filter(stringResults(), combined_score >= input$stringConf)

    if (length(input$wasInputSTRING) == 1) {
      if (input$wasInputSTRING == "yes") {
        data <- filter(data, wasInput == "yes")
      } else {
        data <- filter(data, wasInput == "no")
      }
    }

    if (nrow(data) == 0) {
      return()
    }

    ggplot(data = data,aes(x=combined_score, y = Input)) +
      geom_point(shape=5, alpha=0.9, show.legend = TRUE, aes(color=wasInput)) +
      xlim(0,1000) + scale_y_discrete(limits=rev) +
      scale_color_brewer(palette = "Set2", name<-"Present in input gene panel") +
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "grey95",colour = "grey95",
                                            size = 1, linetype = "solid")) +
      geom_vline(xintercept = input$stringConf, color="red") +
      xlab("combined_score")
  })



  output$stringDBplot <- renderPlot({
    stringPlot()
  })



  stringTable <- eventReactive(c(input$renderSTRING, input$stringConf, input$wasInputSTRING, input$allSTRINGScores), {

    if (is.null(stringResults())) {
      return()
    }
    gp <- get_input_genes(genesRV(), input, filtered = TRUE)

    # sort by decreasing score
    stringResults <- stringResults()[order(stringResults()$combined_score, decreasing = TRUE),]

    stringResults <- filter(stringResults, combined_score >= input$stringConf)

    if (length(input$wasInputSTRING) == 1) {
      if (input$wasInputSTRING == "Yes") {
        stringResults <- filter(stringResults, wasInput == "yes")
      } else {
        stringResults <- filter(stringResults, wasInput == "no")
      }
    }

    if (input$allSTRINGScores == "No") {
      drop <- c("experimental_score", "database_score", "textmining_score")
      stringResults <- stringResults[,!(names(stringResults) %in% drop)]
    }

    newGenes <- as.character(unique(stringResults$Result))
    newGenes <- newGenes[!(newGenes %in% gp)] # remove results if they are in the input panel
    newGenesText <- paste(unique(newGenes), collapse=" ") # prepare space-separated text block
    # update the lower text area with the result text block
    updateTextAreaInput(inputId = "stringTextOutput", value = newGenesText)

    # save a copy of the table in an Output folder.
    # The Download button will get it from there.
    if (!dir.exists("Output")) {dir.create("Output")}
    write.csv(stringResults,"Output/stringResults.csv", row.names = FALSE, quote = FALSE)

    return(stringResults)
  })



  output$sdbtable <- renderDataTable({
    stringTable()
  }, options = list(autoWidth=TRUE))




  # Downloadable csv of STRING Results ----
  output$downloadSTRING <- downloadHandler(
    filename = function() {
      "stringResults.csv"
    },
    content = function(file) {
      f <- read.csv("Output/stringResults.csv")
      write.csv(f, file, row.names = FALSE)
    })

  observeEvent(input$addSTRINGResultsToInput, {
    #add result genes to custom input genes
    genes <- append(unlist(strsplit(input$addGenes, split = " ")), unlist(strsplit(input$stringTextOutput, split = " ")))
    genes <- unique(genes)
    genes <- paste(genes, collapse=" ")

    updateTextAreaInput(inputId = "addGenes",
                        value = genes)

    #redirect user to panel input
    updateTabsetPanel(session, "mainTabs", "Panel Input")
  })



################################################################################
# GTEX FUNCTIONALITY
################################################################################

  updateCheckboxGroupInput(inputId = 'advGTExTissue',
                           choices = colnames(gtexmatFiltered))

  observeEvent(input$advGTExTissue, {
    brain <- c("Brain...Amygdala",
               "Brain...Anterior.cingulate.cortex..BA24.",
               "Brain...Caudate..basal.ganglia.",
               "Brain...Cerebellar.Hemisphere",
               "Brain...Cerebellum",
               "Brain...Cortex",
               "Brain...Frontal.Cortex..BA9.",
               "Brain...Hippocampus",
               "Brain...Hypothalamus",
               "Brain...Nucleus.accumbens..basal.ganglia.",
               "Brain...Putamen..basal.ganglia.",
               "Brain...Spinal.cord..cervical.c.1.",
               "Brain...Substantia.nigra",
               "Muscle...Skeletal",
               "Nerve...Tibial",
               "Pituitary")
    immune <- c("Cells...EBV.transformed.lymphocytes",
                "Spleen",
                "Thyroid",
                "Whole.Blood")

    #note that order of lists matters for identical(list1, list2)
    if (identical(input$advGTExTissue, brain)) {
      updateSelectizeInput(inputId = "gtexTissue", selected = "Brain")
    } else if (identical(input$advGTExTissue, immune)) {
      updateSelectizeInput(inputId = "gtexTissue", selected = "Immune System")
    } else if (identical(input$advGTExTissue, colnames(gtexmatFiltered))) {
      updateSelectizeInput(inputId = "gtexTissue", selected = "All")
    } else {
      updateSelectizeInput(inputId = "gtexTissue", selected = "Advanced")
    }

  })

  observeEvent(input$gtexTissue, {
    if (input$gtexTissue == "Advanced") {
      return()
    } else if (input$gtexTissue == "Brain") {
      updateCheckboxGroupInput(inputId = "advGTExTissue",
                               selected = c("Brain...Amygdala",
                                            "Brain...Anterior.cingulate.cortex..BA24.",
                                            "Brain...Caudate..basal.ganglia.",
                                            "Brain...Cerebellar.Hemisphere",
                                            "Brain...Cerebellum",
                                            "Brain...Cortex",
                                            "Brain...Frontal.Cortex..BA9.",
                                            "Brain...Hippocampus",
                                            "Brain...Hypothalamus",
                                            "Brain...Nucleus.accumbens..basal.ganglia.",
                                            "Brain...Putamen..basal.ganglia.",
                                            "Brain...Spinal.cord..cervical.c.1.",
                                            "Brain...Substantia.nigra",
                                            "Pituitary",
                                            "Muscle...Skeletal",
                                            "Nerve...Tibial"))
    } else if (input$gtexTissue == "Immune System") {
      updateCheckboxGroupInput(inputId = "advGTExTissue",
                               selected = c("Cells...EBV.transformed.lymphocytes",
                                            "Spleen",
                                            "Thyroid",
                                            "Whole.Blood"))
    } else if (input$gtexTissue == "All") {
      updateCheckboxGroupInput(inputId = "advGTExTissue",
                               selected = colnames(gtexmatFiltered))
    }
  })


  # Downloadable csv of GTEx Results ----
  output$downloadGTEX <- downloadHandler(
    filename = function() {
      "gtexResults.csv"
    },
    content = function(file) {
      f <- read.csv("Output/gtexResults.csv")
      print(head(f))
      write.csv(f, file, row.names = FALSE)
    }
    )

  gtexClusters <- reactiveVal()

  output$downloadGTEXClusters <- downloadHandler(
    filename = function(){"gtex_clusters_table.csv"},
    content = function(file) {
      write.csv(gtexClusters(), file, row.names = FALSE, quote = FALSE)
    }
  )

  gtexResults <- reactiveVal()

  observeEvent(input$gtexClusterStart, {

    progressiveGTEx(NULL)
    updateTextAreaInput(inputId = "gtexProgressiveTextOutput",
                        value = "")

    #get gtex table of input genes against input genes
    inGenes <- unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol)
    if (length(inGenes) == 0) {
      return()
    }
    withProgress(message = 'Evaluating GTEx coexpression...', value = 0, max = 1, {
      ref <- switch(input$gtexTissue,
                    "Brain" = gtexmat.brain,
                    "Immune System" = gtexmat.immune,
                    "All" = gtexmatFiltered)

      gtexTable <- inspectGTExPanel(inGenes, input$advGTExTissue)
    })
    gtexResults(gtexTable)
    #create clusters
    df <- data.matrix(gtexTable[gtexTable$Gene %in% inGenes,-(1:2)])

    clust <- agnes(df, method = "ward")

    #split into clusters based on # clusters input
    if (input$numClusters == 0 | input$numClusters > length(inGenes)) {

      #calculate gap statistic for each number of clusters then calculate k
      gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = length(gtexTable) - 3, B = 50)

      output$gtexGapStatistic <- renderPlot({
        fviz_gap_stat(gap_stat)
      }, height = 200)

      k <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method="Tibs2001SEmax")
      updateNumericInput(inputId = "numClusters",
                         value = k)

      clusters_df <- as.data.frame(cbind(names(cutree(clust, k=k)), cutree(clust, k=k)))

      colnames(clusters_df) <- c("Gene", "Cluster")
      rownames(clusters_df) <- clusters_df$Gene
      clusters_df$Cluster <- as.numeric(clusters_df$Cluster)
      clusters_df <- clusters_df[order(clusters_df$Cluster),]

    } else {
      clusters_df <- as.data.frame(cbind(names(cutree(clust, k=input$numClusters)), cutree(clust, k=input$numClusters)))

      colnames(clusters_df) <- c("Gene", "Cluster")
      rownames(clusters_df) <- clusters_df$Gene
      clusters_df$Cluster <- as.numeric(clusters_df$Cluster)
      clusters_df <- clusters_df[order(clusters_df$Cluster),]
    }

    gtexClusters(clusters_df)

    #prepare dendrogram to pass as argument in Heatmap() to plot clusters
    dend <- as.dendrogram(clust)
    clusters_df <- clusters_df[match(labels(dend), clusters_df$Gene),]
    d <- color_branches(dend, clusters = clusters_df$Cluster)

    #produce heatmap/dendrogram plots
    output$gtexHeatmap <- renderPlot ({
      col_fun = colorRamp2(breaks = seq(-1,1,0.02), colors = viridis(101))

      Heatmap(df,
              col=col_fun,
              column_names_gp = gpar(fontsize=6, fontfamily="Helvetica"),
              row_names_gp = gpar(fontsize=6, fontfamily="Helvetica"),
              heatmap_legend_param = list(title = "Correlation",
                                          at = c(-1,0,1)),
              heatmap_height = unit(8,"cm"),
              heatmap_width = unit(8,"cm"),
              clustering_distance_rows = "canberra",
              clustering_distance_columns = "canberra")
    }, height = 200)

    output$gtexDendrogram <- renderPlot({

      plot(d)

    })

    output$gtexClusters <- renderDataTable({
      clusters_df
    })

    updateRadioButtons(inputId = "chooseCluster",
                       choices = append(c("All"), sort(unique(clusters_df$Cluster))))
  })

  #update the cluster gene preview when user selects a new cluster
  observeEvent(input$chooseCluster, {

    if (input$chooseCluster == "All") {
      updateTextAreaInput(inputId = "gtexClusterPreview",
                          value = paste(gtexClusters()$Gene, collapse=" "))
      return()
    }

    if (input$chooseCluster != "null") {
      updateTextAreaInput(inputId = "gtexClusterPreview",
                          value = paste(gtexClusters()$Gene[gtexClusters()$Cluster == input$chooseCluster], collapse=" "))
    }

  })



  gtexTable_filtered <- reactiveVal()

  observeEvent(input$gtexStart, {

    inGenes <- NULL
    if (input$chooseCluster == "All") {
      inGenes <- unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol)
    } else {
      inGenes <- gtexClusters()$Gene[gtexClusters()$Cluster == input$chooseCluster]
    }

    if (sum(inGenes %in% colnames(gtexResults())) == length(inGenes)) {
      gtexTable <- gtexResults()
      gtexTable <- select(gtexTable, append(c("Gene"), inGenes))
    } else {
      withProgress(message = 'Evaluating GTEx coexpression...', value = 0, max = 1, {
        ref <- switch(input$gtexTissue,
                      "Brain" = gtexmat.brain,
                      "Immune System" = gtexmat.immune,
                      "All" = gtexmatFiltered)
        gtexTable <- inspectGTExPanel(inGenes, input$advGTExTissue)
      })
    }

    gtexTable <- mutate(gtexTable, Sum = rowSums(across(where(is.numeric))))

    #convert Sum column to ratio
    gtexTable_filtered <- mutate(gtexTable, Avg = Sum / length(inGenes), Sum = NULL)
    gtexTable_filtered <- relocate(gtexTable_filtered, Avg, .after=Gene)
    gtexTable_filtered <- mutate(gtexTable_filtered, Cluster = input$chooseCluster)

    #filter gtexTable
    gtexTable_filtered <- filter(gtexTable_filtered, Avg >= input$gtexAvgMin)
    gtexTable_filtered <- filter(gtexTable_filtered, Avg <= input$gtexAvgMax)

    #sort by avg (decreasing/increasing)
    if (input$gtexResultSort == "Increasing") {
      gtexTable_filtered <- gtexTable_filtered[order(gtexTable_filtered$Avg),]
    } else if (input$gtexResultSort == "Decreasing") {
      gtexTable_filtered <- gtexTable_filtered[order(gtexTable_filtered$Avg, decreasing=TRUE),]
    }

    #remove input genes
    gtexTable_filtered <- filter(gtexTable_filtered, ! Gene %in% unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol))

    #restrict # result genes (upper limit)
    gtexTable_filtered <- head(gtexTable_filtered, input$gtexNumResultGenes)

    #update reactive value
    gtexTable_filtered(gtexTable_filtered)

    #updatetextareainput
    updateTextAreaInput("gtexTextOutput", session=session,
                        value = paste(unique(gtexTable_filtered$Gene), collapse=" "))

    #create download directory
    if (!dir.exists("Output")) {dir.create("Output")}
    write.csv(gtexTable_filtered,"Output/gtexResults.csv", row.names = FALSE)

    #create heatmap
    output$gtexClusterHeatmap <- renderPlot({

      col_fun = colorRamp2(breaks = seq(-1,1,0.02), colors = viridis(101))

      Heatmap(as.matrix(gtexTable[gtexTable$Gene %in% inGenes,-(1:2)]),
              col=col_fun,
              column_names_gp = gpar(fontsize=6, fontfamily="Helvetica"),
              row_names_gp = gpar(fontsize=6, fontfamily="Helvetica"),
              heatmap_legend_param = list(title = "Correlation",
                                          at = c(-1,0,1)),
              heatmap_height = unit(8,"cm"),
              heatmap_width = unit(8,"cm"),
              clustering_distance_rows = "canberra",
              clustering_distance_columns = "canberra")
    }, height = 200)

    #create radar plot
    output$gtexRadar <- renderPlot ({
      radarData <- gtexTable[gtexTable$Gene %in% inGenes,-(2)]
      radarData<- radarData %>%
        mutate_at(vars(-Gene), rescale)
      ggradar(plot.data = radarData, group.line.width = 1,
              font.radar = "Helvetica",
              grid.label.size = 3,
              axis.label.size = 3,legend.text.size = 6,plot.legend = FALSE,
              group.colours = hue_pal()(length(inGenes)),
              group.point.size = 3)
    }, height = 200)

    #output table
    output$gtextable <- renderDataTable(gtexTable_filtered)

  })

  output$clusterGeneCount <- renderText({
    if (input$chooseCluster == "All") {
      paste("Gene Count:", length(unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol)), sep = " ")
    } else {
      paste("Gene Count:", length(gtexClusters()$Gene[gtexClusters()$Cluster == input$chooseCluster]), sep = " ")
    }

  })

  output$gtexResultCount <- renderText({
    paste("Gene Count:", length(unique(gtexTable_filtered()$Gene)), sep=" ")
  })

  output$gtexProgressiveResultCount <- renderText({
    paste("Gene Count:", length(unique(unlist(strsplit(input$gtexProgressiveTextOutput, split=" ")))), sep=" ")
  })

  progressiveGTEx <- reactiveVal()

  observeEvent(input$gtexAppendToProgressive, {
    genes <- append(unlist(strsplit(input$gtexProgressiveTextOutput, split = " ")), unlist(strsplit(input$gtexTextOutput, split = " ")))
    genes <- unique(genes)
    genes <- paste(genes, collapse = " ")
    updateTextAreaInput(inputId = "gtexProgressiveTextOutput",
                        value = genes)

    progressive_results <- progressiveGTEx()
    if (length(gtexTable_filtered()) == 0) {
      return()
    }
    progressive_results <- rbind(progressive_results, select(gtexTable_filtered(), Gene, Avg, Cluster))

    progressive_results <- distinct(progressive_results)
    #add actual cluster genes? might be useful

    progressiveGTEx(progressive_results)

  })

  observeEvent(input$gtexClearProgressive, {
    updateTextAreaInput(inputId = "gtexProgressiveTextOutput",
                        value = "")
    progressiveGTEx(NULL)
  })

  output$individualGTExTable <- renderDataTable({
    ref <- gtexmatFiltered[,colnames(gtexmatFiltered) %in% input$advGTExTissue]
    ref <- as.data.frame(ref)
    ref <- add_column(ref, Gene = rownames(ref))
    ref <- relocate(ref, Gene)
    result <- filter(ref, Gene %in% unlist(strsplit(input$individualGTExGenes, split=" ")))
    output$individualGTExDownload <- downloadHandler(
      filename = function(){"individual_gtex_results.csv"},
      content = function(file) {
        write.csv(result, file, row.names = FALSE, quote = FALSE)
      }
    )
    result
  })


################################################################################
# COMBO FUNCTIONALITY
################################################################################

  comboText <- reactiveVal(value = "")

  output$comboText <- renderText({
    comboText()
  })

  comboTable <- eventReactive(input$comboStart, {

    table <- progressiveGTEx()

    if ((is.null(table) | length(table) == 0) & (is.null(stringResults()) | length(stringResults()) == 0)) {
      comboText("Please compute STRING & GTEx results before creating table.")
      return()
    }
    if (is.null(table) | length(table) == 0) {
      comboText("Please compute GTEx results before creating table.")
      return()
    }

    if (is.null(stringResults()) | length(stringResults()) == 0) {
      comboText("Please compute STRING results before creating table.")
      return()
    }

    comboText("")

    withProgress(message = "Combining STRING and GTEx results", min=0, max=1, {

      #make a max STRING score column
      #all string scores column with list of (SCORE, INPUT_GENE) tuples
      table <- add_column(table, max_STRING_score = 0)
      table <- add_column(table, STRING_input_gene = NA)
      table <- add_column(table, STRING_score = NA)
      for (i in 1:length(table$Gene)) {

        string_matches <- filter(stringResults(), Result == table$Gene[i])

        string_matches <- string_matches[order(-string_matches$combined_score),]
        #prevent warning msgs by first checking if string_matches has > 0 rows
        if (length(string_matches$combined_score) == 0) {
          #pass
        } else {
          table$max_STRING_score[i] <- max(string_matches$combined_score)
          table$STRING_input_gene[i] <- paste(string_matches$Input, collapse="<br>")
          table$STRING_score[i] <- paste(string_matches$combined_score, collapse="<br>")
        }
        incProgress(amount = 1/length(table$Gene))
      }

    })

    #add STRING results that didn't show up in GTEx
    STRING_results <- filter(stringResults(), ! Result %in% table$Gene)
    for (gene in unique(STRING_results$Result)) {
      string_matches <- filter(STRING_results, Result == gene)
      string_matches <- string_matches[order(-string_matches$combined_score),]
      table[nrow(table) + 1,] <- c(gene, 0, 0, max(string_matches$combined_score), paste(string_matches$Input, collapse="<br>"), paste(string_matches$combined_score, collapse="<br>"))
    }

    if (input$comboNAvalues == "Yes") {
      table <- filter(table, max_STRING_score >= input$comboMaxSTRINGFilter | max_STRING_score == 0)
      table <- filter(table, Avg >= input$comboMinAvgFilter | Cluster == 0)
      table <- filter(table, Avg <= input$comboMaxAvgFilter | Cluster == 0)
    } else {
      table <- filter(table, max_STRING_score >= input$comboMaxSTRINGFilter & max_STRING_score > 0)
      table <- filter(table, Avg >= input$comboMinAvgFilter & Cluster != 0)
      table <- filter(table, Avg <= input$comboMaxAvgFilter & Cluster != 0)
    }



    table <- filter(table, ! Gene %in% unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol))

    STRING_weight <- input$comboRankWeighting*2
    GTEx_weight <- (1-input$comboRankWeighting)*2

    table <- mutate(table, Rank = rank(1/(as.numeric(Avg)*GTEx_weight + (as.numeric(max_STRING_score)/1000)*STRING_weight), ties.method = "min"))
    table <- table[order(table$Rank),]
    table <- relocate(table, Rank, .after=Gene)

    table <- rename(table, GTEx_Avg = Avg)
    table <- rename(table, GTEx_Cluster = Cluster)

    table$GTEx_Avg[table$GTEx_Avg == 0 & table$GTEx_Cluster == 0] <- NA
    table$GTEx_Cluster[is.na(table$GTEx_Avg) & table$GTEx_Cluster == 0] <- NA
    table$max_STRING_score[table$max_STRING_score == 0] <- NA


    updateTextAreaInput(inputId = "comboResultGenes", value = paste(unique(table$Gene), collapse = " "))

    if (input$comboRankColumn == "No") {
      table <- table[, !(colnames(table) == "Rank")]
    }

    table
  })


  output$comboTable <- renderDataTable({

    table <- comboTable()

    output$downloadCombo <- downloadHandler(
      filename = function(){"combination_results.csv"},
      content = function(file) {
        write.csv(table, file, row.names = FALSE, quote = FALSE)
      }
    )

    table

  }, escape = FALSE)


  #unused
  output$comboPlot <- renderPlot({

    return()
    ref <- switch(input$gtexTissue,
                  "Brain" = gtexmat.brain,
                  "Immune System" = gtexmat.immune,
                  "All" = gtexmatFiltered)
    g <- data.frame(Correlation=getSimilarGTEx(input$gene, ref))
    g$Gene <- row.names(g)
    g <- g[order(g$Correlation, decreasing = TRUE),]

    sdb <- stringDBMatches(input$gene, cutoff = 0)
    colnames(sdb)[3] <- "Gene"
    result <- merge(g,sdb, by="Gene", all.x = TRUE)
    result$sdbScore <- as.integer(result$combined_score)
    result$sdbScore[is.na(result$combined_score)] <- 0

    # make a combined score
    result$score2 <- (result$Correlation^6 * input$gtexWeight) + result$sdbScore
    result <- result[order(result$score2, decreasing = TRUE),]
    result$input <- input$gene
    result <- result[-which(result$input==result$Gene),] # remove the input gene from results
    result$rank <- 1:length(result$Gene)
    result$label <- ""
    result$label[1:10] <- result$Gene[1:10]
    for (j in 1:10) {
      result$label[j] <- paste0(j,") ", result$label[j])
    }
    # label up to ten best protein-protein links with score
    k=0
    for (j in which(result$sdbScore>0)) {
      if(k<10) {
        result$label[j] <- paste0(j,") ", result$Gene[j])
        k<-k+1
      }
    }

    ggplot(result, aes(x=Correlation, y=score2, label=label)) +
      geom_point(alpha=0.25, size=(1+result$sdbScore/100)) + xlim(c(-1,1)) +
      geom_label_repel(box.padding = 0.4, max.overlaps = Inf, size=3,
                       segment.size = 0.25, segment.color = '#636363') +
      xlab("GTEx spatial correlation") + ylab("Combined score (GTEx & STRING)") +
      ggtitle(paste0("Input: ",input$gene))

  })

  #unused
  output$rankTable <- renderDataTable({ # Generate rank result table


  })


################################################################################
# HPO FUNCTIONALITY
################################################################################


  # Downloadable csv of HPO Results ----
  output$downloadHPO <- downloadHandler(
    filename = function() {
      "hpoResults.csv"
    },
    content = function(file) {
      f <- read.csv("Output/hpoResults.csv")
      print(head(f))
      write.csv(f, file, row.names = FALSE)
    }
  )

  output$countHpo <- renderText({
    paste0("Gene Count: ", length(textblock2genes(input$hpoTextOutput, input$useOMIM=="Yes")))
  })


  observeEvent(input$hpoStart, {
    #   # HPO START button clicked
    #................................
    #
    print("HPO START")
    withProgress(message = 'Evaluating HPO phenotypes', value = 0, max = 1, {

      inGenes <- unique(get_input_genes(genesRV(), input, filtered = TRUE)$symbol)

      overlapTable <- hpoOverlap(inGenes)

      # save a copy of the table in an Output folder.
      # The Download button will get it from there.
      if (!dir.exists("Output")) {dir.create("Output")}
      write.csv(overlapTable,"Output/hpoResults.csv", row.names = FALSE)

      percentOrder <- order(overlapTable$overlapPercent, decreasing = TRUE)
      idx <- unique(c(1:10, percentOrder[1:10]))

      # update the hpoResultDropdown list with result phenotypes
      updateCheckboxGroupInput(inputId = "hpoChecksCount",
                               choices = overlapTable$phenotypes[1:20])
      updateCheckboxGroupInput(inputId = "hpoChecksPercent",
                               choices = overlapTable$phenotypes[percentOrder[1:20]])

    })

  })

  observeEvent(input$hpoMakePlots, {
    #   # HPO PLOT button clicked
    #................................
    #   # Update result displays
    output$hpoPlot <- renderPlot({
      print("HPO PLOT")
      inGenes <- unique(get_input_genes(genesRV(), input, filtered=TRUE)$symbol)
      # overlapTable <- hpoOverlap(inGenes)
      # ^ reload from saved file rather than calculate again
      overlapTable <- read.csv("Output/hpoResults.csv", header = TRUE)

      # filter the top phenotypes based on user selections
      selected.phenotypes <- unique(c(input$hpoChecksCount, input$hpoChecksPercent))
      idx <- which(overlapTable$phenotypes %in% selected.phenotypes)

      print("Selected:")
      print(overlapTable$phenotypes[idx])

      # update the hpoResultDropdown list with result phenotypes
      updateSelectizeInput(inputId = "hpoResultDropdown", server=TRUE,
                           choices = overlapTable$phenotypes[idx])

      pgenes <- NULL
      for (p in overlapTable$phenotypes[idx]) {
        pgenes <- c(pgenes,(unique(hpo_p2g$Gene[hpo_p2g$Phenotype==p])))
      }
      unique.genes <- unique(pgenes)
      geneCounts <- integer(length = length(unique.genes))
      for (i in 1:length(unique.genes)) {
        geneCounts[i] <- length(which(pgenes == unique.genes[i]))
      }
      inInputGenes <- c("No","Yes")[1+as.integer(unique.genes %in% inGenes)]
      topHpoGenes <- data.frame(gene = unique.genes, counts = geneCounts, inInput = inInputGenes)
      topHpoGenes <- topHpoGenes[order(topHpoGenes$counts, decreasing = TRUE),] # sort recurring genes to the top
      if (input$hpoShowInput == "No") {
        topHpoGenes <- topHpoGenes[topHpoGenes$inInput=="No",] # remove input genes
      }
      write.csv(topHpoGenes,"Output/topHpoGenes.csv", row.names = FALSE)

      # update the lower text area with the result text block
      updateTextAreaInput(inputId = "hpoTextOutputRecurringGenes",
                          value = paste(topHpoGenes$gene[1:20],collapse = " "))

      # Set of 3 plots
      hpoPlot1 <- ggplot(overlapTable[idx,], aes(y=reorder(phenotypes, overlap), x=overlap)) +
        geom_col(width = 0.7) + ylab("HPO Phenotype") + xlab("Overlap with Input") +
        geom_text(aes(label=overlapPercentLabel), nudge_x = 2) +
        theme_light()
      hpoPlot2 <- ggplot(overlapTable[idx,], aes(y=overlap, x=overlapPercent)) +
        geom_point() + ylab("Overlap Count") + xlab("Overlap Percentage") +
        theme_light() +
        theme(panel.grid.minor = element_blank()) +
        geom_hline(yintercept = 0, alpha=0.5) + geom_vline(xintercept = 0, alpha=0.5) +
        scale_x_continuous(expand = expansion(mult=0.5), breaks = seq(0,100,25)) +
        scale_y_continuous(expand = expansion(mult=0.5)) +
        geom_text_repel(aes(label=phenotypes), max.overlaps=Inf, size=2.5,
                        min.segment.length = 0, box.padding = 1, segment.alpha=0.2)
      hpoPlot3 <- ggplot(topHpoGenes[1:min(20,length(topHpoGenes$gene)),],
                         aes(y=reorder(gene,counts), x=counts, fill=inInput)) +
        geom_col(width = 0.7) + labs(fill="Input panel gene") +
        ylab("Gene") + xlab("# inclusions in top Phenotypes") +
        theme_light()
      grid.arrange(hpoPlot1, hpoPlot2, hpoPlot3, nrow=3)
    })
  })

  observe({ # when the dropdown changes, update the lower text area with the result text block
    x <- input$hpoResultDropdown
    hpoResultGenes <- as.character(unique(hpo_p2g$Gene[hpo_p2g$Phenotype == x]))
    updateTextAreaInput(session = session,
                        inputId = "hpoTextOutput",
                        label=paste0("Genes in HPO phenotype: ",x),
                        value = paste(hpoResultGenes, collapse = ' '))
  })


  output$countOverlap1 <- renderText({
    paste0("Gene Count: ", length(textblock2genes(input$overlap1TextArea, input$useOMIM=="Yes")))
  })
  output$countOverlap2 <- renderText({
    paste0("Gene Count: ", length(textblock2genes(input$overlap2TextArea, input$useOMIM=="Yes")))
  })


  # generate venn diagram of two panels
  output$overlapVenn <- renderPlot({
    data <- list("Set 1"=textblock2genes(input$countOverlap1, input$useOMIM=="Yes"),
                 "Set 2"=textblock2genes(input$countOverlap2, input$useOMIM=="Yes"))
    names(data) <- c(input$overlap1name, input$overlap2name)
    #data <- list("Set 1"=c("ANO3"),"Set 2",c("ANO3","VPS16"))# minimal test
    display_venn(data)
  })



################################################################################
# GENE ONTOLOGY
################################################################################

  annotations <- eventReactive(input$renderGOAnnotations, {

    queryGenes <- unique(unlist(strsplit(input$annotationInput, split=" ")))
    if (input$annotationIncludeInput == "Yes") {
      queryGenes <- append(queryGenes, unique(get_input_genes(genesRV(), input, filtered=TRUE)$symbol))
    }


    results <- data.frame(Gene=character(),
                          Qualifier=character(),
                          GO_ID=character())

    withProgress(message = "Retrieving GO Annotations", value = 0, {
      i <- 1
      for (gene in queryGenes) {
        results <- rbind(results, filter(GO_annotations, `DB Object Symbol` == gene)[,c("DB Object Symbol", "Qualifier", "GO ID", "Aspect")])
        incProgress(1/length(queryGenes), detail = paste("Gene #", i, " of ", length(queryGenes), sep=""))
        i <- i + 1
      }
    })

    results$Aspect <- replace(results$Aspect, results$Aspect == "C", "Cellular Component")
    results$Aspect <- replace(results$Aspect, results$Aspect == "F", "Molecular Function")
    results$Aspect <- replace(results$Aspect, results$Aspect == "P", "Biological Process")

    results <- add_column(results, Plaintext=NA)

    if (nrow(results) > 0) {
      withProgress(message = "Retrieving plaintext descriptions", value = 0, {
        for (i in 1:nrow(results)) {
          results$Plaintext[i] <- filter(GO_terms, id == results$`GO ID`[i])$name
          incProgress(1/nrow(results))
        }
      })
    }

    results <- distinct(results)

    updateSelectizeInput("annotationPlaintext", session=session, choices = unique(results$Plaintext))

    return(results)
  })



  output$GOannotations <- renderDataTable({
    inGenes <- unique(get_input_genes(genesRV(), input, filtered=TRUE)$symbol)
    GO_annotations <- annotations()
    GO_annotations <- filter(GO_annotations, Aspect %in% input$annotationAspect)
    if (length(input$annotationPlaintext) > 0) {
      GO_annotations <- filter(GO_annotations, Plaintext %in% input$annotationPlaintext)
    }
    if (length(input$qualifierGO) > 0) {
      GO_annotations <- filter(GO_annotations, Qualifier %in% input$qualifierGO)
    }
    df <- data.frame(matrix(ncol=6,nrow=0))
    colnames(df) <- c("Gene", "Qualifiers", "GO IDs", "Plaintexts", "Aspects", "Input")
    genes <- unique(GO_annotations$`DB Object Symbol`)
    withProgress(message = "Creating table", value = 0, {
      for (gene in genes) {
        Qualifiers <- c()
        Aspects <- c()
        `GO ID` <- c()
        Plaintext <- c()
        for (i in 1:nrow(GO_annotations)) {
          if (GO_annotations$`DB Object Symbol`[i] == gene) {
            Qualifiers <- append(Qualifiers, GO_annotations$Qualifier[i])
            `GO ID` <- append(`GO ID`, GO_annotations$`GO ID`[i])
            Plaintext <- append(Plaintext, GO_annotations$Plaintext[i])
            Aspects <- append(Aspects, GO_annotations$Aspect[i])
          }
        }

        if (gene %in% inGenes) {
          df[nrow(df) + 1,] <- c(gene, paste(Qualifiers, collapse = "<br>"), paste(`GO ID`, collapse="<br>"), paste(Plaintext, collapse="<br>"), paste(Aspects, collapse = "<br>"), TRUE)
        } else {
          df[nrow(df) + 1,] <- c(gene, paste(Qualifiers, collapse = "<br>"), paste(`GO ID`, collapse="<br>"), paste(Plaintext, collapse="<br>"), paste(Aspects, collapse = "<br>"), FALSE)
        }

        incProgress(1/length(genes))
      }

    })

    df
  }, escape = FALSE, options = list(autoWidth = TRUE))

  output$mostCommonAnnotations <- renderPlot({

    annotations <- annotations()
    annotations <- filter(annotations, Aspect %in% input$annotationAspect)
    if (length(input$annotationPlaintext) > 0) {
      annotations <- filter(annotations, Plaintext %in% input$annotationPlaintext)
    }
    if (length(input$qualifierGO) > 0) {
      annotations <- filter(annotations, Qualifier %in% input$qualifierGO)
    }

    if (length(annotations) == 0) {
      return()
    }

    unique_annotations <- distinct(annotations, Plaintext, Aspect)

    counts <- c()
    aspects <- c()
    withProgress(message = "Creating plot", value=0, min=0, max=1, {
      for (j in 1:length(unique_annotations$Plaintext)) {
        sum <- 0
        for (i in 1:length(annotations$Plaintext)) {
          if (annotations$Plaintext[i] == unique_annotations$Plaintext[j]) {
            sum <- sum + 1
          }
        }
        counts <- append(counts, sum)
        aspects <- append(aspects, unique_annotations$Aspect[j])
      }
      incProgress(1/length(unique_annotations$Plaintext))
    })


    if (length(counts) == 0) {
      return()
    }

    df <- data.frame(annotations=unique_annotations$Plaintext,
                     counts=counts,
                     Type=aspects)
    df <- df[order(-counts),]
    ggplot(df[1:10,], aes(x=fct_inorder(annotations), y=counts, fill=Type)) +
      geom_bar(stat = "identity") +
      xlab("plaintext GO ID") +
      ylab("Count")

  })






################################################################################
# VALIDATION FUNCTIONALITY
################################################################################

  clinvar <- read_tsv(file="Data/new/ClinVar/gene_condition_source_id.txt", show_col_types = FALSE)

  output$clinvarInfo <- renderDataTable({

    input_genes <- unlist(strsplit(input$validationGenes, split=" "))

    #filter
    if (length(input_genes) > 0) {
      clinvar <- filter(clinvar, AssociatedGenes %in% input_genes | RelatedGenes %in% input_genes)
    }
    if (length(input$validationDiseases) > 0) {
      clinvar <- filter(clinvar, grepl(input$validationDiseases, DiseaseName))
    }

    output$clinvarTable <- downloadHandler(
      filename = function(){"clinvar_gene_disease_associations.csv"},
      content = function(file) {
        write.csv(clinvar, file, row.names = FALSE, quote = FALSE)
      }
    )

    clinvar
  })

  variants <- read_tsv(file="Data/new/ClinVar/variant_summary.txt", show_col_types = FALSE)

  output$variantTable <- renderDataTable({

    input_genes <- unlist(strsplit(input$validationVariantGenes, split=" "))

    #filter
    if (length(input_genes) > 0) {
      variants <- filter(variants, GeneSymbol %in% input_genes)
    }
    if (length(input$variantClinicalSignificance) > 0) {
      variants <- filter(variants, ClinicalSignificance %in% input$variantClinicalSignificance)
    }

    output$variantTableDownload <- downloadHandler(
      filename = function(){"clinvar_gene_variant_info.csv"},
      content = function(file) {
        write.csv(variants, file, row.names = FALSE, quote = FALSE)
      }
    )

    variants
  })


  output$validationLocalTable <- renderDataTable({

    input_genes <- unlist(strsplit(input$validationLocalGenes, split=" "))

    table <- data.frame(Gene = input_genes)
    table <- add_column(table, `# Input Lists` = 0)

    #only count local gene panels that are in the same disease group as the currently selected disease(s)
    local_data <- filter(genesRV(), disease %in% input$disease)

    if (length(input_genes) > 0) {
      for (i in 1:nrow(table)) {
        table$`# Input Lists`[i] <- nrow(filter(local_data, symbol == table$Gene[i]))
      }
    }

    output$validationLocalTableDownload <- downloadHandler(
      filename = function(){"local_data_check.csv"},
      content = function(file) {
        write.csv(table, file, row.names = FALSE, quote = FALSE)
      }
    )

    table
  })

}
