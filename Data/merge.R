genes <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(genes) <- c("symbol", "disease", "panel", "panel_name", "evidence_level")

disgenet <- read_tsv(file = "Data/new/DisGeNET/disgenet.tsv", show_col_types = FALSE)
for (i in 1:nrow(disgenet)) {
  #unique(disgenet$EL_gda) = c("strong", NA, "definitive", "moderate", "limited")
  evidence_level <- disgenet$EL_gda[i]
  genes[nrow(genes) + 1,] = c(disgenet$Gene[i], disgenet$Disease[i], "DisGeNET", NA, evidence_level)
}

hpo <- read_tsv(file = "Data/new/HPO/HPO_genes.tsv", show_col_types = FALSE)
for (i in 1:nrow(hpo)) {
  genes[nrow(genes) + 1,] = c(hpo$gene_symbol[i], hpo$phenotype[i], "HPO", NA, NA)
}

dystonia_kishore <- read.table(file = "Data/dystoniaGenes.txt", col.names="Gene")
for (i in 1:nrow(dystonia_kishore)) {
  genes[nrow(genes) + 1,] = c(dystonia_kishore$Gene[i], "Dystonia", "Avi/Kishore", NA, NA)
}

dystonia_invitae <- read.table(file = "Data/new/Invitae/dystonia.txt", col.names = "Gene")
for (i in 1:nrow(dystonia_invitae)) {
  genes[nrow(genes) + 1,] = c(dystonia_invitae$Gene[i], "Dystonia", "Invitae", NA, NA)
}

dystonia_blueprint <- read_tsv(file = "Data/new/BlueprintGenetics/dystonia.txt", show_col_types = FALSE)
for (i in 1:nrow(dystonia_blueprint)) {
  genes[nrow(genes) + 1,] = c(dystonia_blueprint$Gene[i], "Dystonia", "Blueprint Genetics", NA, NA)
}

dystonia_panelapp <- read_tsv(file = "Data/new/PanelApp_Aus/Dystonia_Superpanel.tsv", show_col_types = FALSE)
for (i in 1:nrow(dystonia_panelapp)) {
  
  evidence_level <- NA
  if (grepl("Expert Review Green", dystonia_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "strong"
  } else if (grepl("Expert Review Amber", dystonia_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "moderate"
  } else if (grepl("Expert Review Red", dystonia_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "limited"
  }
  
  genes[nrow(genes) + 1,] = c(dystonia_panelapp$`Gene Symbol`[i], "Dystonia", "PanelApp Australia - Superpanel", NA, evidence_level)
}

dystonia_genomicseng <- read_tsv(file = "Data/new/PanelApp_Eng/Adult_onset_dystonia.tsv", show_col_types = FALSE) 
for (i in 1:nrow(dystonia_genomicseng)) {
  
  evidence_level <- NA
  if (grepl("Expert Review Green", dystonia_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "strong"
  } else if (grepl("Expert Review Amber", dystonia_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "moderate"
  } else if (grepl("Expert Review Red", dystonia_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "limited"
  }
  
  genes[nrow(genes) + 1,] = c(dystonia_genomicseng$`Gene Symbol`[i], "Dystonia", "Genomics England - Adult-onset", NA, evidence_level)
}

alzheimers_invitae <- read.table(file = "Data/new/Invitae/early_onset_alzheimer's_disease.txt", col.names = "Gene")
for (i in 1:nrow(alzheimers_invitae)) {
  genes[nrow(genes) + 1,] = c(alzheimers_invitae$Gene[i], "Alzheimer's Disease", "Invitae - Early-onset", NA, NA)
}

asd_blueprint <- read_tsv(file = "Data/new/BlueprintGenetics/asd.txt", show_col_types = FALSE)
for (i in 1:nrow(asd_blueprint)) {
  genes[nrow(genes) + 1,] = c(asd_blueprint$Gene[i], "Autism Spectrum Disorders", "Blueprint Genetics", NA, NA)
}

asd_panelapp <- read_tsv(file = "Data/new/PanelApp_Aus/Autism.tsv", show_col_types = FALSE)
for (i in 1:nrow(asd_panelapp)) {
  
  evidence_level <- NA
  if (grepl("Expert Review Green", asd_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "strong"
  } else if (grepl("Expert Review Amber", asd_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "moderate"
  } else if (grepl("Expert Review Red", asd_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "limited"
  }
  
  genes[nrow(genes) + 1,] = c(asd_panelapp$`Gene Symbol`[i], "Autism Spectrum Disorders", "PanelApp Australia", NA, evidence_level)
}

asd_genomicseng <- read_tsv(file = "Data/new/PanelApp_Eng/Autism.tsv", show_col_types = FALSE) 
for (i in 1:nrow(asd_genomicseng)) {
  
  evidence_level <- NA
  if (grepl("Expert Review Green", asd_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "strong"
  } else if (grepl("Expert Review Amber", asd_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "moderate"
  } else if (grepl("Expert Review Red", asd_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "limited"
  }
  
  genes[nrow(genes) + 1,] = c(asd_genomicseng$`Gene Symbol`[i], "Autism Spectrum Disorders", "Genomics England", NA, evidence_level)
}

huntington <- read.table(file = "Data/huntingtonGenes.txt", col.names="Gene")
for (i in 1:nrow(huntington)) {
  genes[nrow(genes) + 1,] = c(huntington$Gene[i], "Huntington's Disease", "old .txt", NA, NA)
}

motor_old <- read.table(file = "Data/motorGenes.txt", col.names="Gene")
for (i in 1:nrow(motor_old)) {
  genes[nrow(genes) + 1,] = c(motor_old$Gene[i], "Motor Development Disorders", "old .txt", NA, NA)
}

ms_old <- read.table(file = "Data/msGenes.txt", col.names="Gene")
for (i in 1:nrow(ms_old)) {
  genes[nrow(genes) + 1,] = c(ms_old$Gene[i], "Multiple Sclerosis", "old .txt", NA, NA)
}

parkinson_blueprint <- read_tsv(file = "Data/new/BlueprintGenetics/parkinson.txt", show_col_types = FALSE)
for (i in 1:nrow(parkinson_blueprint)) {
  genes[nrow(genes) + 1,] = c(parkinson_blueprint$Gene[i], "Parkinson Disease", "Blueprint Genetics", NA, NA)
}

parkinson_invitae <- read.table(file = "Data/new/Invitae/parkinson_disease.txt", col.names = "Gene")
for (i in 1:nrow(parkinson_invitae)) {
  genes[nrow(genes) + 1,] = c(parkinson_invitae$Gene[i], "Parkinson Disease", "Invitae", NA, NA)
}

parkinson_panelapp <- read_tsv(file = "Data/new/PanelApp_Aus/Early-onset_Parkinson_disease.tsv", show_col_types = FALSE)
for (i in 1:nrow(parkinson_panelapp)) {
  
  evidence_level <- NA
  if (grepl("Expert Review Green", parkinson_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "strong"
  } else if (grepl("Expert Review Amber", parkinson_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "moderate"
  } else if (grepl("Expert Review Red", parkinson_panelapp$`Sources(; separated)`[i])) {
    evidence_level <- "limited"
  }
  
  genes[nrow(genes) + 1,] = c(parkinson_panelapp$`Gene Symbol`[i], "Parkinson Disease", "PanelApp Australia - Early-onset", NA, evidence_level)
}

parkinson_genomicseng <- read_tsv(file = "Data/new/PanelApp_Eng/Parkinson_Disease_and_Complex_Parkinsonism.tsv", show_col_types = FALSE) 
for (i in 1:nrow(parkinson_genomicseng)) {
  
  evidence_level <- NA
  if (grepl("Expert Review Green", parkinson_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "strong"
  } else if (grepl("Expert Review Amber", parkinson_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "moderate"
  } else if (grepl("Expert Review Red", parkinson_genomicseng$`Sources(; separated)`[i])) {
    evidence_level <- "limited"
  }
  
  genes[nrow(genes) + 1,] = c(parkinson_genomicseng$`Gene Symbol`[i], "Parkinson Disease", "Genomics England - PD + Complex Parkinsonism", NA, evidence_level)
}

parkinson_old <- read.table(file = "Data/parkinsonGenes.txt",col.names = "Gene")
for (i in 1:nrow(parkinson_old)) {
  genes[nrow(genes) + 1,] = c(parkinson_old$Gene[i], "Parkinson Disease", "old .txt", NA, NA)
}


genes$panel_name = paste(genes$disease, " (", genes$panel, ")", sep='')


#get rid of duplicates
genes <- genes %>% distinct(symbol, panel_name, .keep_all = TRUE)

write.csv(genes, file = "startup1.csv", quote = FALSE, row.names = FALSE)
