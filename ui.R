
# Use Cairo for (allegedly) improved graph quality
library(Cairo)
options(shiny.usecairo=T)

# Display a loading spinner when plots are rendering
library(shinycssloaders)
options(spinner.color="#4D00C7", spinner.color.background="#ffffff", spinner.size=2)

# Shows a full-page busy indicator when app initializes
library(shinybusy)

versionDate <- "09/02/2024" #format(as.Date(Sys.Date(), format="%Y-%m-%d"), "%d/%m/%Y")
workingName <- "Gene Panel Explorer"



fluidPage(
  # message handler
  #tags$head(tags$script(src="message-handler.js")),
  
  # Garvan favicon
  tags$head(tags$link(rel="shortcut icon", href="https://images.contentstack.io/v3/assets/blt324fd0a04af716e6/bltb9bd048ad3178ab1/64104d835fc9b810bec23256/favicon.png")),
  
  # Shows a full-page busy indicator when app initializes
  busy_start_up(loader = spin_kit(spin = "cube-grid",
                                  color = "#4D00C7",
                                  style = "width:50px; height:50px;"),
                text = HTML("<b>Application Loading...</b>"),
                mode="auto"),
  
  # Application title
  titlePanel(paste0(workingName," (version date: ", versionDate,")")),
  
  # css to disable input
  tags$head(
    tags$style(HTML(
        ".prevent_click{
        position:fixed;
        z-index:9;
        width:100%;
        height:100vh;
        background-color: transparent;
        }"
    )
    )
  ),
  
  # removing "search" from dataTables
  tagList(
    singleton(
      tags$head(
        tags$style(type="text/css", "tfoot {display:none;}")
      )
    )
  ),
  
  
  # The main panel is divided by Tabs
  mainPanel(width = 12,
            tabsetPanel(id = "mainTabs",
                        
                        #####################
                        #                   #
                        #  PANEL INPUT TAB  #
                        #                   #
                        #####################
                        
                        tabPanel("Panel Input",
                                 
                                 tags$br(),
                                 fluidRow(
                                   column(2, actionButton("showPanelInputHelp", "Show/Hide Help", width="150px"))
                                   
                                 ),
                                 conditionalPanel(condition = 'input.showPanelInputHelp % 2 == 1',
                                                  h5("1. Select any combination of diseases/phenotypes to display the associated preset gene panels. 
                                    You can select any combination of preset panels to take the union of the genes in those panels as input. 
                                    You can also enter a custom set of genes as input in the \"Custom Input\" text area below."),
                                                  h5("2. Filter your input and/or the displayed panels by clicking \"Show/Hide Filters\" and making selections."),
                                                  h5("3. Go to other tabs to visualise the input ('View Input') or run analytics on the input ('STRING', 'GTEx', 'HPO')."),
                                                  h5("You can create/edit/delete the preset panels with the buttons below. Changes are automatically saved for future sessions.")
                                                  ),
                                 tags$hr(),
                                 fluidRow(
                                   column(2, actionButton("showFilters", "Show/Hide Filters", width="150px")),
                                   column(2, actionButton("addPanel", "Create Gene Panel", width="150px")),
                                   column(2, actionButton("editPanel", "Edit Gene Panel", width="150px")),
                                   column(2, actionButton("removePanel", "Delete Gene Panel", width="150px"))
                                 ),
                                 
                                 conditionalPanel(
                                   #clicking an actionButton increments its value by 1
                                   #the modulo operator allows binary on/off functionality
                                   condition = 'input.showFilters % 2 == 1', 
                                   tags$hr(),
                                   fluidRow(
                                     column(1,
                                            radioButtons(inputId = "useOMIM", 
                                                         label = "OMIM Genes Only:",
                                                         choices = c("Yes","No"),
                                                         selected = "Yes")),
                                     column(1,
                                            selectizeInput(inputId = "excludeGenes",
                                                           label = "Exclude Genes:",
                                                           choices = NULL,
                                                           selected = NULL,
                                                           multiple = TRUE)),
                                     column(1, 
                                            checkboxGroupInput(inputId = "filterEL",
                                                               label = "Evidence Level:",
                                                               #render choices by server: unique(genesRV()$evidence_level)
                                                               choices = c("definitive", "strong",
                                                                           "moderate", "limited",
                                                                           "NA"),
                                                               selected = NULL)),
                                     column(2,
                                            dateRangeInput("dateRange", label="Panel Date:",
                                                           start = "2023-12-01")),
                                   )
                                 ),
                                 
                                 tags$hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          checkboxGroupInput(inputId = "disease", 
                                                             label = "Diseases/Phenotypes:",
                                                             choices = NULL),
                                          textAreaInput(inputId = "addGenes",
                                                        label = "Custom Input:",
                                                        placeholder = "Enter genes (space-separated)"
                                          ),
                                          textAreaInput(inputId = "panelAreaInput", 
                                                        label = "Preview Current Selected Genes:", 
                                                        value = "", 
                                                        resize = "vertical",
                                                        placeholder = "No genes selected"),
                                          textOutput(outputId = "countInput")
                                   ),
                                   conditionalPanel(condition = 'input.disease.length > 0',
                                                    column(3,
                                                           checkboxGroupInput(inputId = "selectedPanels",
                                                                              label = "Commercial Panels:",
                                                                              choices = NULL))),
                                   conditionalPanel(condition = 'input.disease.length > 0',
                                                    column(3,
                                                           checkboxGroupInput(inputId = "selectedPanelsCurated",
                                                                              label = "Curated Panels:",
                                                                              choices = NULL))),
                                   conditionalPanel(condition = 'input.disease.length > 0',
                                                    column(3,
                                                           checkboxGroupInput(inputId = "selectedPanelsExperimental",
                                                                              label = "Experimental Panels:",
                                                                              choices = NULL))),
                                 ),
                                 
                                 tags$br()
                                 
                        ),                    
                        
                        ######################
                        #                    #
                        #   VIEW INPUT TAB   #
                        #                    #
                        ######################
                        
                        tabPanel("View Input",
                                 
                                 tags$br(),
                                 fluidRow(
                                   column(2, actionButton("showViewInputHelp", "Show/Hide Help", width="150px"))
                                 ),
                                 conditionalPanel(condition = 'input.showViewInputHelp % 2 == 1',
                                                  h5("1. Select any combination of plots to view them.
                                                     The UpSet plot (requires 2+ input panels) and Venn diagram (requires 2-4 input panels) shows the overlap between input panels. 
                                                     The bar plot (requires 1+ panels) shows the number of genes versus number of input lists."),
                                                  h5("2. Sort the table to identify trends (e.g. sort '# input lists' decreasing to find which genes are most common across the selected input panels)."),
                                                  h5("You can download each of the plots and/or table with the download buttons."),
                                                  h5("DisGeNET evidence level values can be (in decreasing order) 'definitive', 'strong', 'moderate', 'limited' or 'NA'. See ", tags$a(href="https://www.disgenet.org/dbinfo#score:~:text=Evidence%20Level,as%20limited",
                                                            target="_blank",
                                                            "here"), " for how they're calculated."),
                                                  h5("PanelApp Australia / Genomics England evidence level values can be (in decreasing order) 'strong', 'moderate', 'limited' or 'NA'. See", tags$a(href="https://panelapp.genomicsengland.co.uk/#!Guidelines",
                                                            target="_blank",
                                                            "here"), " for how they're calculated.")
                                 ),
                                 tags$hr(),
                                 fluidRow(
                                   column(4, checkboxGroupInput(inputId = "inputPlots",
                                                                label = "View Plots:",
                                                                choices = c("UpSet Plot",
                                                                            "Venn Diagram",
                                                                            "Bar Plot")))
                                 ),
                                 
                                 tags$hr(),
                                 
                                 fluidRow(
                                   conditionalPanel(condition = 'input.inputPlots.includes("UpSet Plot")',
                                                    column(6,
                                                           h3("UpSet Plot"),
                                                           conditionalPanel(condition = 'input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length < 2',
                                                                            span(h5("Please select at least 2 panels."), style="color:red")),
                                                           conditionalPanel(condition = 'input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length >= 2',
                                                                            downloadButton("downloadUpset", "Download .png"),
                                                                            withSpinner(plotOutput("upset"),
                                                                                        type = 2))
                                                    )
                                   ),
                                   conditionalPanel(condition = 'input.inputPlots.includes("Venn Diagram")',
                                                    column(6,
                                                           h3("Venn Diagram"),
                                                           conditionalPanel(condition = 'input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length < 2 ||
                                                                        input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length > 4',
                                                                            span(h5("Please select 2-4 panels."), style="color:red")),
                                                           conditionalPanel(condition = 'input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length >= 2 &&
                                                                        input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length <= 4',
                                                                            downloadButton("downloadVenn", "Download .png"),
                                                                            withSpinner(plotOutput("venn"),
                                                                                        type = 2))
                                                    )
                                   ),
                                   conditionalPanel(condition = 'input.inputPlots.includes("Bar Plot")',
                                                    column(6,
                                                           h3("Bar Plot"),
                                                           conditionalPanel(condition = 'input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length == 0',
                                                                            span(h5("You haven't inputted any genes yet."), style="color:red")),
                                                           conditionalPanel(condition  = 'input.selectedPanels.length + (input.addGenes.length > 0) + input.selectedPanelsCurated.length + input.selectedPanelsExperimental.length > 0',
                                                                            downloadButton("downloadBar", "Download .png"),
                                                                            withSpinner(plotOutput("bar"),
                                                                                        type = 2))
                                                    ))
                                 ),
                                 
                                 fluidRow(
                                   column(4, 
                                          h3("Input Table"),
                                          downloadButton("downloadGeneTable", "Download .csv"),
                                   )
                                 )
                                 ,
                                 tags$br(),
                                 
                                 dataTableOutput(outputId = "geneTable")
                        ),
                        
                        ################
                        #              #
                        #  STRING TAB  #
                        #              #
                        ################
                        
                        tabPanel("STRING",
                                 tags$br(),
                                 fluidRow(
                                   column(6,
                                          tags$img(height=50, src="string.png"),
                                          tags$h4("Protein-protein interaction partners.")
                                          )
                                 ),
                                 fluidRow(
                                   column(2, actionButton("showSTRINGHelp", "Show/Hide Help", width="150px")),
                                   column(8, ),
                                   #column(2, actionButton("updateSTRING", "Update STRING Data"), h5("Last updated: x/y/z"))
                                 ),
                                 conditionalPanel(condition = 'input.showSTRINGHelp % 2 == 1',
                                                  h5("1. Compute the results for the input genes by clicking the button below (computes all STRING results - may be slow). You will need to manually recompute when you change the input genes."),
                                                  h5("2. Filter the results (plots/table are automatically re-rendered with selections applied)."),
                                                  h5(tags$a(href="http://version10.string-db.org/help/faq/#how-are-the-scores-computed",
                                                            target="_blank",
                                                            "How are STRING protein-protein interaction scores calculated?")),
                                                  h5(tags$a(href="https://string-db.org/cgi/info?sessionId=bWfA0C3yBWIJ&footer_active_subpage=scores",
                                                            target="_blank",
                                                            "What do STRING protein-protein interaction scores represent?")),
                                                  h5(tags$a(href="http://version10.string-db.org/help/faq/#:~:text=escore%20%2D%20experimental%20score,names%20in%20abstracts).",
                                                            target="_blank",
                                                            "What do the STRING experimental, database, textmining scores represent?"))
                                                  
                                 ),
                                 tags$hr(),
                                 fluidRow(
                                   column(2, actionButton("showSTRINGFilters", "Show/Hide Filters", width="150px")),
                                   column(8, actionButton("renderSTRING", "Compute Results", width="150px")),

                                   column(2, downloadButton("downloadSTRING", "Download Table")),
                                 ),
                                 
                                 conditionalPanel(
                                   #clicking an actionButton increments its value by 1
                                   #the modulo operator allows binary on/off functionality
                                   condition = 'input.showSTRINGFilters % 2 == 1', 
                                   tags$hr(),
                                   fluidRow(
                                     column(1,
                                            checkboxGroupInput(inputId = "wasInputSTRING",
                                                               label = "wasInput:",
                                                               choices = c("Yes", "No"),
                                                               selected = NULL)
                                            ),
                                     column(1,
                                            radioButtons(inputId = "allSTRINGScores",
                                                               label = "Display subscores:",
                                                               choices = c("Yes", "No"),
                                                               selected = "No")
                                     ),
                                     column(2,
                                            conditionalPanel(
                                              #if user presses 'compute results', they can't interact with conf threshold slider
                                              condition = "$(\'html\').hasClass(\'shiny-busy\')", 
                                              tags$div(class = "prevent_click")
                                            ),
                                            sliderInput("stringConf", "combined_score cut-off:",
                                                        min = 0, max = 999,value = 400),
                                            numericInput("stringConfExact", min = 0, max = 999,
                                                         value=400, label="")),
                                   )
                                 ),
                                 
                                 tags$hr(),
                                 span(textOutput("STRINGerrormsg"), style="color:red"),
                                 fluidRow(
                                   # Show plots of the stringDB results on the left
                                   column(6,
                                          withSpinner(plotOutput('stringDBhistogram'),
                                                      type = 2),
                                          withSpinner(plotOutput("stringDBplot"),
                                                      type = 2),
                                          textAreaInput(inputId = "stringTextOutput", 
                                                        label = "Result Genes",
                                                        resize = "vertical"),
                                          textOutput(outputId = "countString"),
                                          tags$br(),
                                          actionButton("addSTRINGResultsToInput",
                                                       "Append Results to Custom Input")
                                          ),
                                   # Table of StringDB results on the right
                                   column(6,
                                          withSpinner(dataTableOutput('sdbtable'),
                                                      type = 2)
                                          )
                                   ),
                                 tags$br(),
                                 tags$br()
                                 
                                 
                        ),  
                        
                        ##############
                        #            #
                        #  GTEX TAB  #
                        #            #
                        ##############
                        
                        tabPanel("GTEx", 
                                 tags$br(),
                                 tags$img(height=50, src="gtex.png"),
                                 fluidRow(
                                   column(8,tags$h4("Tissue co-expression."))
                                 ),
                                 fluidRow(
                                   column(2, actionButton("showGTExGHelp", "Show/Hide Help", width="150px")),
                                   column(3, actionButton("showIndividualGTEx", "Show/Hide Individual GTEx"))
                                 ),
                                 conditionalPanel(condition = 'input.showGTExGHelp % 2 == 1',
                                                  h5("1. Divide the input genes into a custom number of subgroups (or an automatic number calculated using the gap statistic) based on their coexpression levels (Compute Clusters)."),
                                                  h5("2. View the resulting clusters plotted over a heatmap, dendrogram and table. You can also view the gap statistic and re-compute the subgroups with an adjusted number of clusters if desired."),
                                                  h5("3. Choose your desired cluster and filter options, then run the analysis (Compute GTEx Results)."),
                                                  h5("4. View the results in a table (can also view the intra-cluster interactions with a heatmap/radar plot). Append the top result genes to the progressive result gene list with the button at the bottom of the page and re-run the analysis with a new cluster."),
                                                  tags$br(),
                                                  h5(tags$a(href="https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/#gap-statistic-method",
                                                            target="_blank",
                                                            "What is the gap statistic?")),
                                                  h5("Essentially, we want to choose k clusters such that the gap statistic is maximised."),
                                                  h5("The chosen k may not actually maximise the gap statistic (the algorithm used chooses a local rather than a global maximum).")
                                 ),
                                 tags$hr(),
                                 selectizeInput("gtexTissue", "GTEx Tissue Selection:",
                                                choices = c("Brain","Immune System", "All", "Advanced"),
                                                selected = "Brain"),
                                 actionButton("showAdvTissue", "Show/Hide Adv. Tissue Selection"),
                                 conditionalPanel(condition = 'input.showAdvTissue % 2 == 1',
                                                  tags$br(),
                                                  fluidRow(
                                                      checkboxGroupInput("advGTExTissue", "Adv. GTEx Tissue Selection:",
                                                                         choices = c(),
                                                                         #inline = TRUE
                                                                         #uncomment above if you want checkbox to be displayed horizontally - not well spaced though
                                                                         )
                                                  )
                                 ),
                                 tags$hr(),
                                 conditionalPanel(condition = 'input.showIndividualGTEx % 2 == 1',
                                                  h4(HTML("<b>View Individual Gene-Tissue Expression Levels</b>")),
                                                  textAreaInput("individualGTExGenes",
                                                                "Enter genes:",
                                                                placeholder = "Enter genes (space-separated)",
                                                                value = ""
                                                                ),
                                                  downloadButton("individualGTExDownload", "Download Table"),
                                                  tags$br(),
                                                  tags$br(),
                                                  dataTableOutput("individualGTExTable"),
                                                  tags$hr(),
                                                  ),
                                 h4(HTML("<b>Divide Input Into Subgroups</b>")),
                                 fluidRow(
                                   column(6,
                                          numericInput("numClusters", "Number of Clusters:",
                                                       value = 0, min = 0, step=1),
                                          h5("The number of clusters is calculated using the gap statistic if left equal to 0."),
                                          ),
                                   column(6,
                                          checkboxGroupInput("gtexClusterPlots",
                                                             "View Clusters:",
                                                             choices=c("Gap Statistic", 
                                                                       "Heatmap",
                                                                       "Dendrogram",
                                                                       "Table"),
                                                             selected=c("Dendrogram"))
                                          )
                                 ),
                                 tags$br(),
                                 fluidRow(
                                   column(6,
                                          actionButton(inputId = "gtexClusterStart", label = "Compute Clusters")
                                          ),
                                   column(2,
                                          downloadButton("downloadGTEXClusters", "Download Table")
                                          )
                                 ),
                                 
                                 tags$hr(),
                                 conditionalPanel(condition = 'input.gtexClusterPlots.includes("Gap Statistic")',
                                                  plotOutput("gtexGapStatistic", width = "100%", height = "auto")
                                 ),
                                 conditionalPanel(condition = 'input.gtexClusterPlots.includes("Heatmap")',
                                                  plotOutput("gtexHeatmap", width = "100%", height = "auto")
                                                  ),
                                 conditionalPanel(condition = 'input.gtexClusterPlots.includes("Dendrogram")',
                                                  plotOutput("gtexDendrogram"),
                                 ),
                                 conditionalPanel(condition = 'input.gtexClusterPlots.includes("Table")',
                                                  dataTableOutput("gtexClusters")
                                 ),
                                 tags$hr(),
                                 h4(HTML("<b>Run Analysis</b>")),
                                 fluidRow(
                                   column(3,
                                          radioButtons("chooseCluster", "Choose Cluster:",
                                                       choices = c("All"), selected = "All"),
                                          textAreaInput("gtexClusterPreview", "Cluster Genes (preview):",
                                                        value = "", resize="vertical"),
                                          textOutput(outputId = "clusterGeneCount")
                                   ),
                                   column(3,
                                          numericInput("gtexAvgMin", "Minimum Avg:", max=1, min=-1, value=-1),
                                          numericInput("gtexAvgMax", "Maximum Avg:", max=1, min=-1, value=1),
                                          radioButtons("gtexResultSort", "Sort by Avg:",
                                                       choices = c("Decreasing", "Increasing"),
                                                       selected = "Decreasing"),
                                          numericInput("gtexNumResultGenes", "Restrict # Result Genes (Upper Limit)",
                                                       value = 100, step = 1, min = 0)
                                          
                                   ),
                                   column(3,
                                          checkboxGroupInput("gtexResultPlots",
                                                             "View Results:",
                                                             choices=c("Heatmap",
                                                                       "Radar",
                                                                       "Table"),
                                                             selected=c("Table"))
                                          )
                                 ),
                                 tags$br(),
                                 fluidRow(
                                   column(6,
                                          actionButton(inputId = "gtexStart", label = "Compute GTEx Results")
                                          ),
                                   column(6,
                                          column(2,downloadButton("downloadGTEx", "Download Results Table"))
                                          )
                                 ),
                                 tags$hr(),
                                 conditionalPanel(condition = 'input.gtexResultPlots.includes("Heatmap")',
                                                  column(6,
                                                         plotOutput("gtexClusterHeatmap", width = "100%", height = "auto")
                                                         )
                                 ),
                                 conditionalPanel(condition = 'input.gtexResultPlots.includes("Radar")',
                                                  column(6,
                                                         plotOutput("gtexRadar", width = "100%", height = "auto")
                                                         )
                                 ),
                                 conditionalPanel(condition = 'input.gtexResultPlots.includes("Heatmap") | input.gtexResultPlots.includes("Radar")',
                                                  tags$hr()
                                                  ),
                                 conditionalPanel(condition = 'input.gtexResultPlots.includes("Table")',
                                                  dataTableOutput('gtextable')
                                 ),
                                 tags$hr(),
                                 fluidRow(
                                   column(6,
                                          textAreaInput(inputId = "gtexTextOutput", 
                                                        label = "Result Genes",
                                                        resize = "vertical"),
                                          textOutput(outputId = "gtexResultCount"),
                                          tags$br(),
                                          actionButton("gtexAppendToProgressive",
                                                       "Append to Progressive Result Genes")
                                          ),
                                   column(6,
                                          textAreaInput(inputId = "gtexProgressiveTextOutput", 
                                                        label = "Progressive Result Genes",
                                                        resize = "vertical"),
                                          textOutput(outputId = "gtexProgressiveResultCount"),
                                          tags$br(),
                                          actionButton("gtexClearProgressive",
                                                       "Clear Progressive Results")
                                          )
                                 ),
                                 tags$br()
                                 
                                 
                                 
                        ),
                        
                        
                        #####################
                        #                   #
                        #  COMBINATION TAB  #
                        #                   #
                        #####################
                        
                        tabPanel("Combination",
                                 tags$br(),
                                 h4("Combination of STRING-DB and GTEx:"),
                                 h5("1. Compute results for STRING."),
                                 h5("2. Compute results for GTEx and append to progressive results."),
                                 h5("3. 'Create Table' to combine STRING results and GTEx progressive results."),
                                 h5("Ranking: Calculated by (GTEx correlation value + max_STRING_score/1000) by default."),
                                 tags$br(),
                                 fluidRow(
                                   column(3,
                                          numericInput("comboMaxSTRINGFilter", "Minimum max_STRING_score:",
                                                       value=0, min=0, max=999, step=1),
                                          numericInput("comboMinAvgFilter", "Minimum Avg:",
                                                       value = -1, max = 1, min = -1),
                                          numericInput("comboMaxAvgFilter", "Maximum Avg:",
                                                       value = 1, max = 1, min = -1)
                                          ),
                                   column(3,
                                          radioButtons("comboNAvalues",
                                                       "Include NA Values?",
                                                       choices = c("Yes", "No"),
                                                       selected = "Yes"),
                                          radioButtons("comboRankColumn",
                                                       "Include Rank Column?",
                                                       choices = c("Yes", "No"),
                                                       selected = "Yes"),
                                          sliderInput(inputId = 'comboRankWeighting',
                                                      label = 'STRING Rank Weighting:',
                                                      min = 0,
                                                      max = 1,
                                                      value = 0.5,
                                                      step = 0.05)
                                          ),
                                   column(6,
                                          textAreaInput("comboResultGenes", "Result Genes")
                                          )
                                 ),
                                 fluidRow(
                                   column(6,
                                          actionButton("comboStart", "Create Table"),
                                          ),
                                   column(6,
                                          downloadButton("downloadCombo", "Download Table")
                                          )
                                 ),
                                 tags$hr(),
                                 span(textOutput("comboText"), style="color:red"),
                                 fluidRow(
                                   column(12, dataTableOutput("comboTable"))
                                 ),
                                 tags$hr(),
                                 #fluidRow(
                                 #  column(12, plotOutput("comboPlot"))
                                 #),
                                 #tags$hr(),
                                 #fluidRow(
                                 #  column(12, dataTableOutput('rankTable'))
                                 #)
                        ),
                        
                        #############
                        #           #
                        #  HPO TAB  #
                        #           #
                        #############
                        
                        tabPanel("HPO",
                                 tags$br(),
                                 tags$img(height=50, src="hpo.png"),
                                 fluidRow(
                                   column(8,
                                          tags$h4("Human Phenotype Ontology"),
                                          tags$h5("The plots will show HPO phenotypes with the highest overlap count or percentage with the input genes(s)."),
                                          tags$h5("Below are the genes belonging to those HPO phenotypes (use the dropdown list to select a HPO phenotype).")
                                   ),
                                   column(2,downloadButton("downloadHPO", "Download results"))
                                 ),
                                 tags$hr(),
                                 fluidRow(
                                   tags$h5("Note: calculation may take several minutes."),
                                   actionButton(inputId = "hpoStart", label = "Start HPO analysis"),
                                   
                                 ),
                                 tags$hr(),
                                 tags$h5("HPO terms matching your input will appear here."),
                                 tags$h5("Select clinically relevant and specific terms."),
                                 fluidRow( # check boxes to optionally remove unwanted HPO terms
                                   column(6,
                                          checkboxGroupInput(inputId = "hpoChecksCount", 
                                                             label = "Top-ranked by count"),
                                   ),
                                   column(6,
                                          checkboxGroupInput(inputId = "hpoChecksPercent", 
                                                             label = "Top-ranked by %"),
                                   )
                                 ),
                                 fluidRow(
                                   actionButton(inputId = "hpoMakePlots", label = "Display Results")
                                 ),
                                 tags$hr(),
                                 fluidRow(
                                   column(12, 
                                          plotOutput("hpoPlot", height = 1000, width = 600),
                                   )
                                 ),
                                 tags$hr(),
                                 # opt-in to show input genes in the recurring gene results
                                 radioButtons("hpoShowInput", "Show input genes?",
                                              choices = c("No","Yes"), selected = "No"),
                                 tags$hr(),
                                 fluidRow( # dropdown to select which HPO phenotype to take as results
                                   column(4, selectizeInput(inputId = "hpoResultDropdown",
                                                            choices = NULL, # will be added once results are found
                                                            label = "HPO phenotype-to-gene list:")),
                                   column(8, # Add a text output area, to copy out space-separated result genes
                                          textAreaInput(inputId = "hpoTextOutput", 
                                                        label = "Results (as text block)", 
                                                        resize = "vertical", width = "100%"),
                                          textOutput(outputId=paste0("countHpo")))
                                 ),
                                 fluidRow(
                                   textAreaInput(inputId = "hpoTextOutputRecurringGenes", 
                                                 label = "Top Recurring Genes (as text block)", 
                                                 resize = "vertical", width = "100%")
                                 )
                        ),
                        
                        
                        #######################
                        #                     #
                        #  GENE ONTOLOGY TAB  #
                        #                     #
                        #######################
                        
                        tabPanel("GO Annotations",
                                 tags$br(),
                                 fluidRow(
                                   column(2, actionButton("showGOHelp", "Show/Hide Help", width="150px"))
                                 ),
                                 conditionalPanel(condition = 'input.showGOHelp % 2 == 1',
                                                  h5(tags$a(href="https://geneontology.org/docs/go-annotations/#annotation-qualifiers",
                                                            target="_blank",
                                                            "Intro to GO annotations")),
                                                  h5(tags$a(href="https://link.springer.com/protocol/10.1007/978-1-4939-3743-1_14#:~:text=Modification%20of%20Annotation%20Meaning%20by%20Qualifiers",
                                                            target="_blank",
                                                            "Modification of annotation meaning by qualifiers"))
                                 ),
                                 tags$hr(),
                                 textAreaInput("annotationInput",
                                               "Search GO Annotations:",
                                               value = "",
                                               placeholder = "Enter genes (space-separated)"),
                                 radioButtons("annotationIncludeInput",
                                              "Include Input Genes?",
                                              choices = c("Yes", "No"),
                                              selected = "Yes"),
                                 fluidRow(
                                   column(2, actionButton("showGOFilters", "Show/Hide Filters", width="150px")),
                                   column(8, actionButton("renderGOAnnotations", "Compute Results", width="150px"))
                                 ),
                                 
                                 conditionalPanel(
                                   #clicking an actionButton increments its value by 1
                                   #the modulo operator allows binary on/off functionality
                                   condition = 'input.showGOFilters % 2 == 1', 
                                   tags$hr(),
                                   fluidRow(
                                     column(2,
                                            checkboxGroupInput("annotationAspect",
                                                               "Annotation Type:",
                                                               choices = c("Biological Process",
                                                                           "Cellular Component",
                                                                           "Molecular Function"),
                                                               selected = c("Biological Process",
                                                                            "Cellular Component",
                                                                            "Molecular Function")
                                                               
                                            )
                                     ),
                                     column(1,
                                            selectizeInput(inputId = "qualifierGO",
                                                           label = "Qualifier:",
                                                           #display all possible qualifier choices in the GO annotations dataset
                                                           choices = c("enables",
                                                                       "located_in",
                                                                       "involved_in",
                                                                       "part_of",
                                                                       "NOT|enables",
                                                                       "NOT|involved_in",
                                                                       "is_active_in",
                                                                       "NOT|colocalizes_with",
                                                                       "colocalizes_with",
                                                                       "acts_upstream_of_or_within",
                                                                       "contributes_to",
                                                                       "NOT|located_in",
                                                                       "NOT|part_of",
                                                                       "acts_upstream_of_positive_effect",
                                                                       "NOT|acts_upstream_of_or_within",
                                                                       "acts_upstream_of",
                                                                       "acts_upstream_of_negative_effect",
                                                                       "acts_upstream_of_or_within_positive_effect",
                                                                       "acts_upstream_of_or_within_negative_effect",
                                                                       "NOT|contributes_to",
                                                                       "NOT|acts_upstream_of_or_within_negative_effect",
                                                                       "NOT|is_active_in"),
                                                           selected=NULL,
                                                           multiple=TRUE)
                                     ),
                                     column(2,
                                            selectizeInput(inputId = "annotationPlaintext",
                                            label = "Plaintext:",
                                            choices = NULL,
                                            selected = NULL,
                                            multiple = TRUE)
                                     )
                                     
                                   )
                                 ),
                                 tags$hr(),
                                 withSpinner(dataTableOutput("GOannotations"),
                                             type = 2),
                                 plotOutput("mostCommonAnnotations")
                                 ),
                        
                        #####################
                        #                   #
                        #  VALIDATION TAB   #
                        #                   #
                        #####################
                        
                        tabPanel("Validation",
                                 tags$br(),
                                 radioButtons(inputId = "validationDisplay",
                                              label = "Choose method of validation:",
                                              choices = c("ClinVar gene-disease associations",
                                                          "ClinVar gene variant information",
                                                          "Check gene panels in local data"),
                                              selected = "ClinVar gene-disease associations"),
                                 conditionalPanel(condition = "input.validationDisplay.includes('ClinVar gene-disease associations')",
                                                  h4("ClinVar gene-disease associations"),
                                                  fluidRow(
                                                    column(6,
                                                           textAreaInput("validationGenes",
                                                                         "Search AssociatedGenes/RelatedGenes:",
                                                                         placeholder = "Enter genes (space-separated)"),
                                                    ),
                                                    column(6,
                                                           textAreaInput("validationDiseases",
                                                                         "Search DiseaseName:",
                                                                         placeholder = "Enter a single disease term"),
                                                    )
                                                  ),
                                                  downloadButton("clinvarTable", "Download Table"),
                                                  tags$br(),
                                                  tags$br(),
                                                  dataTableOutput("clinvarInfo")
                                 ),
                                 conditionalPanel(condition = "input.validationDisplay.includes('ClinVar gene variant information')",
                                                  h4("ClinVar gene variant information"),
                                                  fluidRow(
                                                    column(3,
                                                           textAreaInput("validationVariantGenes",
                                                                         "Search Genes:",
                                                                         placeholder = "Enter genes (space-separated)"),
                                                    ),
                                                    column(3,
                                                           checkboxGroupInput(inputId = "variantClinicalSignificance",
                                                                              label = "ClinicalSignificance:",
                                                                              choices = c("Conflicting classifications of pathogenicity",
                                                                                          "Benign",
                                                                                          "Benign/Likely benign",
                                                                                          "Likely benign",
                                                                                          "Uncertain significance",
                                                                                          "Likely pathogenic",
                                                                                          "Pathogenic/Likely pathogenic",
                                                                                          "Pathogenic"),
                                                                              selected = "Pathogenic")
                                                    ),
                                                  ),
                                                  downloadButton("variantTableDownload", "Download Table"),
                                                  tags$br(),
                                                  tags$br(),
                                                  dataTableOutput("variantTable")
                                 ),
                                 conditionalPanel(condition = "input.validationDisplay.includes('Check gene panels in local data')",
                                                  h4("Check gene panels in local data"),
                                                  fluidRow(
                                                    column(3,
                                                           textAreaInput("validationLocalGenes",
                                                                         "Search Genes:",
                                                                         placeholder = "Enter genes (space-separated)"),
                                                    ),
                                                  ),
                                                  downloadButton("validationLocalTableDownload"),
                                                  tags$br(),
                                                  tags$br(),
                                                  dataTableOutput("validationLocalTable")
                                 ),
                                 
                                 
                        )


            )
  )
)
