shinyUI(
  
  fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("darkly"),
  
  sidebarLayout(      
    #######################################
    ###### SIDEBAR PANEL BEGINS HERE  #####
    #######################################
    sidebarPanel(
  
  h5(
    em(
      "Enlarge window for a better visualisation of the variables"
    )
  ),
  
  h5(
    em(
      "Click on the window and hover over an input box to display more information about the variable. It might be necessary to call first: 'library(shinyBS)' for the information to be displayed"
    )
  ),
  
  h5(
    em(
      "Title of input box is the variable's name as used in documentation, tutorials and code of simulations"
    )
  ),
  
  h3(strong("General variables")),
  
  fluidRow(
    
    column(
      3,
      textInput(
        "chromosome_name", 
        tags$div(tags$i(HTML("chromosome_name<br/>")),
                 "Chromosome name from where to extract location, alllele frequency, recombination map and targets of selection (if provided)"),
        value = "2L"
      ),    
      shinyBS::bsTooltip(id = "chromosome_name",
                         title = "Information pending")
    )
  ),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "chunk_number",  
        tags$div(tags$i(HTML("chunk_number<br/>")),
                 "Number of chromosome chunks"
        ),
        value = 10,
        min = 0
      ), 
      shinyBS::bsTooltip(id = "chunk_number",
                         title = "Information pending")
    ),
    
    column(
      3,
      radioButtons(
        "real_loc",
        tags$div(tags$i(HTML("real_loc<br/>")),
                 "Extract location of neutral loci from genlight object"
        ) ,
        choices = list("TRUE" = TRUE,
                       "FALSE" = FALSE),
        selected = FALSE
      ),    
      shinyBS::bsTooltip(id = "real_loc",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "neutral_loci_chunk",  
        tags$div(tags$i(HTML("neutral_loci_chunk<br/>")),
                 "Number of neutral loci per chromosome chunk"
        ),
        value = 1,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "neutral_loci_chunk",
                         title = "Information pending")
    )
  ),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "loci_under_selection", 
        tags$div(tags$i(HTML("loci_under_selection<br/>")),
                 "Total number of loci under selection"
        ),
        value = 100,
        min = 0
      ),     
      shinyBS::bsTooltip(id = "loci_under_selection",
                         title = "Information pending")
    ),
    
    
    column(
      3,
      numericInput(
        "targets_factor",
        tags$div(tags$i(HTML("targets_factor<br/>")),
                 "Percentage of the number of loci under selection from the input file 'targets_of_selection.csv' to use"),
        value = 5,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "targets_factor",
                         title = "Information pending")
    )
    
  ),
  
  hr(),
  
  h3(strong("Alelle frequency variables")),
  
  fluidRow(
    
    column(
      3,
      radioButtons(
        "q_distribution",   
        tags$div(tags$i(HTML("q_distribution<br/>")),
                 "How the initial allele frequency of the deleterious allele (q) should be determined"),
        choices = list("All equal" = "equal", "From equation" = "equation"),
        selected = "equal"
      ),    
      shinyBS::bsTooltip(id = "q_distribution",
                         title = "Information pending")
    ),
    
    column(
      3,
      sliderInput(
        "q_gral", 
        tags$div(tags$i(HTML("q_gral<br/>")),
                 "Initial frequencies of all deleterious alleles"),
        value = 0.15,
        min = 0,
        max = 1
      ),    
      shinyBS::bsTooltip(id = "q_gral",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "mutation_rate", 
        tags$div(tags$i(HTML("mutation_rate<br/>")),
                 "Mutation rate per generation per site. Value only used in the equation to determine q"),
        value = 5 * 10 ^ -5
      ),    
      shinyBS::bsTooltip(id = "mutation_rate",
                         title = "Information pending")
    )
  ),
  
  fluidRow(
    
    column(
      3,
      radioButtons(
        "real_freq", 
        tags$div(tags$i(HTML("real_freq<br/>")),
                 "Extract allele frequencies of neutral loci from genlight object"),
        choices = list("TRUE" = TRUE,
                       "FALSE" = FALSE),
        selected = FALSE
      ),    
      shinyBS::bsTooltip(id = "real_freq",
                         title = "Information pending")
    ),
    
    column(
      3,
      sliderInput(
        "q_neutral", 
        tags$div(tags$i(HTML("q_neutral<br/>")),
                 "Initial frequencies of all neutral alleles"),
        value = 0.5,
        min = 0,
        max = 1
      ),    
      shinyBS::bsTooltip(id = "q_neutral",
                         title = "Information pending")
    )
  ),
  
  hr(),
  
  h3(strong("Recombination variables")),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "chunk_recombination",
        tags$div(tags$i(HTML("chunk_recombination<br/>")),
                 "Recombination rate (cM) per chromosome chunk"),
        value = 1,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "chunk_recombination",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "chunk_bp",
        tags$div(tags$i(HTML("chunk_bp<br/>")),
                 "Length of each chromosome chunk (bp) or the resolution of the recombination map (if provided)"),
        value = 100000,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "chunk_bp",
                         title = "Information pending")
    )
  ),
  
  hr(),
  
  h3(strong("Selection variables")),
  
  h4("Selection coefficient variables"),
  
  fluidRow(
    
    column(
      3,
      radioButtons(
        "s_distribution",
        tags$div(tags$i(HTML("s_distribution<br/>")),
                 "Distribution to sample selection coefficients"),
        choices = list(
          "All equal" = "equal",
          "Gamma distribution" = "gamma",
          "Log normal distribution" = "log_normal"
        ),
        selected = "equal"
      ),    
      shinyBS::bsTooltip(id = "s_distribution",
                         title = "Information pending")
    )
  ),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "s_gral",
        tags$div(tags$i(HTML("s_gral<br/>")),
                 "Selection coefficient for all loci under selection"),
        value = 0.001,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "s_gral",
                         title = "Information pending")
    )
    
  ),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "exp_rate",
        tags$div(tags$i(HTML("exp_rate<br/>")),
                 "Mean of the exponential distribution of advantageous alleles"),
        value = 16,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "exp_rate",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "percent_adv",
        tags$div(tags$i(HTML("percent_adv<br/>")),
                 "Percentage of beneficial mutations "),
        value = 2,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "percent_adv",
                         title = "Information pending")
    )
    
  ),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "gamma_scale",
        tags$div(tags$i(HTML("gamma_scale<br/>")),
                 "Scale of gamma distribution"),
        value = 0.03,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "gamma_scale",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "gamma_shape",
        tags$div(tags$i(HTML("gamma_shape<br/>")),
                 "Shape of gamma distribution"),
        value = 0.25,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "gamma_shape",
                         title = "Information pending")
    )
    
  ),
  
  fluidRow(
    
    column(
      3,
      numericInput(
        "log_mean",
        tags$div(tags$i(HTML("log_mean<br/>")),
                 "Mean of log normal distribution"),
        value = 0.002,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "log_mean",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "log_sd",
        tags$div(tags$i(HTML("log_sd<br/>")),
                 "Standard deviation of log normal distribution"),
        value = 4,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "log_sd",
                         title = "Information pending")
    )
    
  ),
  
  hr(),
  
  h4("Dominance coefficient variables"),
  
  fluidRow(
    
    column(
      3,
      radioButtons(
        "h_distribution",
        tags$div(tags$i(HTML("h_distribution<br/>")),
                 "Distribution to sample dominance coefficient"),
        choices = list(
          "All equal" = "equal",
          "Normal distribution" = "normal",
          "From equation" = "equation"
        ),
        selected = "equal"
      ),    
      shinyBS::bsTooltip(id = "h_distribution",
                         title = "Information pending")
    )
    
  ),
  
  fluidRow(
    
    column(
      3,
      sliderInput(
        "h_gral",
        tags$div(tags$i(HTML("h_gral<br/>")),
                 "Dominance coefficient for all loci under selection"),
        value = 0.25,
        min = 0,
        max = 1
      ),    
      shinyBS::bsTooltip(id = "h_gral",
                         title = "Information pending")
    )
    
  ),
  
  fluidRow(
    
    column(
      3,
      sliderInput(
        "dominance_mean",
        tags$div(tags$i(HTML("dominance_mean<br/>")),
                 "Mean of normal distribution"),
        value = 0.25,
        min = 0,
        max = 1
      ),    
      shinyBS::bsTooltip(id = "dominance_mean",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "dominance_sd",
        tags$div(tags$i(HTML("dominance_sd<br/>")),
                 "Standard deviation of normal distribution"),
        value = sqrt(0.001),
        min = 0
      ),    
      shinyBS::bsTooltip(id = "dominance_sd",
                         title = "Information pending")
    )
    
  ),
  
  fluidRow(
    
    column(
      3,
      sliderInput(
        "intercept",
        tags$div(tags$i(HTML("intercept<br/>")),
                 "Value for the intercept of the equation"),
        value = 0.5,
        min = 0,
        max = 1
      ),    
      shinyBS::bsTooltip(id = "intercept",
                         title = "Information pending")
    ),
    
    column(
      3,
      numericInput(
        "rate",
        tags$div(tags$i(HTML("rate<br/>")),
                 "Value for the variable rate of the equation"),
        value = 500,
        min = 0
      ),    
      shinyBS::bsTooltip(id = "rate",
                         title = "Information pending")
    )
    
  ),
  
  hr(),
  
  fluidRow(
    
    column(
      3,
      radioButtons(
        "mutation", 
        tags$div(tags$i(HTML("mutation<br/>")),
                 "Simulate mutation"),
        choices = list("TRUE" = TRUE,
                       "FALSE" = FALSE),
        selected = FALSE
      ),    
      shinyBS::bsTooltip(id = "mutation",
                         title = "Information pending")
    )  
    
  ),
  
  hr(),
  
  fluidRow(column(
    12,
    actionButton(
      "close",
      label = h4(strong("RUN")),
      icon = icon("play"),
      width = "100%",
      class = "btn-success"
    )
  )),
  
  hr()
  
    ), # End ALL Sidepanels here
  
  #######################################
  ######   MAIN PANEL BEGINS HERE   #####
  #######################################
  mainPanel(
    
    imageOutput("plot1")
    # source('./ext/disaggregated-table-modal.R')$value,  # Modal for dowloading the disaggregated indicators CSV file
    # source('./ext/disaggregated-plot-modal.R')$value,  # Modal for dowloading the disaggregated indicators plot
    # source('./ext/summary-table-modal.R')$value,  # Modal for downloading the summary inequalities CSV file,
    # source('./ext/summary-plot-modal.R')$value,  # Modal for dowloading the summary inequalities plot
    # source('./ext/comparison-plot1-modal.R')$value,  # Modal for downloading comparison plot 1
    # source('./ext/comparison-plot2-modal.R')$value,  # Modal for downloading comparison plot 2
    # 
    
    # tabsetPanel(id = "mainPanel",
    #             # source('./ext/select-database-panel.R')$value,  # Select the database
    #             # source('./ext/explore-inequality-panel.R')$value,  # Explore inequalities (and subtabs)
    #             # source('./ext/compare-inequality-panel.R')$value,  # Compare with benchmark countries (and subtabs)
    #             # source('./ext/information-panel.R')$value  # Information panel, with About, Glossary, Administration subtabs                
    # )  # End tabPanel in mainPanel
    , width=8)  # End mainPanel    
  )  # End sidebarLayout 
  )  # End fluidPage 
)  # End shinyUi
