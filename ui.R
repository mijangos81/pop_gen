library(dartR)
library(gganimate)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(shinyWidgets)
library(data.table)
library(waiter)
library(symengine)
library(BiocManager)
# options(repos = BiocManager::repositories())
# library(gdsfmt)
# BiocManager::install("gdsfmt")
# devtools::install_github("astamm/nloptr")
# devtools::install_github("mijangos81/dartR@beta")
# rsconnect::deployApp()

shinyUI(
  fluidPage(
  autoWaiter(color="white",html = spin_whirly()),
  theme = shinythemes::shinytheme("cerulean"),

  titlePanel(
    "Population genetics"
  ),
  navbarPage(
    "Let's get started",
    
    #######################################################################
    ############### BEFORE START #########################################
    #######################################################################
    tabPanel(
      "1. Before starting",
      
      fluidRow(
        column(width = 3),
        column(width = 6, 
               h3(p(strong("Remember that..."),
                    style = "color:black;text-align:center")
               ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
      ),
      
      br(),
      
      fluidRow(
        
        column(width = 3),
        column(width = 6,
          p(
            strong("Genomes"),"consist of sequences of deoxyribonucleic acid (DNA) that encode most of an organism’s traits or",strong("phenotype."),"DNA is packaged within structures called",strong("chromosomes."),"A",strong("diploid"),"organism carries two copies of each chromosome, with one copy inherited from each of its parents.",
            style = "color:black;text-align:justify"
          ),
          
          p(
            "The location of a specific DNA sequence within the genome is called",strong("locus")," (with",strong("loci"),"being the plural), and the different variants of DNA sequences at a locus are called",strong("alleles."),"The",strong("frequency"),"of an allele is the proportion in which a given allele is present in a population. An allele can be",strong("lost"),"when it is no longer present in the population; in turn, an allele becomes",strong("fixed"),"when it is the only allele present in the population.",
            style = "color:black;text-align:justify"
          ),
          
          p(
            "A",strong("genotype"),"is the combination of alleles present at a particular locus in an individual. The genotype of an individual carrying different alleles at a particular locus is said to be",strong("heterozygote"), "at that locus, while an individual carrying the same allele is a",strong("homozygote"), "at that locus." ,
            style = "color:black;text-align:justify"
          ),
          
          style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
          
        )
      ),
      
      br(),
      
      p(em("Developed by"), br("Luis Mijangos"), style =
          "text-align:center; font-family: times")
    ),
    
    #######################################################################
    ############### BACKGROUND #########################################
    #######################################################################
    tabPanel(
      "2. Background",
      
      fluidRow(
        column(width = 3),
        column(width = 6, 
               h3(p(strong("A bit of history of population genetics..."),
                    style = "color:black;text-align:center")
               ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
      ),
      br(),

      fluidRow(
        column(width = 3),
        column(width = 6,
          
          p(strong("Recipe for a mouse"),style = "color:black;text-align:justify"),
          tags$ol(
            tags$li("Put a dirty shirt or some rags in an open pot or barrel."),
            tags$li("Add a few grains of wheat."),
            tags$li("Wait 21 days,  and the mouse will appear spontaneously.")),
          
          p(em("I am not very convinced about this recipe,"), "could have been one of",strong("Charles Darwin’s"), "thoughts about this 17th-century recipe before embarking on his trip to the Galapagos Islands. Some years later in 1859, Darwin proposed instead in his book:",strong(em("'On the origin of species by means of natural selection',")), "that all the species descend from a common ancestor, as branches of a tree join its trunk. He also proposed that species originated by natural selection, a process in which the fittest individuals will survive, reproduce and pass their traits to their offspring. At about the same time in 1866,",strong("Gregor Mendel"), "discovered that physical characteristics are inherited through discrete and independent pieces of information and that each parent provides precisely half of these pieces of information, which were called later genes.",style = "color:black;text-align:justify"),
          br(),
          div(img(src='Darwin.png',width="150",height="200"), 
              img(src='Mendel.png',width="150",height="200"), 
              style="text-align: center;"),
      br(),

          p("It is still uncertain why nobody realised that Darwin and Mendel’s discoveries had a relationship until the 1920s and 30’s when",strong("Sewall Wright, J. B. S. Haldane and Ronald Fisher"), "reconciled their work to establish the field of population genetics. Their joint work finally explained the mechanisms by which evolution gives place to adaptation and speciation. By using a mathematical framework, the three giants of population genetics developed innumerable complex models, an incredible achievement given that the molecular basis of genes (i.e. DNA) was not discovered yet.", style = "color:black;text-align:justify"),
          br(),
          
          div(img(src='Wright.png',width="150",height="200"), 
              img(src='Haldane.png',width="150",height="200"),
              img(src='Fisher.png',width="150",height="200"), 
              style="text-align: center;"),
          br(),
          
          p("Some decades later, in the 1960s, it was already clear that natural selection is the predominant force driving evolutionary change at the phenotypic level, as first proposed by Darwin. However, little was known about the kind and amount of variation at the gene level due to a lack of a suitable technique to determine it unambiguously. In line with the assertion that natural selection is the primary evolutionary force at the phenotypic level, researchers at the time considered that natural selection played an equally important role in determining variation at the gene level. Two main genetic variation hypotheses existed. The",strong("classical hypothesis"),"proposed that variation should be low because most mutations would be deleterious and eliminated rapidly by natural selection. The",strong("balance hypothesis"), "proposed that variation should be high because natural selection maintains variation by favouring heterozygote genotypes. In 1966, researchers using the first molecular techniques based on the electrophoretic mobility of proteins reported unexpectedly high genetic variation, data which supported the balance hypothesis. However, soon after in 1968,",strong("Motoo Kimura"), "proposed that most genetic variation between individuals and between species is selectively almost",strong("neutral"), "(i.e. it has little or no negative or positive effects on fitness for survival or reproduction). Thus the fate of neutral genes, whether lost or becoming fixed, is dictated by the random sampling of genes occurring from one generation to the next",strong("('genetic drift')"), "rather than by natural selection. Kimura’s",strong("neutral theory of molecular evolution"), "is now an established model in population genetics to describe and predict patterns of genetic diversity.",style = "color:black;text-align:justify"),
          br(),
          
          div(img(src='Kimura.png',width="150",height="200"), 
              style="text-align: center;"),

          style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
        )
      ),
      
      hr(),
      
      h3(p(
        em("SIMULATIONS "),
        icon("dna", lib = "font-awesome"),
        style = "color:black;text-align:center"
      )),
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}"
        )
      ),
      
      br(),
      
      p(em("Developed by"), br("Luis Mijangos"), style =
          "text-align:center; font-family: times")
    ),
    
    #######################################################################
    ############### What's population genetics ############################
    #######################################################################
    tabPanel(
      "3. Population genetics",
      
      fluidRow(
        column(width = 3),
        column(width = 6, 
               h3(p(strong("What is population genetics?"),
                    style = "color:black;text-align:center")
               ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
      ),
      br(),
      
      fluidRow(

        column(width = 3),
        column(width = 6,
               br(),
              
          p("It is a field in genetics that uses",strong("mathematical models"), "to understand how and why allele frequencies change over time due to the action of the",strong("five evolutionary forces:"),style = "color:black;text-align:justify"),
          
          h5(strong(tags$ol(
            tags$li("Mutation."),
            tags$li("Natural selection."),
            tags$li("Gene flow."),
            tags$li("Genetic drift."),
            tags$li("Recombination.")
          )),style = "color:black;text-align:justify"),
          
          br(),
          
          h4(p(strong("What are models, and why do we use them?"), style = "color:black;text-align:center")),
          
          br(),

          p("Theoretical models use mathematical principles to translate verbal hypotheses into abstract models that allow us to understand complex phenomena and predict their future outcomes. One of the aims of theoretical models is to identify the overarching elements, principles, mechanisms, and interactions that govern a particular system or process. To achieve this goal, models simplify the process or system under study by making",strong("assumptions."), "Assumptions are statements describing the conditions in which a specific model is situated. Theoretical models can be seen as ”proof-of-concept” models because they test unambiguously and precisely whether certain assumptions lead to specific predictions.", style = "color:black;text-align:justify"),

          h5(p(strong("In a nutshell, models:", style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"))),
          h5(strong(tags$ol(
            tags$li("Make us consider and define the parameters that need to be considered."),
            tags$li("Allow us to test hypotheses."),
            tags$li("Allow us to generalise results."),
            tags$li("Allow us to predict how a system will operate in the future."))),style = "color:black;text-align:justify"),

          p("The development of theoretical models has been the primary approach used in population genetics to explain and describe how genetic diversity is affected by evolutionary forces.", style = "color:black;text-align:justify"),
          
          br(),
          
          h4(p(strong("The idealised population model"), style = "color:black;text-align:center")),
          
          br(),
          
          p("The basic models in population genetics generalise and simplify the different structures and breeding systems occurring in natural populations using an idealised population and make the following assumptions:", style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
          
          h5(strong(tags$ol(
            tags$li("Organisms are diploid."),
            tags$li("Only sexual reproduction occurs."),
            tags$li("Generations are not overlapping (i.e., parents and offspring do not coexist)."),
            tags$li("Mating is random."),
            tags$li("Population size is infinitely large."),
            tags$li("Allele frequencies are equal in males in females."),
            tags$li("There is no migration."),
            tags$li("There is no mutation."),
            tags$li("There is no natural selection."))),style = "color:black;text-align:justify"),
          
          br(),
          
          h4(p(strong("The Hardy-Weinberg principle"), style = "color:black;text-align:center")),
             
          br(),
          
          p("We can’t begin to understand what a force does until we understand what would happen in the absence of any forces.",strong("Zero-force"), "concepts help us to grasp this idea. A famous example of a zero-force concept is the first law of Newton: ", style = "color:black;text-align:justify"),
          
          p("An object at rest remains at rest, and an object that is moving will continue to move straight and with constant velocity, if and only if there is no net force acting on that object.", style = "color:black;text-align:justify"),
          
          p("An analogous concept in population genetics is called the",strong("Hardy-Weinberg principle."), "This principle was the first important model developed in population genetics and states that an allele will not be lost or reach fixation unless a force acts on it, assuming the attributes of an idealised population. This model predicts the proportion of homozygotes and heterozygotes individuals occurring in the next generation based on the allele frequencies existing in the present population.", style = "color:black;text-align:justify"),
          
          h5(p("In the simplest case of a single locus with two alleles denoted",strong("A"), "and",strong("a"), "with frequencies:",strong(em("f(A) = p")), "and",strong(em("f(a) = q,")), "the expected genotype proportions are:", style = "color:black;text-align:justify")),
          
          h5(strong(tags$ol(
            tags$li(em("f(Aa) = 2pq"), "for heterozygotes."),
            tags$li(em("f(AA) = p2"), "for AA homozygotes,"),
            tags$li(em("f(AA) = p2"), "for AA homozygotes,"),style = "color:black;text-align:justify"))),
          
          style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
      )
        ),
      
      br(),
      
      h3(p(
        em("ACTIVITY "),
        icon("dna", lib = "font-awesome"),
        style = "color:black;text-align:center"
      )),
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}"
        )
      ),
      
      br(),
      
      sidebarLayout(
        
        sidebarPanel(
          
          p("To see the Hardy-Weinberg principle in action, use the slider below to choose the frequency of the",strong("A"),"allele",strong("(p)."),"As we have just two alleles, the frequency of the",strong("a"),"allele is simply:",strong(em("q = 1 - p.")), style = "color:black;text-align:justify"),
          
          p("This graphic shows the genotype proportions based on the Hardy–Weinberg principle, which assumes that the union of the gametes produced by females and males is random.", style = "color:black;text-align:justify"),
          
          p(strong("Can you tell what is the allele frequency that maximises the most the proportion of heterozygotes?"), style = "color:black;text-align:justify"),
          
          sliderInput(
            "p_freq",
            tags$div(
              "Frequency of allele A (p)"
            ),
            value = 0.2,
            min = 0,
            max = 1,
            step = 0.01
          ),
          br()
        ),
        mainPanel(
          plotOutput("plot_HWE")
        )
      ),
      br(),
      
      p(em("Developed by"), br("Luis Mijangos"), style =
          "text-align:center; font-family: times")
    ),
    
    #######################################################################
    ############### GENETIC DIVERSITY ############################
    #######################################################################
    tabPanel(
      "4. Genetic diversity",
      
      fluidRow(
        column(width = 3),
        column(width = 6, 
               h3(p(strong("How can we measure genetic diversity?"),
                    style = "color:black;text-align:center")
               ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
      ),
      
      br(),
      
      fluidRow(
        
        column(width = 3),
        column(width = 6,
               br(),
               
               p("We can measure",strong("genetic diversity"), "within populations by estimating the proportion of heterozygotes in each locus. This measure is called",strong("heterozygosity."), "We can measure heterozygosity in two ways. The first one consists in simply counting the actual number of heterozygote individuals and divide it by the number of individuals. This measure is called",strong("Observed heterozygosity"),"and abreviatead",strong(em("Ho.")), "The second way is to use the Hardy-Weinberg principle to calculate the expected number of heterozygotes based on allele frequencies. This measure is called",strong("Expected heterozygosity"),"and abreviated",strong(em("He.")),"So,", strong(em("He = 2pq.")), "This is the preferred measure to report because it is less affected than Ho when we don’t sample the whole population (i.e. sampling bias).", style = "color:black;text-align:justify"),

               p("In the next section, we will learn about one of the evolutionary forces that have major consequences in 'ecological timeframes' (i.e. in a period of tens to hundreds of years), especially in small populations:",strong("genetic drift."), style = "color:black;text-align:justify"),
      
      style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
      ),

      br(),

      p(em("Developed by"), br("Luis Mijangos"), style =
          "text-align:center; font-family: times")
    )
    ),
    
    #######################################################################
    ############### GENETIC DRIFT #########################################
    #######################################################################
    
    tabPanel(
      "5. Genetic drift",

      fluidRow(
        column(width = 3),
        column(width = 6, 
               h3(p(strong("What is genetic drift?"),
                    style = "color:black;text-align:center")
               ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
      ),
      br(),

      fluidRow(
        column(width = 3),
        column(width = 6,
        p("Within a population, alleles are not sampled perfectly when parents pass their alleles to their children; instead, rare alleles (those with a low frequency) are prone to be lost and common alleles to become fixed. This sampling error is called genetic drift, and its effects increase as the population size decreases.", style = "color:black;text-align:center"),
        
        p("Let’s use simulations to exemplify how genetic drift changes allele frequencies in populations of different sizes. Use the slider below to choose a population size to simulate, and then click on the RUN button and wait a few seconds for the simulation results to appear. Try to run simulations with different population sizes and pay attention to how much allele frequencies change from one generation to the next.", style = "color:black;text-align:center"),
        
        p("Can you identify which alleles are lost and which become fixed?", style = "color:black;text-align:center"),

        p("When you are done, click on the next section:",strong("effective population size"), style = "color:black;text-align:center"),

        
        style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")
      ),
      hr(),

      h3(p(
        em("ACTIVITY "),
        icon("dna", lib = "font-awesome"),
        style = "color:black;text-align:center"
      )),
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}"
        )
      ),

      br(),

      sidebarLayout(
        sidebarPanel(

          sliderInput(
            "population_size_phase2",
            tags$div(
              "Population size"
            ),
            value = 10,
            min = 2,
            max = 100,
            step = 2
          ),
          br(),
          actionButton(
            "close_drift",
            label = h4(strong("RUN")),
            icon = icon("play"),
            width = "100%",
            class = "btn-success"
          )
        ),
        mainPanel(
          imageOutput("plot_freq")
        )
      ),
      br(),

      p(em("Developed by"), br("Luis Mijangos"), style =
          "text-align:center; font-family: times")
    ),
    
    #######################################################################
    ############### Effective population size ############################
    #######################################################################
          tabPanel(
            "6. Effective population size",
            
          fluidRow(
            column(width = 3),
            column(width = 6, 
                   h3(p(strong("What is effective population size?"),
                style = "color:black;text-align:center")
            ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
          ),
          br(),

          fluidRow(

            column(width = 3),
            column(width = 6,
              p("The effective population size is a powerful idea that reduces the effect of genetic drift on genetic diversity to one variable allowing us to use population genetic models without having to worry about the specific characteristics of a population that are difficult or even impossible to determine.", style = "color:black;text-align:justify"),

              p("Effective population size (abbreviated Ne) is defined as the size of an idealised population that would have the same amount of genetic drift as the population under consideration.",style = "color:black;text-align:justify"),

              p("Not very enlightening, isn't it? Don't worry, we will go bit by bit and use computer simulations to explain the difficult parts.",style = "color:black;text-align:center"),

              style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
            )
          ),

          hr(),

          h3(p(
            em("SIMULATIONS "),
            icon("dna", lib = "font-awesome"),
            style = "color:black;text-align:center"
          )),
          tags$style(
            HTML(
              ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}"
            )
          ),
          tags$style(
            HTML(
              ".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}"
            )
          ),

          br(),

          p(em("Developed by"), br("Luis Mijangos"), style =
              "text-align:center; font-family: times")
        ),
      
      #######################################################################
      ############### Ne and family size ############################
      #######################################################################
      
      tabPanel(
        "7. Ne and family size",
        
        fluidRow(
          column(width = 3),
          column(width = 6, 
                 h3(p(strong("Ne and family size"),
                      style = "color:black;text-align:center")
                 ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
        ),
        br(),

      fluidRow(
        column(width = 3),
        column(width = 6,
          p("We are going to use the equation of loss of heterozygosity across generations:",
            style = "color:black;text-align:justify"
          ),
          p(tags$img(src="equation.png",width="160px",height="80px",style="border: 1px solid black; margin-left: 100px")),
          p("where He0  is heterozygosity at generation 0 and t is the number of generations."),
          style = "background-color:lavender;border-radius: 10px"
        ),
      ),
      br(),

      fluidRow(
        column(width = 3),
        column(width = 6,
          p("To understand how family size affects Ne, enter different values in the input boxes for the simulations and then clic RUN once and wait around 5 seconds to get the results",
            style = "color:black;text-align:center"
          ),
          style = "background-color:papayawhip;border-radius: 10px"
        )
      ),

      hr(),
      h3(p(
        em("SIMULATIONS "),
        icon("chart-pie", lib = "font-awesome"),
        style = "color:black;text-align:center"
      )),
      tags$style(
        HTML(
          ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}"
        )
      ),
      tags$style(
        HTML(
          ".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}"
        )
      ),

      br(),
          
            sidebarLayout(
              sidebarPanel(

                sliderInput(
                  "variance_offspring_phase2",
                  tags$div(
                    "Coefficient that determines the variance in the number of offspring per mating"
                  ),
                  value = 1,
                  min = 0.1,
                  max = 10
                ),
                br(),
                actionButton(
                  "close_size",
                  label = h4(strong("RUN")),
                  icon = icon("play"),
                  width = "100%",
                  class = "btn-success"
                )
              ),
              mainPanel(

                imageOutput("plot_size"),

              )
            ),

      br()
    ),
    
    #######################################################################
    ############### Ne and sex ratio #########################################
    #######################################################################

    # tabPanel(
    #   "8. Ne and sex ratio",
    # 
    #   fluidRow(
    #     column(width = 2),
    #     column(h4(
    #       p("Introduction", style = "color:black;text-align:center")
    #     ),
    #     width = 8, style = "background-color:lavender;border-radius: 10px")
    #   ),
    #   br(),
    # 
    #   fluidRow(
    #     column(width = 2),
    #     column(
    #       p("We are going to use the equation of loss of heterozygosity across generations:",
    #         style = "color:black;text-align:justify"
    #       ),
    #       p(tags$img(src="equation.png",width="160px",height="80px",style="border: 1px solid black; margin-left: 100px")),
    #       p("where He0  is heterozygosity at generation 0 and t is the number of generations."),
    #       width = 8,
    #       style = "background-color:lavender;border-radius: 10px"
    #     ),
    #   ),
    #   br(),
    #   fluidRow(
    #     column(width = 2),
    #     column(
    #       p("To understand how family size affects Ne, enter different values in the input boxes for the simulations and then clic RUN once and wait around 5 seconds to get the results",
    #         style = "color:black;text-align:center"
    #       ),
    #       width = 8,
    #       style = "background-color:papayawhip;border-radius: 10px"
    #     )
    #   ),
    # 
    #   hr(),
    #   h3(p(
    #     em("SIMULATIONS "),
    #     icon("chart-pie", lib = "font-awesome"),
    #     style = "color:black;text-align:center"
    #   )),
    #   tags$style(
    #     HTML(
    #       ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}"
    #     )
    #   ),
    #   tags$style(
    #     HTML(
    #       ".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}"
    #     )
    #   ),
    # 
    #   br(),
    # 
    #   sidebarLayout(
    #     sidebarPanel(
    # 
    #       sliderInput(
    #         "variance_offspring_phase2",
    #         tags$div(
    #           "Coefficient that determines the variance in the number of offspring per mating"
    #         ),
    #         value = 1,
    #         min = 0.1,
    #         max = 10
    #       ),
    #       br(),
    #       actionButton(
    #         "close_sex",
    #         label = h4(strong("RUN")),
    #         icon = icon("play"),
    #         width = "100%",
    #         class = "btn-success"
    #       )
    #     ),
    #     mainPanel(
    # 
    #       imageOutput("plot2"),
    # 
    #     )
    #   ),
    #   br()
    # ),

    #######################################################################
    ######################## TEST #########################################
    #######################################################################

    tabPanel(
      "8. Test",
      
      fluidRow(
        column(width = 3),
        column(width = 6, 
               h3(p(strong("Let's review what we have learned"),
                    style = "color:black;text-align:center")
               ), style = "background-color:burlywood;padding:10px;border-radius: 20px")
      ),
      
      br(),
      fluidRow(
        column(width = 3),
        column(width = 6,
          p("Go for it!", style = "color:black;text-align:center"),
            style = "background-color:lavender;border-radius: 10px"
        )
      ),
      hr(),
      navlistPanel(
        widths = c(2, 10),
        tabPanel(
          "Level 1",
          
          column(
            width = 12,
            h3("Level 1 questions (Theory)"),
            p(
              "1. QUESTION"
            ),
            fluidRow(column(
              width = 4,
              radioButtons(
                "Question1",
                "",
                choices = c(
                  'a. option1' = 1,
                  'b. option2' = 2,
                  'c. option3' = 3,
                  'd. option4' = 4
                ),
                selected = "_None"
              )
            ),
            column(width =
                     8, uiOutput("Answer1"))),
            br()
            
          )
          
        ),
        tabPanel(
          icon("book"),
          column(
            width = 12,
            h3("Glossary"),
            br(),
            p(strong("TEXT"),
              "TEXT"
            )
          )
          
          
        )
      )
      
    )
  )
)
)
