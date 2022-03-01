library(dartR)
library(data.table)
library(viridis)
library(gganimate)


shinyServer(function(input, output, session){ 

  observeEvent(input$q_distribution, {
    
    shinyjs::toggleElement(
      id = "q_gral",
      condition = input$q_distribution == "equal"
    )
    
    shinyjs::toggleElement(
      id = "mutation_rate",
      condition = input$q_distribution == "equation"
    )
    
  })
  
  observeEvent(input$h_distribution, {
    
    shinyjs::toggleElement(
      id = "h_gral",
      condition = input$h_distribution == "equal"
    )
    
    shinyjs::toggleElement(
      id = "dominance_mean",
      condition = input$h_distribution == "normal"
    )
    
    shinyjs::toggleElement(
      id = "dominance_sd",
      condition = input$h_distribution == "normal"
    )
    
    shinyjs::toggleElement(
      id = "intercept",
      condition = input$h_distribution == "equation"
    )
    
    shinyjs::toggleElement(
      id = "rate",
      condition = input$h_distribution == "equation"
    )
    
  })
  
  observeEvent(input$s_distribution, {
    
    shinyjs::toggleElement(
      id = "s_gral",
      condition = input$s_distribution == "equal"
    )
    
    shinyjs::toggleElement(
      id = "gamma_scale",
      condition = input$s_distribution == "gamma"
    )
    
    shinyjs::toggleElement(
      id = "gamma_shape",
      condition = input$s_distribution == "gamma"
    )
    
    shinyjs::toggleElement(
      id = "log_mean",
      condition = input$s_distribution == "log_normal"
    )
    
    shinyjs::toggleElement(
      id = "log_sd",
      condition = input$s_distribution == "log_normal"
    )
    
  })
  
  observeEvent(input$close, {
    
    ref_vars_temp <- as.data.frame(cbind(
      c(
        "real_freq",
        "real_loc",
        "loci_under_selection",
        "mutation_rate",
        "chunk_bp",
        "dominance_mean",
        "gamma_scale",
        "gamma_shape",
        "h_gral",
        "intercept",
        "log_mean",
        "log_sd",
        "q_gral",
        "rate",
        "s_gral",
        "targets_factor",
        "chromosome_name",
        "chunk_number",
        "chunk_recombination",
        "q_neutral",
        "q_distribution",
        "h_distribution",
        "s_distribution",
        "neutral_loci_chunk",
        "dominance_sd",
        "mutation",
        "exp_rate",
        "percent_adv"
      ),
      c(
        input$real_freq,
        input$real_loc,
        input$loci_under_selection,
        input$mutation_rate,
        input$chunk_bp,
        input$dominance_mean,
        input$gamma_scale,
        input$gamma_shape,
        input$h_gral,
        input$intercept,
        input$log_mean,
        input$log_sd,
        input$q_gral,
        input$rate,
        input$s_gral,
        input$targets_factor,
        input$chromosome_name,
        input$chunk_number,
        input$chunk_recombination,
        input$q_neutral,
        input$q_distribution,
        input$h_distribution,
        input$s_distribution,
        input$neutral_loci_chunk,
        input$dominance_sd,
        input$mutation,
        input$exp_rate,
        input$percent_adv
      )
    ))
    
    colnames(ref_vars_temp) <- c("variable", "value")
    
    theDataPlot <- reactive({
      print("Reactive: theDataPlot")
      # reference table for the simulations
      res <- gl.sim.WF.table(file_var = "ref_variables.csv",
                             interactive_vars = FALSE)
      
      sim <- gl.sim.WF.run(
        file_var = "sim_variables.csv",
        ref_table = res,
        every_gen = 1,
        sample_percent = 10,
        interactive_vars = FALSE,
        parallel = FALSE,
        # seed = 42,
        # selection_phase2 = TRUE,
        # dispersal_phase2 = dis,
        verbose = 0
      )
      
      res_1 <- sim[[1]]
      res_2 <- lapply(res_1,function(y){
        loc_keep <- which(y$other$loc.metrics$selection=="mutation_del" |
                            y$other$loc.metrics$selection=="mutation_adv")
        loc_keep_2 <- locNames(y)[loc_keep]
        loc_keep_3 <- gl.keep.loc(y,loc.list = loc_keep_2,verbose=0)
        return(loc_keep_3)
      })
      
      # res_3 <- lapply(res_2,dartR:::utils.reset.flags,verbose=0)
      
      res_4 <- lapply(res_2,function(y){
        
        freq <- gl.percent.freq(y,verbose = 0)
        freq_2 <- cbind(freq,mut=y$other$loc.metrics$selection)
        freq_2$gen <- y$other$sim.vars$generation
        return(freq_2)
        
      })
      
      
      res_5 <- rbindlist(res_4)
      res_5$frequency <- 1-(res_5$frequency/100)
    })
    
    output$plot1 <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      p <- ggplot(res_5,aes(x=gen,y=frequency,color=mut,group=locus))+
        geom_line() +
        geom_point() +
        scale_color_viridis(discrete = TRUE) +
        ggtitle("Popularity of American names in the previous 30 years") +
        # theme_ipsum() +
        ylab("Number of babies born") +
        transition_reveal(gen)
      
      anim_save("outfile.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )}, deleteFile = TRUE)
    
    # output$plot1 <- renderImage({
    #   # A temp file to save the output.
    #   # This file will be removed later by renderImage
    #   outfile <- tempfile(fileext='.gif')
    #   
    #   print(theDataPlot())  # Remember that print(theDataPlot) just prints the code
    # }, res=90, height=exprToFunction(input$plot_height1), width=exprToFunction(input$plot_width1))
    
    # stopApp(ref_vars_temp)
    
  })
  
})
