
library(shiny)
library(gifski)
library(png)


shinyServer(function(input, output,session){
  
  # Sys.sleep(10)
  # waiter_hide()
  # 
  # session$onFlushed(function() {
  #   Sys.sleep(5)
  #   waiter::waiter_hide()
  #   # showModal(licenseModal())
  # })
  # 
  
  # observeEvent(input$q_distribution, {
  #   
  #   shinyjs::toggleElement(
  #     id = "q_gral",
  #     condition = input$q_distribution == "equal"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "mutation_rate",
  #     condition = input$q_distribution == "equation"
  #   )
  #   
  # })
  
  # observeEvent(input$h_distribution, {
  #   
  #   shinyjs::toggleElement(
  #     id = "h_gral",
  #     condition = input$h_distribution == "equal"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "dominance_mean",
  #     condition = input$h_distribution == "normal"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "dominance_sd",
  #     condition = input$h_distribution == "normal"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "intercept",
  #     condition = input$h_distribution == "equation"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "rate",
  #     condition = input$h_distribution == "equation"
  #   )
  #   
  # })
  
  # observeEvent(input$s_distribution, {
  #   
  #   shinyjs::toggleElement(
  #     id = "s_gral",
  #     condition = input$s_distribution == "equal"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "gamma_scale",
  #     condition = input$s_distribution == "gamma"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "gamma_shape",
  #     condition = input$s_distribution == "gamma"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "log_mean",
  #     condition = input$s_distribution == "log_normal"
  #   )
  #   
  #   shinyjs::toggleElement(
  #     id = "log_sd",
  #     condition = input$s_distribution == "log_normal"
  #   )
  #   
  # })
  # 
  
  #######################################################################
  ############### MODELS #########################################
  #######################################################################
  
  ################# HWE #################
  
  output$plot_HWE <- renderPlot({
    p <- input$p_freq

    n <-  100
    p <- round(p,2)
    q <- 1 - p
    
    AA <- p^2 * n
    aA <- p*q* n
    Aa <- p*q* n
    aa <- q^2 * n
    max_freq <- max(AA,aa,aA,Aa)/2
    
    freqs <- as.data.frame(rbind(AA,aA,Aa,aa))
    
    colors_plot <- rainbow(4)
    
ggplot() + 
      geom_rect(aes(xmin = 0,xmax = -AA/2,ymin =0,ymax = AA/2), fill = colors_plot[1],alpha = 1/2,color = "black")+
      geom_rect(aes(xmin = 0,xmax = Aa/2,ymin = 0,ymax = Aa/2), fill = colors_plot[2],alpha = 1/2,color = "black")+
      geom_rect(aes(xmin = 0,xmax = -aA/2,ymin = 0,ymax = -aA/2), fill = colors_plot[3],alpha = 1/2,color = "black")+
      geom_rect(aes(xmin = 0,xmax = aa/2,ymin = 0,ymax = -aa/2), fill = colors_plot[4],alpha = 1/2,color = "black")+
      geom_rect(aes(xmin=-max_freq,xmax=max_freq,ymin=-max_freq,ymax=max_freq),alpha=1/15,color="black",size=1,fill="black")+
      geom_text(x = -max_freq/2 , y = max_freq*1.05,size=4,fontface="bold" ,aes(label = "A")) + 
      geom_text(x = max_freq/2 , y = max_freq*1.05,size=4,fontface="bold" ,aes(label = "a")) + 
      geom_text(x = -max_freq*1.05, y = max_freq/2,size=4,fontface="bold", aes(label = "A")) + 
      geom_text(x = -max_freq*1.05, y = -max_freq/2,size=4,fontface="bold",aes(label = "a")) +
      geom_text(x = -max_freq/2, y = max_freq/2 ,size=4,fontface="bold", aes(label = paste0("AA(p^2=",p^2,")"))) +
      geom_text(x = max_freq/2, y = max_freq/2,size=4,fontface="bold", aes(label = paste0("aA(q*p=",p*q,")"))) +
      geom_text(x = -max_freq/2, y = -max_freq/2,size=4,fontface="bold", aes(label = paste0("Aa(p*q=",p*q,")"))) +
      geom_text(x = max_freq/2, y = -max_freq/2,size=4,fontface="bold", aes(label = paste0("aa(q^2=",q^2,")"))) +
      geom_vline(xintercept = 0,size=1)+
      geom_hline(yintercept = 0,size=1)+
      xlab("Females gametes") + 
      ylab("Males\ngametes") + 
      theme_classic()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())+
      theme(axis.title.y=element_text(angle = 0,vjust=0.5,size=16,face="bold"))+
      theme(axis.title.x=element_text(size =16,face="bold"))+
      coord_fixed()
  
  })
  
  #######################################################################
  ############### WAHLUND EFFECT #########################################
  #######################################################################
  
  observeEvent(input$close_wal, {
    
    n_pop  <- isolate(input$no_pop)
    pop_sizes_tmp <- ceiling(100/n_pop)
    pop_sizes_tmp <- (pop_sizes_tmp %% 2 != 0) + pop_sizes_tmp
    pop_sizes <- paste(rep(pop_sizes_tmp,n_pop),collapse = " ")
    
    # reference table for the simulations
    res <- gl.sim.WF.table(file_var = "ref_variables.csv",
                           interactive_vars = FALSE,
                           chunk_number=50)
    
    sim <- gl.sim.WF.run(
      file_var = "sim_variables.csv",
      ref_table = res,
      every_gen = 2,
      sample_percent = 100,
      interactive_vars = FALSE,
      gen_number_phase2= 50,
      population_size_phase2= pop_sizes,
      number_pops_phase2 = n_pop,
      dispersal_phase2 = FALSE
    )
    
    res_1 <- sim[[1]]
    
    output$plot_wal <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      res_4 <- lapply(res_1,function(y){
        pop(y) <- rep(as.factor("1"),nInd(y))
        stats <- utils.basic.stats(y)
        res_wal <- as.data.frame(cbind(stats$overall[1], stats$overall[2], y$other$sim.vars$generation))
        return(res_wal)
        
      })
      
      res_5 <- rbindlist(res_4)
      colnames(res_5) <- c("Ho","He","gen")
      
      p <- ggplot(res_5)+
        geom_line(aes(x=gen,y=Ho,colour="Observed"),size=2) +
        geom_line(aes(x=gen,y=He,colour="Expected"),size=2) +
        # geom_point(size=4) +
        ylim(0,1)+
        scale_colour_manual(values = c("brown","deeppink")) +
        guides(color=guide_legend("Heterozygosity")) +
        theme_few(base_size = 18) +
        ylab("Heterozygosity") +
        xlab("GENERATION")+
        transition_reveal(gen)
      
      save_file <- paste0(getwd(),"/outfile_wal.gif")
      
      anim_save(save_file, animate(p,nframes=10,duration=4, height = 4, 
                                   width = 10, units = "in", res = 100,
                                   renderer = gifski_renderer()))
      
      # Return a list containing the filename
      list(src = save_file, contentType = 'image/gif')
      
    }, deleteFile = TRUE)
    
  })
  
  
  #######################################################################
  ############### GENETIC DRIFT #########################################
  #######################################################################
  
  ################# FREQUENCY #################
  observeEvent(input$close_drift, {
    
    # reference table for the simulations
    res <- gl.sim.WF.table(file_var = "ref_variables.csv",
                           interactive_vars = FALSE,
                           chunk_number=9)
    
    sim <- gl.sim.WF.run(
      file_var = "sim_variables.csv",
      ref_table = res,
      every_gen = 2,
      sample_percent = 100,
      interactive_vars = FALSE,
      gen_number_phase2= 30,
      population_size_phase2= isolate(input$population_size_phase2)  
    )
    
    res_1 <- sim[[1]]
    
    output$plot_freq <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')

      res_4 <- lapply(res_1,function(y){

        freq <- gl.percent.freq(y,verbose = 0)
        freq_2 <- cbind(freq,mut=y$other$loc.metrics$type)
        freq_2$gen <- y$other$sim.vars$generation
        return(freq_2)

      })

      res_5 <- rbindlist(res_4)
      res_5$frequency <- 1-(res_5$frequency/100)

      p <- ggplot(res_5,aes(x=gen,y=frequency,color=locus,group=locus))+
        geom_line() +
        geom_point(size=4) +
        ylim(0,1)+
        scale_colour_manual(values = rainbow(9)) +
        theme_few(base_size = 18) +
        ylab("Allele frequency") +
        xlab("GENERATION")+
        transition_reveal(gen)

      anim_save("outfile.gif", animate(p,nframes=15,duration=4, height = 4,
                                       width = 10, units = "in", res = 100,
                                       renderer = gifski_renderer()))

      # Return a list containing the filename
      list(src = "outfile.gif",contentType = 'image/gif')

    }, deleteFile = TRUE)

    ################# HETEROZYGOSITY #################

    # output$plot_het <- renderImage({
    #   # A temp file to save the output.
    #   # This file will be removed later by renderImage
    #   outfile <- tempfile(fileext='.gif')
    # 
    #   res_het <- lapply(res_1,function(y){
    #     het <- utils.basic.stats(y)
    #     het_2 <- het$Ho
    #     het_2$gen <- y$other$sim.vars$generation
    #     het_2$locus <- 1:nrow(het_2)
    #     return(het_2)
    #   })
    # 
    #   res_het <- rbindlist(res_het)
    #   colnames(res_het) <- c("het","gen","locus")
    #   res_het$locus <- as.factor(res_het$locus)
    # 
    #   p2 <- ggplot(res_het,aes(x=gen,y=het,color=locus,group=locus))+
    #     geom_line() +
    #     geom_point(size=4) +
    #     ylim(0,1)+
    #     scale_colour_manual(values = rainbow(9)) +
    #     theme_few(base_size = 18) +
    #     ylab("Heterozygosity") +
    #     xlab("GENERATION")+
    #     transition_reveal(gen)
    # 
    #   anim_save("outfile.gif", animate(p2,nframes=15,duration=4, height = 4, width = 10, units = "in", res = 100))
    # 
    #   # Return a list containing the filename
    #   list(src = "outfile.gif",contentType = 'image/gif')
    # 
    # }, deleteFile = TRUE)

  })
  
  #######################################################################
  ############### FAMILY SIZE #########################################
  #######################################################################
  
  observeEvent(input$close_size, {

    output$plot_size <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')

      res <- gl.sim.WF.table(file_var = "ref_variables.csv",
                             interactive_vars = FALSE)

      sim <- gl.sim.WF.run(
        file_var = "sim_variables.csv",
        ref_table = res,
        every_gen = 5,
        sample_percent = 50,
        interactive_vars = FALSE,
        variance_offspring_phase2= isolate(input$variance_offspring_phase2)
          )

      vars_sim <- sim[[1]][[1]]$other$sim.vars
      number_generations <- as.numeric(vars_sim$gen_number_phase2)
      Ne <- gsub('\"', "", vars_sim$population_size_phase2, fixed = TRUE)
      Ne <- as.numeric(gsub("'","",Ne))

      rate_of_loss <- 1 - (1 / (2 * Ne))
      every_gen <- 5

      fst_res <- lapply(sim[[1]],utils.basic.stats)

      # get the first value of He to calculate the rate of loss of heterozygosity
      first_het_pop1 <- mean(lapply(fst_res, "[[", "Hs")[[1]][, 1])
      # expected_het_pop1 <- as.vector(sapply(first_het_pop1, "*", (rate_of_loss ^ c(1:number_generations))))
      expected_het_pop1 <- c(first_het_pop1,first_het_pop1 * (rate_of_loss ^ c(1:(number_generations-1))))

      het_pops <- lapply(fst_res, "[[", "Hs")
      het_pops_2 <- lapply(het_pops, colMeans)
      het_pops_3 <- as.data.frame(Reduce(rbind,het_pops_2))
      colnames(het_pops_3) <- "het"

      gens_temp <- c(1,seq(0,number_generations,every_gen)[-1])

      expected_het_pop1<- expected_het_pop1[gens_temp]

      generations <- data.frame("gens"=gens_temp,"exp_het"=expected_het_pop1,"obs_het"=het_pops_3)

      # now make the animation
      p <- ggplot(generations, aes(gens)) +
                    geom_line(aes(y = het_pops_3$het, colour = "Simulations He"),size=1) +
                    geom_line(aes(y = expected_het_pop1, colour = "Expected He"),size=1) +
                    theme_dark(base_size = 18) +
                    labs(x="GENERATIONS", y="He", title=NULL)+
                    theme(legend.title=element_blank())+
                    theme(legend.position =  "bottom") +
        gganimate::transition_reveal(gens)

      anim_save("outfile.gif", animate(p,nframes=20,duration=4, 
                                       height = 4, width = 6, units = "in", 
                                       res = 100,
                                       renderer = gifski_renderer()))

      # Return a list containing the filename
      list(src = "outfile.gif",contentType = 'image/gif')

    }, deleteFile = TRUE)


  })
  
  #######################################################################
  ############### SEX RATIO  #########################################
  #######################################################################
  
  # observeEvent(input$close_sex, {
  #   
  #   output$plot2 <- renderImage({
  #     # A temp file to save the output.
  #     # This file will be removed later by renderImage
  #     outfile <- tempfile(fileext='.gif')
  #     
  #     res <- gl.sim.WF.table(file_var = "ref_variables.csv",
  #                            interactive_vars = FALSE)
  #     
  #     sim <- gl.sim.WF.run(
  #       file_var = "sim_variables.csv",
  #       ref_table = res,
  #       every_gen = 5,
  #       sample_percent = 50,
  #       interactive_vars = FALSE,
  #       parallel = FALSE,
  #       variance_offspring_phase2= isolate(input$variance_offspring_phase2),
  #       verbose = 0
  #     )
  #     
  #     vars_sim <- sim[[1]][[1]]$other$sim.vars
  #     number_generations <- as.numeric(vars_sim$gen_number_phase2)
  #     Ne <-  as.numeric(gsub('\"', "", vars_sim$population_size_phase2, fixed = TRUE))
  #     rate_of_loss <- 1 - (1 / (2 * Ne))
  #     every_gen <- 5
  #     
  #     fst_res <- lapply(sim[[1]],gl.basic.stats,verbose=0)
  #     
  #     # get the first value of He to calculate the rate of loss of heterozygosity
  #     first_het_pop1 <- mean(lapply(fst_res, "[[", "Hs")[[1]][, 1])
  #     # expected_het_pop1 <- as.vector(sapply(first_het_pop1, "*", (rate_of_loss ^ c(1:number_generations))))
  #     expected_het_pop1 <- c(first_het_pop1,first_het_pop1 * (rate_of_loss ^ c(1:(number_generations-1))))
  #     
  #     het_pops <- lapply(fst_res, "[[", "Hs")
  #     het_pops_2 <- lapply(het_pops, colMeans)
  #     het_pops_3 <- as.data.frame(Reduce(rbind,het_pops_2))
  #     
  #     gens_temp <- c(1,seq(0,number_generations,every_gen)[-1])
  #     
  #     expected_het_pop1<- expected_het_pop1[gens_temp]
  #     
  #     generations <- data.frame("gens"=gens_temp,"exp_het"=expected_het_pop1,"obs_het"=het_pops_3)
  #     
  #     # now make the animation
  #     p <- print(ggplot(generations, aes(gens)) +
  #                  geom_line(aes(y = het_pops_3$`1`, colour = "Simulations He"),size=1) +
  #                  geom_line(aes(y = expected_het_pop1, colour = "Expected He"),size=1) +
  #                  theme_dark(base_size = 18) +
  #                  labs(x="GENERATIONS", y="He", title=NULL)+ 
  #                  theme(legend.title=element_blank())+
  #                  theme(legend.position =  "bottom") +
  #                  theme(legend.text=element_text(size=14)), plot.margin=unit(c(-1,1,1,-1), "in") )+
  #       gganimate::transition_reveal(gens)
  #     
  #     anim_save("outfile.gif", animate(p,nframes=20,duration=4, height = 4, width = 6, units = "in", res = 100))
  #     
  #     # Return a list containing the filename
  #     list(src = "outfile.gif",contentType = 'image/gif')
  #     
  #   }, deleteFile = TRUE)
  #   
  #   
  # })
  
  #######################################################################
  ######################## TEST #########################################
  #######################################################################

  output$Answer1 <- renderUI({

    actionID <- input$Question1
    if(!is.null(actionID)){

      if(input$Question1 == 1){mensaje = "It is right!"}
      if(input$Question1 == 2){mensaje = "It is not correct!"}
      if(input$Question1 == 3){mensaje = "It is not correct!"}
      if(input$Question1 == 4){mensaje = "It is not correct!"}

      if(input$Question1 == 1){p(mensaje,icon("smile-wink","fa-2x"),style="background-color:#BFF7BB;padding:15px;text-align:justify;border-left: 8px solid green")}
      else
      {
        p(mensaje,icon("sad-cry","fa-2x"),style="background-color:#FFA8A8;padding:15px;text-align:justify;border-left: 8px solid red")
      }

    }
    else
    {""}

  })

}
)






