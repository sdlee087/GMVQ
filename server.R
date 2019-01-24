#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

##Preliminaries
graph_data_mu <- list()
graph_data_K <- list()
for(n in c("10","50","200")){
  x4 <- read.csv(paste0("n",n,".csv"))[1,]
  r4 <- read.csv(paste0("n",n,".csv"))[-1,]
  
  ns <- nrow(r4)/2
  ds <- ncol(r4)
  
  K <- 1/r4[seq(2,2*ns,by=2),]
  mu <- r4[seq(1,2*ns,by=2),]*K
  
  mus <- c()
  Ks <- c()
  for(i in 1:ds) mus[i] <- paste0("mu",i)
  for(i in 1:ds) Ks[i] <- paste0("K",i)
  colnames(mu) <- mus
  colnames(K) <- Ks
  mu_tib <- as_tibble(mu)
  K_tib <- as_tibble(K)
  
  mu_lambdas <- list()
  K_lambdas <- list()
  for(i in 1:ns){
    mu_lambdas[[i]] <- mu_tib[i,] %>% gather(key = "index", value = "mu") %>% mutate(index = factor(index, levels = mus))
    K_lambdas[[i]] <- K_tib[i,] %>% gather(key = "index", value = "K") %>% mutate(index = factor(index, levels = Ks))
  }
  graph_data_mu[[n]] <- mu_lambdas
  graph_data_K[[n]] <- K_lambdas
}

shinyServer(function(input, output) {
  output$muPlot <- renderPlot({
    graph_data_mu[[input$n]][[1-input$target*5]] %>% ggplot(aes(x=mu)) + geom_histogram(binwidth = 0.5) + 
      coord_cartesian(xlim = c(-6, 6), ylim = c(0,nrow(graph_data_mu[[input$n]][[1-input$target*5]])*3/5) )
  })
  output$KPlot <- renderPlot({
    graph_data_K[[input$n]][[1-input$target*5]] %>% ggplot(aes(x=K)) + geom_histogram(binwidth = 0.5) + 
      coord_cartesian(xlim = c(0, 10), ylim = c(0,nrow(graph_data_mu[[input$n]][[1-input$target*5]])*3/5) )
  })
  
})
