####################################################
##R server script 
###################################################

################################################;
#Author: Brian Batz;
#Program Purpose: Final Project;
#Date: 5/6/21;
################################################;

#read in packages we need
library(dplyr)
library(ggplot2)
library(Lahman)

#modify the Teams object (creating new teams object) as per instructions
teams <- Teams %>% filter((lgID == "AL" | lgID == "NL") & yearID >1901) 
teams <- teams %>%mutate(WinPct = W/G, rpg = R/G, hrpg = HR/G, TB = H + X2B + 2*X3B + 3*HR,kpg = SO/G, k2bb = SO/BB,WHIP = (3*(H+BB)/IPouts))
teams <- teams  %>% mutate(tbpg = TB/G) %>% select(-(franchID:WSWin),-(name:teamIDretro))


#####################################################
#Basic syntax for a shiny server
shinyServer(function(input, output) {
  filter_years <- reactive({
    teams %>% filter((yearID > input$year[1]) & (yearID < input$year[2]))  
  })
  
  output$table <- DT::renderDataTable({
     filter_years() %>% group_by(yearID) %>% summarise(Avg = round(mean(!!sym(input$plotvar)),2),.groups = 'drop') 
  })
  
  base_plot <- reactive({
    g <- ggplot(filter_years(),aes_string(x= "yearID", y = input$plotvar)) 
  })
  
  output$plot <- renderPlot({
    g <- base_plot()    
      if (input$curve && input$league){
        g <- g + geom_point(aes(color=lgID),na.rm=TRUE)+ stat_smooth(formula = y~x,method = "loess",aes(color=lgID),na.rm=TRUE)                + labs(x= "Year")           
      }
      else if(input$curve){
          g <- g + geom_point(na.rm=TRUE) + stat_smooth(formula = y~x,method = "loess",na.rm=TRUE) + labs(x= "Year")   
      }
      else if (input$league){
          g <- g + geom_point(aes(color=lgID),na.rm=TRUE) + labs(x= "Year")
      }
      else{
        g <- g + geom_point(na.rm=TRUE) + labs(x= "Year")
      }
      g
  })
  
    

  
})


