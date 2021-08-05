####################################################
##R ui script
###################################################

################################################;
#Author: Brian Batz;
#Program Purpose: Final Project;
#Date: 5/6/21;
################################################;

library(shiny)
library(dplyr)
library(Lahman)

#modify the Teams object (creating new teams object) as per instructions
teams <- Teams %>% filter((lgID == "AL" | lgID == "NL") & yearID >=1901) 
teams <- teams %>%mutate(WinPct = W/G, rpg = R/G, hrpg = HR/G, TB = H + X2B + 2*X3B + 3*HR,kpg = SO/G, k2bb = SO/BB,WHIP = (3*(H+BB)/IPouts))
teams <- teams  %>% mutate(tbpg = TB/G) %>% select(-(franchID:WSWin),-(name:teamIDretro))
teams_dropdown <- teams %>% select(X2B:hrpg,tbpg,kpg:WHIP) %>% rename("whip"= WHIP)

###################################################
shinyUI(fluidPage(
  titlePanel("Visualizing Baseball Data Over Time"),
  fluidRow(
    sidebarLayout(
        sidebarPanel(
          sliderInput("year",
                      "Include Years in Range",
                      value = c(min(teams$yearID),max(teams$yearID)),
                      min = min(teams$yearID),
                      max = max(teams$yearID),
                      sep = "" 
                      ),
          selectInput("plotvar", label = "Statistic to Plot", 
                      choices = colnames(teams_dropdown),
                      selected = "HR"),
          checkboxInput("league","Color by League?"),
          checkboxInput("curve","Add in trend across time?")

        ),
        
        mainPanel(
          plotOutput("plot"),
          DT::dataTableOutput("table")
          
        )
    )
  )
))

