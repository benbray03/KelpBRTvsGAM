library(shiny)
library(leaflet)
library(readr)
library(sf)
library(dplyr)
library(ggplot2)

# source("0326modules/giant_se.R")
# source("0326modules/giant_csw.R")
#source("0326modules/bullkelp.R")
#source("0326modules/giant_stipes_csw.R")
#source("0326modules/giant_stipes_se.R")
source("0326modules/nerlue_5yrensemble.R")
source("0326modules/macpyr_5yrensemble.R")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Bull Kelp", bull_5_ui("bull_5_ui")),
    tabPanel("Giant Kelp", giant_5_ui("giant_5_ui"))
    # tabPanel("Giant Kelp CSW", giant_csw_ui("giant_csw_ui")),
    #tabPanel("Giant Kelp Stipes CSW", giantstipes_csw_ui("giantstipes_csw_ui")),
    # tabPanel("Giant Kelp SE", giant_se_ui("giant_se_ui")),
    #tabPanel("Giant Kelp Stipes SE", giantstipes_se_ui("giantstipes_se_ui"))
  )
)

server <- function(input, output, session) {
  # giant_se_server("giant_se_ui")
  #giantstipes_csw_server("giantstipes_csw_ui")
  # giant_csw_server("giant_csw_ui")
  bull_5_server("bull_5_ui")
  giant_5_server("giant_5_ui")
  #giantstipes_se_server("giantstipes_se_ui")
}

shinyApp(ui = ui, server = server)
