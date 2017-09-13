#### interactive plot ####

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr","plotly","shiny","tidyverse"))

ui <- fluidPage(
  plotlyOutput("distPlot", height = 950)
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({
    stores.2017 %>% ggplot(aes(IRs.Lane.Week, IRs.Week, color = Region, size = Num.Lanes))+geom_point()
  })
}

shinyApp(ui = ui, server = server)


