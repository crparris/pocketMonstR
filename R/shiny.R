ui <- fluidPage(
  plotlyOutput("bar"),
  plotlyOutput("list")
)
server <- function(input, output) {
  output$bar <- renderPlotly({
    PokeDex %>% group_by(Primary_Type) %>% summarise(count = n()) %>%
      plot_ly(x = ~ Primary_Type, y = ~ count) %>% add_bars()
  })

  #plot_ly(x = ~ price, data = diamonds)  %>% add_histogram(color = I("pink"))
  output$list <- renderPlotly({
    click <- event_data("plotly_click")
    if (is.null(click)) return(NULL)
    #filter the data by the selected Cut value
    PokeDex %>%
      filter(Primary_Type %in% click$x) %>%
      #produce a histogram based on the filtered data. need to figure out how to display this visual before it's filtered as well
      #textOutput(Species, container = if (inline) span else div, inline = FALSE)
      plot_ly(x = ~ Species) %>% add_table(rownames=FALSE)
    #add_histogram(color = I("pink"))

  })
}
shinyApp(ui, server)
