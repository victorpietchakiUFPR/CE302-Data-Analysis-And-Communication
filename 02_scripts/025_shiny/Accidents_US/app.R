library(shiny)
library(shinydashboard)

head <- dashboardHeader(title = "US Accidents")

sidebar <- dashboardSidebar(
       sidebarMenu(
           menuItem("Analise Temporal", tabName = "analise Temporal"),
           menuItem("Outros", tabName = "outros")
       )
    )

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "analise Temporal",
                h2("Analise Temporal")
        ),
        tabItem(tabName = "outros",
                h2("Outros")
        )
    )
)

ui <- dashboardPage(head, sidebar, body)

server <- function(input, output) {}

shinyApp(ui, server)