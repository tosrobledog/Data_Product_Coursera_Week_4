library(shiny)
library(shinydashboard)
library(visNetwork)
library(DT)
library(plotly)

shinyUI(dashboardPage(
        skin = "green",
        dashboardHeader(title = "Data Product"),
        dashboardSidebar(sidebarMenu(
                sliderInput("size", "Size of the Nodes:",
                            min = 10,
                            max = 100,
                            value = 63),
                menuItem("Networks", tabName = "networks", icon = icon("dashboard")),
                menuItem("About", tabName = "about", icon = icon("th"))
        )),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "networks",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Network", visNetworkOutput("network")),
                                            tabPanel("Local", DT::dataTableOutput("local")),
                                            tabPanel("Global", DT::dataTableOutput("global")),
                                            tabPanel("Topology", plotlyOutput("topology",)))
                                ),
                        tabItem(tabName = "about", h4("The purpose of this shiny app is to show 
                                                      how can we select papers using graph theory. 
                                                      The app uploads a data file with the query 
                                                      Marketing Word of Mouth from Web of Science. 
                                                      The app uploads a graph object: nodes and links. 
                                                      The first tab shows the current papers of this 
                                                      topic and the color depends on the publication 
                                                      year. The second tab shows a list of the main 
                                                      papers: seminals, structural, and currents 
                                                      with the main network metrics: in degree, 
                                                      out degree, and betweenness.  The third one 
                                                      refers to the global metrics of the citation 
                                                      network, such as density, transitivity, diameter, 
                                                      and centralization. Finally, topology's tab presents 
                                                      the log-log distribution. "))
                )
        )
))