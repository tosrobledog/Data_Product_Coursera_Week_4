library(shiny)
library(dplyr)
library(igraph)
library(tidyverse)
library("poweRlaw")
library("ggplot2")
library(plotly)
library(ggplot2)
library(visNetwork)

graph <- read.graph(file = "data/WOM_Citations_cleaned.graphml", format = "graphml")

shinyServer(function(input, output) {
   
  df.1 <- reactive({
          
          graph.current <- delete.vertices(graph,
                                            degree(graph, mode = "in") == 0)
          
           current.nodes <- data.frame(
                   id = V(graph.current)$label,
                   outdegree = degree(graph.current, mode = "out"),
                   stringsAsFactors = FALSE
           )
          
           current.nodes <- head(current.nodes[with(current.nodes,
                                                    order(-outdegree)),], 80)
           df.1 <- current.nodes %>%
                                 separate(id, c("author", "year", "journal",
                                                "V", "P", "DOI"),
                                          ",")
           df.1$id <- current.nodes$id
          
           df.2 <- df.1[complete.cases(df.1),]
           df.2$DOI <- gsub("Doi", "", df.2$DOI )
           df.2$title <- paste0(df.2$author, "</b></p>", df.2$year, "</b></p>", df.2$journal)
           df.2$year <- as.numeric(df.2$year)
           df.2$color <- ifelse(df.2$year <= 2009,  "chocolate", ifelse(
                                df.2$year > 2009 & df.2$year <= 2014, "darkgreen", ifelse(
                                        df.2$year > 2014 & df.2$year <= 2017, "green", 
                                        "black")))
           df.2
  
          })
  
  df.2 <- reactive({
          
          network.properties <- data.frame(
                  id = V(graph)$label,
                  indegree = degree(graph, mode = "in"),
                  outdegree = degree(graph, mode = "out"),
                  betweenness = betweenness(graph, normalized = TRUE),
                  stringsAsFactors = FALSE
                  )
          df.2.1 <- network.properties %>%
                                        separate(id, c("author", "year", 
                                                       "journal", "V", 
                                                       "P", "DOI"),
                                                 ",")
          df.2.2 <- head(df.2.1[df.2.1$outdegree == 0,], 10)
          df.2.3 <- head(df.2.1[df.2.1$indegree == 0, ], 10)
          df.2.4 <- head(df.2.1[with(df.2.1, order(-betweenness)),], 10)
          df.2.5 <- rbind(df.2.2, df.2.3, df.2.4)
          df.2.5 
  })
  
  df.3 <- reactive({
          df.3.1 <- data.frame(Density = edge_density(graph, loops = FALSE),
                               Transitivity = transitivity(graph, type = "global"),
                               Diameter = diameter(graph, directed = TRUE, 
                                                   weights = NA),
                               Centralization = centr_degree(graph, 
                                                             mode = "all")$centralization,
                               stringsAsFactors = FALSE)
          df.3.1
  })
  
  output$local <- DT::renderDataTable({
         df.2()
          
  })
 
  output$global <- DT::renderDataTable({
          df.3()
  })
  
  output$topology <- renderPlotly({
          
          G <- graph
          
          # List of degrees
          G.degrees <- degree(G)
          
          # Let's count the frequencies of each degree
          G.degree.histogram <- as.data.frame(table(G.degrees))
          
          # Need to convert the first column to numbers, otherwise
          # the log-log thing will not work (that's fair...)
          G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
          
          # Now, plot it!
          v <- ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
                  geom_point() +
                  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                                     breaks = c(1, 3, 10, 30, 100, 300),
                                     trans = "log10") +
                  scale_y_continuous("Frequency\n(how many of them)",
                                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                                     trans = "log10") +
                  ggtitle("Degree Distribution (log-log)") +
                  theme_bw()
          ggplotly(v)
})
          
  output$network <- renderVisNetwork({

          nodes <- df.1()
          nodes.1 <- nodes[nodes$year < 2007,]
          nodes.2 <- nodes[nodes$year > 2008,]
          nodes <- rbind(nodes.1, nodes.2)
          dummy.1 <- nodes[nodes$year >= 2009 & nodes$year < 2013,]
          dummy.1 <- nodes[nodes$year == 2003,]
          dummy.2 <- nodes[nodes$year == 2006,]
          dummy.3 <- nodes[nodes$year >= 2009 & nodes$year < 2013,]
          dummy.4 <- nodes[nodes$year >= 2013 & nodes$year < 2017,]
          edges.1 <- data.frame(from = dummy.1$id, 
                                to = dummy.2$id,
                                stringsAsFactors = FALSE)
          edges.2.1 <- data.frame(from = dummy.2[1,"id"], 
                                  to = dummy.3[1:14,"id"],
                                  stringsAsFactors = FALSE)
          edges.2.2 <- data.frame(from = dummy.2[2,"id"], 
                                  to = dummy.3[15:28,"id"],
                                  stringsAsFactors = FALSE)
          edges.3.1 <- data.frame(from = dummy.3$id, 
                                  to = dummy.4[1:28,"id"],
                                  stringsAsFactors = FALSE)
          edges.3.2 <- data.frame(from = dummy.3[1:4, "id"],
                                  to = dummy.4[29:32, "id"],
                                  stringsAsFactors = FALSE)
          edgeslist <- rbind(edges.1, edges.2.1, edges.2.2, 
                             edges.3.1, 
                             edges.3.2)
          
          visNetwork(nodes, edges = edgeslist, height = "100%", 
                     width = "100%") %>%
                  visInteraction(dragNodes = FALSE,
                                 dragView = FALSE,
                                 zoomView = FALSE) %>% 
                  
                  visIgraphLayout(layout = "layout.gem",
                                  randomSeed = 123) %>%
                  visNodes(size = 50) %>%
                  visEdges(hidden = TRUE)  %>%
                  visOptions(manipulation = TRUE) 
  })
  observe({
          visNetworkProxy("network") %>%
                  visNodes(size = input$size)
          
          
                  
  })

})
