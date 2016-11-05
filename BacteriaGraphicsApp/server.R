list.of.packages <- c("shiny", "shinydashboard", "data.table", "magrittr", 
                      "d3heatmap", "ggvis", "visNetwork", "igraph", "XLConnect")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(data.table)
library(magrittr)
library(d3heatmap)
library(ggvis)
library(visNetwork)
library(igraph)
library(XLConnect)
options(shiny.maxRequestSize = 10*1024^2)

shinyServer(function(input, output, session) {

   DataEntry <- reactive({
    
    wb <- loadWorkbook(input$dataset$datapath)
    sheet.names <- getSheets(wb)
    
    data.list <- lapply(sheet.names, function(x){
      readWorksheet(wb, sheet = x)
    })
    names(data.list) <- sheet.names
    
    data.list <- Map(function(x, y){
      as.data.table(x) %>%
        .[, ID := y]
    },data.list, sheet.names)
    
    data <- rbindlist(data.list)
    setnames(data, names(data), c("Species", "Genus", "Family", "count", "ID"))
    data
  })
  
  
  observeEvent(input$dataset, {
    updateSelectInput(session, "size", choices = c("No Size" = "size", names(DataEntry())))
    updateSelectInput(session, "color", choices = c("No Color" = "color", names(DataEntry())))
    updateSelectInput(session, "plotxvar", choices = names(DataEntry()))
    updateSelectInput(session, "plotyvar", choices = names(DataEntry()))
    updateSelectInput(session, "plotfilter", choices = names(DataEntry()))
    updateSelectInput(session, "plotgroup", choices = names(DataEntry()))
    updateSelectInput(session, "family", choices = unique(DataEntry()$Family))
  })
  
  
  WorkingData <- reactive({
    inpt_family <- input$family
    inpt_cutoff <- input$cutoff
    
    data <- DataEntry()[Family %in% inpt_family & count >= inpt_cutoff, ]
    data
  })
  
#   SummaryData <- reactive({
#     data <- as.data.table(WorkingData())
#     data[,summary(.SD), .SDcols = paste(input$summaryvar)]
#   })
#   
#   
#   output$table <- renderDataTable({
#     SummaryData()  
#   })
#   
#   
#   output$filterlevel <- renderUI({
#     selectizeInput("level", "Select Filter Levels", 
#                    choices = WorkingData()[,unique(get(input$filter))], multiple = TRUE)
#   })
  
  plotdata <- reactive({
    data <- WorkingData() 
    data[, ":=" (size = 1, color = 1)]
    
    type <- input$plotType
    xvar <- input$plotxvar
    yvar <- input$plotyvar
    group <- input$plotgroup
    filter <- input$plotfilter
    whisker <- input$coef
    level <- input$level
    
    if(!{is.null(input$size)}) inputsize <- input$size
    if(!{is.null(input$color)}) inputcolor <- input$color
    
    if(!{is.null(filter)}) data <- data[get(filter) %in% paste(level)]
    
    if(is.null(group)){  
      data <- data[, .(x = get(xvar), 
                       y = get(yvar),
                       size = get(inputsize),
                       color = get(inputcolor))]
    } else{
      data <- data[, .(x = get(xvar), 
                       y = get(yvar),
                       size = get(inputsize),
                       color = get(inputcolor)), by = .(Group = get(group))][order(Group)]
    }
    
    list(data = data, type = type, coef = whisker)
  })
  
  observeEvent(input$createplot, {
    dd <- plotdata()$data %>% as.data.frame(.)
    
    if(!{is.null(dd$Group)}){
      dd <- dd %>% group_by(Group) %>%
        ggvis(x = ~x, y = ~y, fill = ~factor(Group))
    } else{
      dd <- dd %>% ggvis(x = ~x, y = ~y)
    }
    
    if(plotdata()$type == "Histogram"){
      dd %>% 
        layer_histograms() %>%
        add_axis("x", title = paste(input$plotxvar)) %>%
        add_axis("x", orient = "top", title = paste(input$titletext), ticks = FALSE) %>%
        bind_shiny("plot")
    }
    
    if(plotdata()$type == "Scatter"){
      dd %>% 
        layer_points(size = ~size, fill = ~color) %>%
        add_axis("x", title = paste(input$plotxvar)) %>%
        add_axis("y", title = paste(input$plotyvar)) %>%
        add_axis("x", orient = "top", title = paste(input$titletext), ticks = FALSE) %>%
        bind_shiny("plot")
    }
    
    if(plotdata()$type == "Boxplot"){
      dd %>% 
        layer_boxplots(coef = plotdata()$coef) %>% 
        add_axis("x", title = paste(input$plotxvar)) %>%
        add_axis("y", title = paste(input$plotyvar)) %>%
        add_axis("x", orient = "top", title = paste(input$titletext), ticks = FALSE) %>%
        bind_shiny("plot")
    }
    
  })
  
  
  NetworkData <- eventReactive(input$updatenetwork,{
    
    WorkingData()[, weight := 1000/count]
    bacteriaALLids <- WorkingData()[, .N, by = Species] %>% .[N==12, Species]
    WorkingData()[, group := ifelse(ID %like% "^PIHC", "PostInfection", "NoInfection")]
    netdata <- WorkingData()[!{Species %in% bacteriaALLids}, ]
    
    nodes.id <- unique(netdata[ ,.SD, .SDcols = c("ID", "group")])
    setnames(nodes.id, "ID", "label")
    nodes.id[, `:=` (shape = "star", title = paste(label))]
    
    nodes.spec <- netdata[, .SD, .SDcols = c("Species", "Genus")]
    setnames(nodes.spec, c("Species","Genus"), c("label", "group"))
    nodes.spec[, `:=` (shape = "circle", title = paste(label))]
    
    nodes <- rbind(nodes.id, nodes.spec)
    nodes[, id := 1 : .N]
    setcolorder(nodes, c("id", "label", "group", "shape", "title"))
    
    edges <- netdata[, .SD, .SDcols = c("Species", "ID", "weight")]
    setnames(edges, c("Species", "ID", "weight"), c("from", "to", "length"))
    edges[, `:=` (from = match(from, nodes$label), to = match(to, nodes$label))]
    
    list(nodes = nodes, edges = edges)
    
  })
  
  
  output$network <- renderVisNetwork({
       if(is.null(NetworkData())) return()
    visNetwork(NetworkData()$nodes, NetworkData()$edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group", manipulation = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE) %>%
      visLegend()
  })
  
  
  
  heatdata <- eventReactive(input$updateheatmap, {
    inpt_rowclust <- input$rowcluster
    inpt_colclust <-input$colcluster
    list(data = dcast(WorkingData(), Species ~ ID, sum, value.var = "count"),
         rowclust = inpt_rowclust, 
         colclust = inpt_colclust)
  })
  
  output$heatmap <- renderD3heatmap({
    if(is.null(heatdata())) return()
    data <- as.data.frame(heatdata()[["data"]])
    rnames <- data[,1]
    data <- data[,-1]
    cnames <- colnames(data)
    data <- as.matrix(data)
    rownames(data) <- rnames
    colnames(data) <- cnames
    d3heatmap(data, na.rm = T, Rowv = heatdata()[["rowclust"]], Colv = heatdata()[["colclust"]], scale = "row")
  })
  

})
