library(data.table)
library(ggplot2)
library(ggvis)
library(visNetwork)
library(igraph)
library(dplyr)
library(d3heatmap)
library(MASS)
library(XLConnect)
library(magrittr)

  # sample network
nodes <- data.frame(id = paste0("s", 1:30))
edges <- data.frame(from = sample(nodes$id, 50, TRUE), to = sample(nodes$id, 50, TRUE))
visNetwork(nodes, edges)

setwd("~/School/Lan")

wb <- loadWorkbook("Mbale BacteriaList 5.4.15.xlsx")
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

data[, weight := 1000/count]

bacteriaALLids <- data[, .N, by = Species] %>% .[N==12, Species]

data.unique <- data[!{Species %in% bacteriaALLids},]
data.unique[, group := ifelse(ID %like% "^PIHC", "PostInfection", "NoInfection")]

netdata <- data.unique[Family == "Burkholderiaceae",]

nodes.id <- unique(netdata[ ,.SD, .SDcols = c("ID", "group")])
setnames(nodes.id, "ID", "label")

nodes.spec <- netdata[, .SD, .SDcols = c("Species", "Genus")]
setnames(nodes.spec, c("Species","Genus"), c("label", "group"))

nodes <- rbind(nodes.id, nodes.spec)
nodes[, id := 1 : .N]
setcolorder(nodes, c("id", "label", "group"))

edges <- netdata[, .SD, .SDcols = c("Species", "ID", "weight")]
setnames(edges, c("Species", "ID", "weight"), c("from", "to", "length"))
edges[, `:=` (from = match(from, nodes$label), to = match(to, nodes$label))]

visNetwork(nodes = nodes, edges = edges, directed = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group", manipulation = TRUE) %>%
  visLayout(randomSeed = 123) %>%
  visInteraction(dragNodes = TRUE, 
                 dragView = TRUE, 
                 zoomView = TRUE)
  visLegend() 



# visClusteringByColor()
# visClusteringByConnection()
# visClusteringByHubsize()
# visClusteringByGroup()
# visClusteringOutliers()




nodes <- data.frame(id = data[Genus %like% "Burkholderia", unique(Species)])

tmp.data <- data[Genus %like% "Burkholderia", Species, by = ID][order(ID)]
setkeyv(tmp.data, "ID")
n <- data[Genus %like% "Burkholderia", length(unique(Species))]
species <- tmp.data[,unique(Species)]
  
tmp <- list()
k <- 1
for(i in 1 : (n - 1)){
  for(j in (i + 1) : n){
    tmp[[k]] <- merge(tmp.data[Species %in% species[i], ],
                      tmp.data[Species %in% species[j], ], by = "ID")
      k <- k + 1
  }
}

edges <- rbindlist(tmp)
edges <- unique(edges[,.(from, to)])
setnames(edges, names(edges), c("ID", "from", "to"))
setcolorder(edges, c("from", "to", "ID"))


visNetwork(nodes,edges)