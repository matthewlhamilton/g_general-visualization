
library(statnet)
# library(readxl)
library(reshape2)
library(ggplot2)
library(dplyr)
library(here)
library(networkD3)

# read raw files ----
cel <- readRDS("../../Data/actorConceptEdgelist.rds")
lup <- readRDS("../../Data/conceptConsolidation.rds")
attr <- readRDS("../../Data/actorAttr.rds")
# ----

el <- unique(cel[,c("sConceptCons","rConceptCons","sign")])

ac_el <- as.data.frame(summarise(group_by(cel, actor, sConceptCons, rConceptCons)))
tmpdf <- as.data.frame(summarise(group_by(ac_el, sConceptCons, rConceptCons), n=n()))
tmpdf <- tmpdf[-which(tmpdf$n == 1),]

smallNames <- c(as.character(tmpdf$sConceptCons), as.character(tmpdf$rConceptCons))

net <- network(el, matrix.type='edgelist',ignore.eval=FALSE,names.eval='weight')

delete.vertices(net, which(!net%v%"vertex.names" %in% smallNames))

links <- as.data.frame(as.edgelist(net,attrname="weight"))

links[,1:2] <- links[,1:2] - 1
linkcols = ifelse(links$V3 == 1, "#fb9a99", "#80b1d3")

nodes <- data.frame(concept = net%v%"vertex.names"
                    , deg = sna::degree(net))

classdf <- data.frame(concept = c(as.character(cel$sConceptCons), as.character(cel$rConceptCons))
                      , class = c(as.character(cel$sConceptClass1), as.character(cel$rConceptClass1))
                      , class2 = c(as.character(cel$sConceptClass2), as.character(cel$rConceptClass2))                      )

classdf$be <- "other"
classdf$be <- ifelse(classdf$class == "strategies","behavior",classdf$be)
classdf$be <- ifelse(classdf$class2 %in% c("context-envTrends","context-envShocks"),"hazard",classdf$be)



nodes$group <- with(classdf, class[match(nodes$concept, concept)])
nodes$begroup <- with(classdf, be[match(nodes$concept, concept)])

library(RColorBrewer)

groupColor <- c("#d9d9d9","#984ea3", "#4daf4a")
groupColor <- paste0('"',paste(groupColor, collapse = '", "'),'"')


forceNetwork(Nodes = nodes, Links = links
             , Value = "V3"
             , linkWidth = JS("function(d) { return (d.value)*0 + 1; }")
             # , linkWidth = .1
             , charge=-155
             , Source = "V1", Target = "V2"
             , NodeID = "concept", Group = "begroup"
             , Nodesize = "deg"
             , opacity = 1
             , arrows = TRUE
             # , radiusCalculation = JS("Math.sqrt(d.nodesize)*5+2.5")
             # , colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);")
             , colourScale = paste0('d3.scaleOrdinal().range([',groupColor,'])')
             , linkColour = linkcols
             , fontSize = 15
             , fontFamily = 'Arial'
             , opacityNoHover = 0
             , bounded = TRUE
             , legend=T
             , zoom = T
             # , clickAction = MyClickScript
) %>%  saveNetwork(file = 'meganetwork.html')
