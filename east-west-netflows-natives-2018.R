---
  title:   "Top 25 net flows of Germans between east and west Germany in 2018 (w/o Berlin)"
  authors: "Andreas Genoni and Nikola Sander"
  date:    "6th of October 2020"
  
  mail:    "andreas.genoni@gmail.com"
  twitter: "@a_genoni"
  
  note:    "After running the code, the plot has been edited with Inkscape:"
           "1) Location of text fields have been customized."
           "2) Triangles have been added."
---
    

    
# load packages ----------------------------------------------------------------
library(tidyverse)
library(circlize)
library(patchwork)
library(hrbrthemes)
library(readxl)
#devtools::install_github("mattflor/chorddiag")
#library(chorddiag)



# set working directory --------------------------------------------------------
setwd("your\working directory")



# preliminaries ----------------------------------------------------------------

# read in data frame
netflow <- read_excel(path = 'bula-netflows-natives.xlsx')

# use values of first column "bula_quelle" as row names
mat <- column_to_rownames(netflow, var = "bula_quelle")

# transform into real matrix
rmat <-  mat %>%
  as.matrix()

# create matrix for link visibility
visible <- matrix(TRUE, nrow = nrow(rmat), ncol = ncol(rmat))
visible[rmat < 120] = FALSE
rownames(visible) = rownames(rmat)
colnames(visible) = colnames(rmat)



# plotting  --------------------------------------------------------------------


# save following plot as pdf (step 1)
pdf(file = "your\path",
    width = 9, # The width of the plot in inches
    height = 9,
    pointsize = 11) # The height of the plot in inches



# (re)set parameters
circos.clear()
circos.par(
  
  # where to start
  start.degree = 267, 
  
  # gaps between sectors
  gap.degree = c(8, 8, 8, 8, 8, 8, 8, 8, 8, 30, 
                 
                 8, 8, 8, 8, 30), 
  
  # gaps between grids and links
  track.margin = c(0.001, 0.001), 
  
  # turn off warnings for plotting outside cell
  points.overflow.warning = FALSE)



# base plot
chordDiagram(
  
  # matrix
  rmat, 
  
  # change order of sectors
  order = c("Bayern", "Baden-Württemberg", "Saarland", "Rheinl.-Pfalz", 
            "Hessen", "Nordrhein-Westfalen", "Bremen",  "Niedersachsen", 
            "Hamburg", "Schleswig-Holstein", 
            
            "Mecklenburg-Vorpommern", "Brandenburg", "Sachsen-Anhalt", "Sachsen", "Thüringen"),
  
  # grid and link colors
  grid.col = c("#1400AC", "#4229FF", "#1D64FF", "#0092FF", "#00BDFF", 
               "#00E6D0", "#B1FF33", "#7DDB00", "#00B400", "#007D00",
    
               "#D45500", "#FF6B21", "#FF9147","#FFDB00", "#F8FF00"),
  
  # transparency of links
  transparency = 0.1,
  link.visible = visible,
  
  # determine direction of links (i.e. direction of relationship between cells)
  directional = 1,
  diffHeight = -0.04,
  
  # how directionality is represented
  direction.type = "diffHeight", 
  
  # style and order of links
  link.sort = TRUE, 
  link.decreasing = FALSE,
  link.largest.ontop = FALSE,
  
  # adjust height of all links
  h.ratio = 0.85,
  
  # defining outer tracks
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.03, 0.01)
  
)



# add text and axis
circos.trackPlotRegion( # or short: circos.track()
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) { # applies plotting to a cell. it thus requires x and y values
    
    xlim = get.cell.meta.data("xlim") # get.cell.meta.data = obtain info of current cell
    sector.index = get.cell.meta.data("sector.index") 
    
    # parameters for text 
    circos.text(
      
      # text location
      x = mean(xlim),
      y = 2.5,
      
      # names of sectors
      labels = sector.index,
      
      # facing of names
      facing = "bending",
      niceFacing = TRUE,
      
      # font size
      cex = 0.8,
      
      # font color
      col = "black"
    )
  }
)



# add annotations
mtext(
"Leipzig und Dresden
sind die beliebtesten
Regionen in Sachsen.", 
      
      # left-alignment within text box
      adj = 0, 
      
      # alignment within plot region
      at = 0.8, line = -37.5, 
      
      # font size and colour
      cex = 0.7,
      col = "#696969")

mtext(
"Aus München wandern mehr
Deutsche nach Leipzig und weniger 
nach Dresden als in die Gegenrichtung.", 
      adj = 0, at = -0.95, line = -41.1, cex = 0.7, col = "#696969")

mtext(
"Auf Kreisebene zählen Wanderungen zw. 
Hamburg und Meck-Pomm zu den größten. 
Das Wanderungssaldo ist aber 
verhältnismäßig klein.",
      adj = 0, at = -1.05, line = -4.4, cex = 0.7, col = "#696969")


# add titles
title(main = "Ost-West-Wanderung in Deutschland")
mtext("Top 25 Nettowanderungen von Deutschen im Jahr 2018 (ohne Berlin)", 
      line = -1.1, cex = 0.8)



# add sources
mtext("Daten: Statistisches Bundesamt", 
      adj = 0, at = -1.1, line = -42.8, cex = 0.7)

mtext("Visualisierung: Andreas Genoni & Nikola Sander", 
      adj = 0, at = -1.1, line = -43.6, cex = 0.7)



# save plot above as pdf (step 2)
dev.off()  




