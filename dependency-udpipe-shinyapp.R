## Vivek Suhag(11810007) and Ratna Prashanth Kumar(11810113)

try(require("shiny")||install.packages("shiny"))
try(require("udpipe")||install.packages("udpipe"))
try(require("textrank")||install.packages("textrank"))
try(require("lattice")||install.packages("lattice"))
try(require("igraph")||install.packages("igraph"))
try(require("ggraph")||install.packages("ggraph"))
try(require("wordcloud")||install.packages("wordcloud"))
try(require("stringr")||install.packages("stringr"))
try(require("ggplot2")||install.packages("ggplot2"))
try(require("dplyr")||install.packages("dplyr"))
try(require("shinydashboard")||install.packages("shinydashboard"))

library("shiny")
library("udpipe")
library("textrank")
library("lattice")
library("igraph")
library("ggraph")
library("ggplot2")
library("wordcloud")
library("stringr")
library("dplyr")
library("shinydashboard")

options(shiny.maxRequestSize = 50*1024^2)
windowsFonts(devanew=windowsFont("Devanagari new normal"))
