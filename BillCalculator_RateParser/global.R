library(shiny)
library(DT)
source("fnBillCalc_Shiny.R")

`%then%` <- shiny:::`%OR%`

DistrictList <- list.files("California")