library(shiny)
library(DT)
source("R/fnBillCalc_Shiny.R")

`%then%` <- shiny:::`%OR%`

ET <- data.frame(Months = month.name,ETF = c(2.26,2.53,3.08,3.74,4.54,4.44,4.71,4.56,4.97,3.88,3.15,2.29))
cust_class <- "RESIDENTIAL_SINGLE"


shinyServer(
  function(input, output) {
    
    # Geocode address. Depends on entering address and clicking "Update" button.
    # geocode_data <- eventReactive(input$go,{
    #     # validate(need(input$address, "-Please specify address"))
    #     coords <- geocode(input$address, source = "dsk", output = "latlon")
    #     # coords <- data.frame("lon"=-118.251081, "lat"=34.044789)
    #     
    #     # sleep for short period then give one extra try in the case of over-limit geocoding
    #     if(is.na(coords$lon) || is.na(coords$lat)){
    #       Sys.sleep(0.5)
    #       coords <- geocode(input$address, source = "dsk", output = "latlon")
    #     }
    #     
    #     if(is.na(coords$lon) || is.na(coords$lat)){
    #       NULL
    #     }else{
    #       coords
    #     }
    #   }
    # )
    
    # Spatial join the geocoded address points with the Water District boundary shapefile
    # After finding the distict, read the OWRS file for that district and return it
    # Depends on geocode_data
    
    # Read OWRS file of a specific utility  
    owrs_file <- function() {
      owrs_file <- read_owrs_file("California/Santa Monica City of - 364/smc-2017-01-01.owrs")
      return(owrs_file)
    }
    # owrs_file <- eventReactive(input$go,{
    #   
    #   if(!is.null(geocode_data())){
    #     districtshp <- st_read("./shp/water_district.shp")
    #     point <- st_point(c(geocode_data()$lon, geocode_data()$lat))
    #     districts_within <- districtshp %>% 
    #       filter(row_number() %in% st_within(point, districtshp)[[1]] ) %>%
    #       mutate(area = st_area(geometry)) %>%
    #       arrange(area)
    #     agency_name <- as.character(districts_within$Agency_Nam)[1]
    #     filePath <- Sys.glob(file.path("California", paste0(agency_name, '*', collapse ='')))
    #     
    #     #TODO if multiple files are available only return the  most recent one
    #     fileName <- list.files(filePath, pattern="*.owrs", full.name=TRUE)
    #     if (length(fileName) >= 1){
    #       fileName <- tail(fileName, n=1)
    #     }
    #     
    #     if (length(fileName) >= 1){
    #       owrs_file <- read_owrs_file(fileName)
    #     }else{
    #       owrs_file <- NULL
    #       showModal(modalDialog(
    #         title = "Pricing data not found.",
    #         paste("Sorry, we were unable to find water pricing information for the address '",
    #               input$address, 
    #               "'. We might not have pricing information for your district yet!"),
    #         easyClose = TRUE
    #       ))
    #     }
    #     
    #   }else{
    #     owrs_file <- NULL
    #     showModal(modalDialog(
    #       title = "Unable to locate address.",
    #       paste("Sorry, we were unable to map the address you entered '",
    #             input$address, 
    #             "' to a location on the Earth. Maybe you mistyped something?"),
    #       easyClose = TRUE
    #     ))
    #   }
    #   
    #   
    #   
    #  
    #   
    #   
    #   owrs_file
    # })
    
    # output the name of the select water district
    # Depends on owrs_file
    output$district <- renderText({
      owrs_file()$metadata$utility_name
    })
    
    # Assemble the inputs into a dataframe and create plots
    # Depends on all inputs
    use_by_tier_data <- reactive({
      
      # req(input[["usage"]])
      
      widget_values <- list()
      owrs_str <- jsonlite::toJSON(owrs_file()$rate_structure[[cust_class]])
      # rate_options <- c("hhsize", "irr_area", "days_in_period", "et_amount", "meter_size", "city_limits",
      #                   "usage_month", "usage_zone", "pressure_zone", "elevation_zone", "lot_size_group",
      #                   "temperature_zone", "senior","water_font", "tax_exemption", "meter_type", "season",
      #                   "tariff_area", "block")
      
      widget_values[["usage_ccf"]] <- input[["usage"]]
      widget_values[["cust_class"]] <- cust_class
      
      if(grepl("hhsize", owrs_str)){
        widget_values[["hhsize"]] <- input[["hhsize"]]
      }
      if(grepl("irr_area", owrs_str)){
        widget_values[["irr_area"]] <- input[["irr_area"]]
      }
      if(grepl("days_in_period", owrs_str)){
        widget_values[["days_in_period"]] <- input[["days_in_period"]]
      }
      if(grepl("et_amount", owrs_str)){
        widget_values[["et_amount"]] <- input[["et_amount"]]
      }
      if(grepl("meter_size", owrs_str)){
        widget_values[["meter_size"]] <- input[["meter_size"]]
      }
      if(grepl("city_limits", owrs_str)){
        widget_values[["city_limits"]] <- input[["city_limits"]]
      }
      if(grepl("usage_month", owrs_str)){
        widget_values[["usage_month"]] <- input[["usage_month"]]
      }
      if(grepl("usage_zone", owrs_str)){
        widget_values[["usage_zone"]] <- input[["usage_zone"]]
      }
      if(grepl("pressure_zone", owrs_str)){
        widget_values[["pressure_zone"]] <- input[["pressure_zone"]]
      }
      if(grepl("elevation_zone", owrs_str)){
        widget_values[["elevation_zone"]] <- input[["elevation_zone"]]
      }
      if(grepl("lot_size_group", owrs_str)){
        widget_values[["lot_size_group"]] <- input[["lot_size_group"]]
      }
      if(grepl("temperature_zone", owrs_str)){
        widget_values[["temperature_zone"]] <- input[["temperature_zone"]]
      }
      if(grepl("senior", owrs_str)){
        widget_values[["senior"]] <- input[["senior"]]
      }
      if(grepl("water_font", owrs_str)){
        widget_values[["water_font"]] <- input[["water_font"]]
      }
      if(grepl("tax_exemption", owrs_str)){
        widget_values[["tax_exemption"]] <- input[["tax_exemption"]]
      }
      if(grepl("meter_type", owrs_str)){
        widget_values[["meter_type"]] <- input[["meter_type"]]
      }
      if(grepl("season", owrs_str)){
        widget_values[["season"]] <- input[["season"]]
      }
      if(grepl("tariff_area", owrs_str)){
        widget_values[["tariff_area"]] <- input[["tariff_area"]]
      }
      if(grepl("block", owrs_str)){
        widget_values[["block"]] <- input[["block"]]
      }
      
      input_df <- data.frame(widget_values)
      
      # input_df <- data.frame(
      #   usage_ccf = input$usage, 
      #   hhsize = input$homesize,
      #   days_in_period = input$bill_days, 
      #   irr_area = input$irr_area, 
      #   et_amount = input$et, 
      #   meter_size = input$meter,
      #   cust_class = cust_class
      #   # city_limits = input$city_limits,
      #   # usage_month = input$usage_month,
      #   # usage_zone = input$usage_zone,
      #   # pressure_zone = input$pressure_zone,
      #   # water_font = input$water_font,
      #   # elevation_zone = input$elevation_zone,
      #   # tax_exemption = input$tax_exemption,
      #   # season = input$season,
      #   # turbine_meter = input$turbine_meter,
      #   # meter_type = input$meter_type,
      #   # senior = input$senior,
      #   # tariff_area = input$tariff_area,
      #   # block = input$block,
      #   # lot_size_group = input$lot_size_group,
      #   # temperature_zone = input$temperature_zone
      # )
      
      #call plots
      if( (nrow(input_df) > 0) && !is.null(owrs_file()) ){
        plotList<-fnUseByTier(input_df, owrs_file())
        plotList
      }
      else{
        list(NULL,NULL,NULL,NULL)
      }
      
    })
    
    # Generate those inputs that are required by the OWRS file
    # depends on owrs_file
    output$rateInputs <- renderUI({
      owrs_str <- jsonlite::toJSON(owrs_file()$rate_structure[[cust_class]])
      widgetList <- list()
      
      widgetList <- append(widgetList,
                           tagList(h4(""),icon("tint"),
                                   h5(paste(owrs_file()$metadata$bill_frequency,"Billing Units Used") ),
                                   h6("(1 BU = 748 gallons)"),
                                   numericInput("usage", label = NULL, value = 15, min = 0, max = 999) )
      )
      
      if(grepl("hhsize", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("users"),
                                     h5("Number of Persons in Household"),
                                     numericInput("hhsize", label = NULL, value = 4, min = 0, max = 99))
        )
      }
      
      if(grepl("irr_area", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("tree"),
                                     h5("Irrigable Area (sq. ft.)"),
                                     numericInput("irr_area", label = NULL, value = 2000, min = 0, max = 99999))
        )
      }
      
      if(grepl("days_in_period", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("calendar"),
                                     h5("Days in Billing Cycle"),
                                     h6("(Typically 28-35 days)"),
                                     numericInput("days_in_period", label = NULL, value = 30, min = 0, max = 99) )
        )
      }
      
      if(grepl("et_amount", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),icon("leaf"),
                                     h5("Evapotranspiration"),
                                     h6("(This month's average ET - ",ET$ETF[ET$Months == months.Date(Sys.Date())],")"),
                                     numericInput("et_amount", label = NULL, value = 5, min = 0, max = 99) )
        )
      }
      
      if(grepl("meter_size", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Meter Size"),
                                     h6("(Typically 3/4 in. for residential customers)"),
                                     selectInput("meter_size", 
                                                 label = NULL,
                                                 choices = c("5/8 in." = '5/8"', "3/4 in." = '3/4"',
                                                             "1 in." = '1"', "1 1/2 in." = '1 1/2"',
                                                             "2 in." = '2"', "3 in." = '3"',
                                                             "4 in." = '4"', "6 in." = '6"',
                                                             "8 in." = '8"', "10 in." = '10"'),
                                                 selected = '3/4"'),
                                     textOutput("budget") )
        )
      }
      
      if(grepl("city_limits", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Are you located within the city limits?"),
                                     selectInput("city_limits", 
                                                 label = NULL, 
                                                 choices = c("inside_city","outside_city"),
                                                 selected = 'inside_city' ) )
        )
      }
      
      if(grepl("usage_month", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Month (1-12)"),
                                     numericInput("usage_month", 
                                                  label = NULL,
                                                  value = as.integer(format(Sys.Date(),"%m")), 
                                                  min = 1, max = 12) )
        )
      }
      
      if(grepl("usage_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Usage Zone"),
                                     numericInput("usage_zone", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 5) )
        )
      }

      if(grepl("pressure_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Pressure Zone"),
                                     numericInput("pressure_zone", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 4) )
        )
      }
      
      if(grepl("elevation_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Elevation Zone"),
                                     numericInput("elevation_zone", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 6) )
        )
      }
      
      if(grepl("lot_size_group", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Lot Size Group (1-5"),
                                     numericInput("lot_size_group", 
                                                  label = NULL,
                                                  value = 1, 
                                                  min = 1, max = 6) )
        )
      }

      if(grepl("temperature_zone", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Temperature Zone"),
                                     selectInput("temperature_zone", 
                                                 label = NULL, 
                                                 selected = "Medium",
                                                 choices = c("Low","Medium","High")) )
        )
      }

      if(grepl("senior", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Senior citizen?"),
                                     selectInput("senior", 
                                                 label = NULL, 
                                                 selected = "no",
                                                 choices = c("yes","no")) )
        )
      }

      if(grepl("water_font", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("What is your water source?"),
                                     selectInput("water_font", 
                                                 label = NULL, 
                                                 selected = "city_delivered",
                                                 choices = c("city_delivered","private_wells")) )
        )
      }
      
      if(grepl("tax_exemption", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Tax exempt?"),
                                     selectInput("tax_exemption", 
                                                 label = NULL, 
                                                 selected = "not_granted",
                                                 choices = c("granted","not_granted")) )
        )
      }
      
      if(grepl("meter_type", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Meter Type"),
                                     selectInput("meter_type", 
                                                 label = NULL, 
                                                 selected = "Displacement",
                                                 choices = c("compound","FM", "Turbine", "Displacement")) )
        )
      }
      
      if(grepl("season", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Season"),
                                     selectInput("season", 
                                                 label = NULL, 
                                                 selected = "Summer",
                                                 choices = c("Summer","Winter")) )
        )
      }

      if(grepl("tariff_area", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Tariff Area"),
                                     selectInput("tariff_area", 
                                                 label = NULL, 
                                                 selected = 1,
                                                 choices = c(1,2,3)) )
        )
      }
      
      if(grepl("block", owrs_str)){
        widgetList <- append(widgetList,
                             tagList(h4(""),
                                     h5("Block"),
                                     selectInput("block", 
                                                 label = NULL, 
                                                 selected = 1,
                                                 choices = c(1,2)) )
        )
      }

      
      tagList(widgetList)
    })
    
    #Usage plot
    output$use <- renderPlot({
      use_by_tier_data()[[1]]
    },
    height = "auto",
    #height = 650,
    width = "auto"
    )
    
    #Charges plot
    output$charge<-renderPlot({
      use_by_tier_data()[[2]]
    },
    height = "auto",
    #height = 650,
    width = "auto"
    )

    #Legend
    output$legend<-renderPlot({
      use_by_tier_data()[[3]]
    })

    #call vol table
    output$vol_table <- renderDataTable({
      datatable(use_by_tier_data()[[4]])
      
      
      # datatable(fnUseByTier(input_df, tablemode = TRUE),#extensions = 'Buttons',
      # 
      #           options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center',
      #                                                                  targets = 1:7)),dom = 't'
      #                          #buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      # 
      #           ))
    })
    
    
    #Logo
    output$logo <- renderImage({
      return(list(
        src = "logo/santa_monica.jpg",
        filetype = "image/png",
        alt = NULL,
        height = 120,
        width = 171
      ))
    }, deleteFile = FALSE
    )
    
  })
