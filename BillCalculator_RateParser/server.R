library(shiny)
library(DT)
source("fnBillCalc_Shiny.R")

`%then%` <- shiny:::`%OR%`

shinyServer(
  function(input, output) {
    #Usage plot
    output$use <- renderPlot({
      validate(
        need(input$district, ""),
        
        need(input$usage, "-Please specify water usage") %then%
          need(input$usage < 999 & input$usage > 0, "-Please enter usage between 0 and 999 BU"),
        
        need(input$bill_days, "-Please specify the number of days in this billing cycle") %then%
          need(input$bill_days < 99 & input$bill_days > 0, "-Please enter between 0 and 99 days"),
        
        if(input$cust_class %in% c("Single Family Residential","Multi Family Residential")){
          need(input$homesize, "-Please specify your household size")} %then% 
          need(input$homesize < 99 & input$homesize > 0, "-Please enter a household size less than 99"),
        
        need(input$irr_area, "-Please specify the irrigable area associated with your account") %then%
            need(input$irr_area < 99999 & input$irr_area > 0, "-Please enter an irrigable area less than 99,999 and greater than 0"),
        
        need(input$et, "-Please specify the evapotranspiration factor associated with this bill:
             ET varies daily by microzone. There are 110 microzones within the MNWD service area.
             Your water budget is calculated based on the actual ET during the billing cycle;
             however, it is possible to estimate your water budget based on historical ET.
             The 7-year historical average ET values for the MNWD service area are:
             Jan = 2.26; Feb = 2.53; Mar=3.08; Apr=3.74;
             May=4.54; Jun=4.44; Jul=4.71; Aug=4.56;
             Sep=4.97; Oct=3.88; Nov=3.15; Dec=2.29") %then% 
          need(input$et < 99 & input$et > 0, "-Please enter an evapotranspiration factor less than 99")
        )
     
      input_df <- data.frame(
        district <- input$district,
        usage_ccf = input$usage, 
        hhsize = input$homesize,
        days_in_period = input$bill_days, 
        irr_area = input$irr_area, 
        et_amount = input$et, 
        meter_size = input$meter, 
        cust_class = input$cust_class,
        city_limits = input$city_limits,
        usage_month = input$usage_month,
        usage_zone = input$usage_zone,
        pressure_zone = input$pressure_zone,
        water_font = input$water_font,
        elevation_zone = input$elevation_zone,
        tax_exemption = input$tax_exemption,
        season = input$season,
        turbine_meter = input$turbine_meter,
        meter_type = input$meter_type,
        senior = input$senior,
        tariff_area = input$tariff_area,
        block = input$block,
        lot_size_group = input$lot_size_group,
        temperature_zone = input$temperature_zone
        #address = input$address
      )
     
      #call plots
      plotList<-fnUseByTier(input_df, tablemode = FALSE)
      plotList[[1]]
    },
    height = "auto",
    #height = 650,
    width = "auto")
    
    #Charges plot
    output$charge<-renderPlot({
      validate(
        need(input$district, ""),
        
        need(input$usage, "") %then%
          need(input$usage < 999 & input$usage > 0, ""),

        need(input$bill_days, "") %then%
          need(input$bill_days < 99 & input$bill_days > 0, ""),

        if(input$cust_class %in% c("Single Family Residential","Multi Family Residential")){
          need(input$homesize, "")} %then%
          need(input$homesize < 99 & input$homesize > 0, ""),

        need(input$irr_area, "") %then%
          need(input$irr_area < 99999 & input$irr_area > 0, ""),

        need(input$et, "") %then%
          need(input$et < 99 & input$et > 0, "")
      )
      input_df <- data.frame(
        district <- input$district,
        usage_ccf = input$usage, 
        hhsize = input$homesize,
        days_in_period = input$bill_days,
        irr_area = input$irr_area,
        et_amount = input$et,
        meter_size = input$meter, 
        cust_class = input$cust_class,
        city_limits = input$city_limits,
        usage_month = input$usage_month,
        usage_zone = input$usage_zone,
        pressure_zone = input$pressure_zone,
        water_font = input$water_font,
        elevation_zone = input$elevation_zone,
        tax_exemption = input$tax_exemption,
        season = input$season,
        turbine_meter = input$turbine_meter,
        meter_type = input$meter_type,
        senior = input$senior,
        tariff_area = input$tariff_area,
        block = input$block,
        lot_size_group = input$lot_size_group,
        temperature_zone = input$temperature_zone
      )
      #call plots
      plotList<-fnUseByTier(input_df, tablemode = FALSE)
      plotList[[2]]
    },
    height = "auto",
    #height = 650,
    width = "auto")

    #Legend
    output$legend<-renderPlot({
      #call plots
      validate(
        need(input$district, ""),
        
        need(input$usage, "") %then%
          need(input$usage < 999 & input$usage > 0, ""),

        need(input$bill_days, "") %then%
          need(input$bill_days < 99 & input$bill_days > 0, ""),

        if(input$cust_class %in% c("Single Family Residential","Multi Family Residential")){
          need(input$homesize, "")} %then%
          need(input$homesize < 99 & input$homesize > 0, ""),

        need(input$irr_area, "") %then%
          need(input$irr_area < 99999 & input$irr_area > 0, ""),

        need(input$et, "") %then%
          need(input$et < 99 & input$et > 0, "")
      )
      input_df <- data.frame(
        district <- input$district,
        usage_ccf = input$usage, 
        hhsize = input$homesize,
        days_in_period = input$bill_days,
        irr_area = input$irr_area,
        et_amount = input$et,
        meter_size = input$meter, 
        cust_class = input$cust_class,
        city_limits = input$city_limits,
        usage_month = input$usage_month,
        usage_zone = input$usage_zone,
        pressure_zone = input$pressure_zone,
        water_font = input$water_font,
        elevation_zone = input$elevation_zone,
        tax_exemption = input$tax_exemption,
        season = input$season,
        turbine_meter = input$turbine_meter,
        meter_type = input$meter_type,
        senior = input$senior,
        tariff_area = input$tariff_area,
        block = input$block,
        lot_size_group = input$lot_size_group,
        temperature_zone = input$temperature_zone
      )
      plotList<-fnUseByTier(input_df, tablemode = FALSE)
      plotList[[3]]
    })

    #call vol table
    output$vol_table <- renderDataTable({
      validate(
        need(input$district, ""),
        
        need(input$usage, "") %then%
          need(input$usage < 999 & input$usage > 0, ""),

        need(input$bill_days, "") %then%
          need(input$bill_days < 99 & input$bill_days > 0, ""),

        if(input$cust_class %in% c("Single Family Residential","Multi Family Residential")){
          need(input$homesize, "")} %then%
          need(input$homesize < 99 & input$homesize > 0, ""),

        need(input$irr_area, "") %then%
          need(input$irr_area < 99999 & input$irr_area > 0, ""),

        need(input$et, "") %then%
          need(input$et < 99 & input$et > 0, "")
      )
      input_df <- data.frame(
        district <- input$district,
        usage_ccf = input$usage, 
        hhsize = input$homesize,
        days_in_period = input$bill_days,
        irr_area = input$irr_area,
        et_amount = input$et,
        meter_size = input$meter, 
        cust_class = input$cust_class,
        city_limits = input$city_limits,
        usage_month = input$usage_month,
        usage_zone = input$usage_zone,
        pressure_zone = input$pressure_zone,
        water_font = input$water_font,
        elevation_zone = input$elevation_zone,
        tax_exemption = input$tax_exemption,
        season = input$season,
        turbine_meter = input$turbine_meter,
        meter_type = input$meter_type,
        senior = input$senior,
        tariff_area = input$tariff_area,
        block = input$block,
        lot_size_group = input$lot_size_group,
        temperature_zone = input$temperature_zone
      )
      
      
      datatable(fnUseByTier(input_df, tablemode = TRUE)
                               
                )
      
      
      # datatable(fnUseByTier(input_df, tablemode = TRUE),#extensions = 'Buttons',
      # 
      #           options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center',
      #                                                                  targets = 1:7)),dom = 't'
      #                          #buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      # 
      #           ))


    })
    # #call bill table
    # output$bill_table <- renderDataTable({
    #   validate(
    #     need(input$usage, "") %then%
    #       need(input$usage < 999 & input$usage > 0, ""),
    # 
    #     need(input$bill_days, "") %then%
    #       need(input$bill_days < 99 & input$bill_days > 0, ""),
    # 
    #     if(input$cust_class %in% c("Single Family Residential","Multi Family Residential")){
    #     need(input$homesize, "")} %then%
    #       need(input$homesize < 99 & input$homesize > 0, ""),
    # 
    #     if(input$cust_class != "Commercial Non-Irrigation"){
    #       need(input$irr_area, "") %then%
    #         need(input$irr_area < 99999 & input$irr_area > 0, "")}
    #     else{
    #       need(input$typical_usage,"") %then%
    #         need(input$typical_usage < 999 & input$typical_usage > 0, "")},
    # 
    #     need(input$et, "") %then%
    #       need(input$et < 99 & input$et > 0, "")
    #     )
    #   datatable(fnUseByTier(usage = input$usage, gpcd = 60, hh_size = input$homesize,
    #               days = input$bill_days, conv_gal2ccf = 748,
    #               lot_size = input$irr_area, plant_factor = 0.7,
    #               evapo_trans = input$et, conv_in2gal = 0.62,
    #               meter_selection = input$meter, cust_class = input$cust_class, commercial_class = input$commercial_class,
    #               tablemode = 3, typical_usage = input$typical_usage),extensions = 'Buttons',
    # 
    #   options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center',
    #                                                          targets = 1:3)),dom = 'Bfrtip',
    #                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    # 
    #   ))
    # })
    
    #Logo
    output$logo <- renderImage({
      return(list(
        src = "logo/logo.png",
        filetype = "image/png",
        alt = NULL,
        height = 120,
        width = 171
      ))
    }, deleteFile = FALSE
    )
    #call budgets
    # output$budget <- renderText({
    #   validate(
    #     need(input$cust_class, ""),
    #     need(input$usage, ""),
    #     #need(input$homesize, ""),
    #     need(input$bill_days, ""),
    #     #need(input$irr_area, ""),
    #     need(input$et, "")
    #   )      
    #   # paste("Your Current calculated water budget = ",
    #   #              fnBudget(usage = input$usage, gpcd = 60, hh_size = input$homesize,
    #   #                       days = input$bill_days, conv_gal2ccf = 748,
    #   #                       lot_size = input$irr_area, plant_factor = 0.7,
    #   #                       evapo_trans = input$et, conv_in2gal = 0.62,
    #   #                       meter_selection = input$meter, cust_class = input$cust_class),
    #   #              " BU")
    #   fnBudget(usage = input$usage, gpcd = 60, hh_size = input$homesize,
    #            days = input$bill_days, conv_gal2ccf = 748,
    #            lot_size = input$irr_area, plant_factor = 0.7,
    #            evapo_trans = input$et, conv_in2gal = 0.62,
    #            meter_selection = input$meter, cust_class = input$cust_class, typical_usage = input$typical_usage)
    # })
  })