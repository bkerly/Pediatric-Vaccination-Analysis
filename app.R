#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(kableExtra)

source(file = "r_scripts/01_libraries and functions.R")
source(file = "r_scripts/00_secrets.R")

version <- "Version 1.1: "

# Load data ---------------------------------------------------------------

user_base <- readRDS("User Base.rds")

county_tract_ids <- read_csv("data/county_tract_ids.csv",
                             show_col_types = FALSE)

all_hazard_region <- read_csv("data/CDPHE Data/AllHazardRegions.csv",
                              show_col_types = FALSE) 

options(tigris_use_cache = TRUE)

suppressMessages({
    geom_with_age <- get_acs(
        geography = "tract",
        state = "CO",
        variables = c(
            Under_18 = "B09001_001"
            
        ),
        year = 2017,
        geometry = TRUE
    ) %>%
        mutate(TractID = as.numeric(GEOID)) %>%
        mutate(Under_18 = estimate) %>%
        select(TractID, NAME,Under_18,geometry) %>%

        sf::st_transform('+proj=longlat +datum=WGS84')
}
)

predictions <- read_csv("data/model predictions.csv",
                        show_col_types = FALSE)

Pediatric_Vaccine_Index <- left_join(geom_with_age, 
                                     read_csv("data/Pediatric_Vaccine_Index.csv",
                                    show_col_types = FALSE) )%>%
    left_join(county_tract_ids) %>%
    
    left_join(predictions) %>%
    mutate(across(Belief_pct:PVI_overall,function(x){x*100}))%>%
    mutate(model_error = PERCENT_VAX_0_17-prediction)


Colorado_data_mr <- read_csv("data/Colorado_data_mr.csv",
                             show_col_types = FALSE) %>%
    left_join(county_tract_ids) %>%
    left_join(geom_with_age)%>%
    left_join(predictions) 


county_data <- read_csv(
    sprintf("https://docs.google.com/uc?id=%s&export=download",
            "1cjLn3Qkvs8hNJDGzCuqrDzbeNechUmkW"
    ),
    show_col_types = FALSE
) %>% mutate(
    COUNTY = toupper(County)
) 



# UI ----------------------------------------------------------------------


# Choices for drop-downs
vars <- c(
    "Overall Pediatric Vaccination Rate" = "PERCENT_VAX_0_17",
    "Predicted Vaccination Rate" = "prediction",
    "Vaccine Belief" = "Belief_pct",
    "Perceived Vulnerability" = "Per_Vul_pct",
    "Healthcare Access" = "HC_access_pct",
    "Socioeconomic Advantage" = "SE_Disadv_pct",
    "Model Error" = "model_error"
)

# Define UI piecewise
header <- dashboardHeader(
    title = "Pediatric COVID Vaccination",
    titleWidth = 300
)

body <- dashboardBody(
    
    # login section
    shinyauthr::loginUI(id = "login"),
    
    
    fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("coloradomap", height = 500)
               ),
                box(width = NULL,
                    p(
                        class = "text-muted",
                        paste("These are the percentile rankings for each of the ",
                              "four vaccine determinant domains examined"
                        )
                    ), #/ p
                    plotOutput("metricgraph")
                )
        ), #/ column
        column(width = 3,
               
               box(width = NULL, status = "danger",
                   align = "center",
                   
                   # logout button
                  shinyauthr::logoutUI(id = "logout")
                   
              
               ), #/ logout box
               
               box(width = NULL, status = "warning",
                   selectizeInput(label = "Choose metric to display",
                               inputId = "metric_select",
                               choices = c(
                                   "Overall Pediatric Vaccination Rate",
                                   "Predicted Vaccination Rate",
                                   "Vaccine Belief",
                                   "Perceived Vulnerability",
                                   "Healthcare Access",
                                   "Socioeconomic Advantage",
                                   "Model Error"
                               ),
                               options = list(
                                   placeholder = 'Please select an option below',
                                   onInitialize = I('function() { this.setValue(""); }')
                               )
                               ),
                   p(
                       class = "text-muted",
                       paste("This is the predicted and observed vaccination rate ",
                             "for the selected census tract."
                       )
                   ), #/ p
                   plotOutput("obspredgraph")
               ) #/ box
        )#/column
    ) #/ fluidrow
) #/ dashboard body

ui <- dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)

# Server ------------------------------------------------------------------



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ### Server Authorization bit------
    credentials <- shinyauthr::loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        sodium_hashed = TRUE,
        log_out = reactive(logout_init())
    )
    
    # Logout to hide
    logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
    )

    output$coloradomap <- renderLeaflet({
        
        # Show only when authenticated
        req(credentials()$user_auth)
        
        
       

        # draw the histogram with the specified number of bins
        leaflet(Pediatric_Vaccine_Index) %>%
            addTiles() %>%
            fitBounds(~min(-109.060253), ~min(36.992426), ~max(-102.041524), ~max(	41.003444))
    }) #/ output#coloradomap
    
    # Change selected metric
    observe({
        mapInput <- switch(input$metric_select,
                           "Overall Pediatric Vaccination Rate" = Pediatric_Vaccine_Index$PERCENT_VAX_0_17,
                           "Predicted Vaccination Rate" = Pediatric_Vaccine_Index$prediction,
                           "Vaccine Belief" = Pediatric_Vaccine_Index$Belief_pct,
                           "Perceived Vulnerability" = Pediatric_Vaccine_Index$Perc_Vuln_pct,
                           "Healthcare Access" = Pediatric_Vaccine_Index$HC_access_pct,
                           "Socioeconomic Advantage" = Pediatric_Vaccine_Index$SE_Disadv_pct,
                           "Model Error" = Pediatric_Vaccine_Index$model_error)
        
        legend_text <- input$metric_select %>% as.character()
        
        
        # generate color palette
        mybins <- case_when(input$metric_select == "Model Error" ~ c(-75,-50,-25,-10,10,25,50,75),
                            TRUE ~ c(0,10,20,30,40,50,75,100)
        )
        mypalette <- colorBin(palette="viridis", 
                              domain=mapInput, 
                              na.color="transparent", bins=mybins)
        
        mytext <- paste(
            Pediatric_Vaccine_Index$COUNTY, " COUNTY","<br/>", 
            "TractID: ", Pediatric_Vaccine_Index$TractID,"<br/>", 
            "Vaccination Rate: ", round(Pediatric_Vaccine_Index$PERCENT_VAX_0_17,0),"%","<br/>", 
            "Predicted Vaccination Rate: ", round(Pediatric_Vaccine_Index$prediction,0),"%",
            sep = ""
        ) %>%
            lapply(htmltools::HTML)
        
        proxy <- leafletProxy("coloradomap", data = Pediatric_Vaccine_Index) %>%
            clearShapes() %>% # removes polygons
            clearControls() %>% # remvoes legend
        
                addPolygons(
            layerId = Pediatric_Vaccine_Index$TractID,
            fillColor = ~mypalette(mapInput), 
            stroke = TRUE,
            fillOpacity = 0.5,
            color = "black",
            weight = 0.3,
            label = mytext,
            labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
            ) #/ label options
              
              ) %>% #/ addPolygons
        addLegend(pal = mypalette,
                   title = legend_text,
                   values = mapInput,
                   opacity = 0.5
        )
        
    })
    
    
    output$metricgraph <- renderPlot({
        
        # Show only when authenticated
        req(credentials()$user_auth)
        
        req(input$coloradomap_shape_click, cancelOutput = FALSE)
        
        event <- input$coloradomap_shape_click
        
            Pediatric_Vaccine_Index %>%
                as.data.frame() %>%
                 filter(TractID == event$id) %>%
                 select(Belief_pct:SE_Disadv_pct,-geometry) %>%
                pivot_longer(everything()) %>%
                mutate(name = factor(name,
                                     levels = c(
                                         "Belief_pct",
                                         "Perc_Vuln_pct",
                                         "HC_access_pct",
                                         "SE_Disadv_pct"
                                     ),
                                     labels = c(
                                         "Vaccine Belief",
                                         "Perceived Vulnerability" ,
                                         "Healthcare Access",
                                         "Socioeconomic Advantage"
                                     )
                                     )) %>%
                ggplot(aes(x=name,y=value)) +
                geom_bar(stat="identity",fill="#fde725",alpha=.6, width=.4)+
                ylim(0,100)+
                coord_flip()+
                xlab("") +
                ylab("")+
                theme_bw()
    }) #/ renderplot
    
    output$obspredgraph <- renderPlot({
        
        # Show only when authenticated
        req(credentials()$user_auth)
        
        req(input$coloradomap_shape_click, cancelOutput = FALSE)
        event <- input$coloradomap_shape_click
        
        Pediatric_Vaccine_Index %>%
            as.data.frame() %>%
            filter(TractID == event$id) %>%
            select(prediction,PERCENT_VAX_0_17) %>%
            pivot_longer(everything()) %>%
            mutate(name = factor(name,
                                 levels = c(
                                     "PERCENT_VAX_0_17",
                                     "prediction"
                                 ),
                                 labels = c(
                                     "Actual Vaccination Rate",
                                     "Predicted Vaccination Rate"
                                 )
            )) %>%
            ggplot(aes(x=name,y=value)) +
            geom_bar(stat="identity",fill="#3b528b",alpha=.6, width=.4)+
            ylim(0,100)+
            xlab("") +
            ylab("")+
            theme_bw()
    }) #/ renderplot
} # / output


# Run ---------------------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)


# Deploy app ---------------------------------------------------------------

#rsconnect::setAccountInfo(name='xx', token='xx', secret='xx/xx')
#library(rsconnect)

# deployApp(account = "cdphe-data", appName = "PedsVax", launch.browser = T, forceUpdate = T)

# Currently generates error:
# # Error: Unhandled Exception: Child Task 1240883183 failed: Error building image: Error fetching ciisr (0.1.0) source. Error accessing GitHub repository leighseverson/ciisr. HTTP 404: Not Found

