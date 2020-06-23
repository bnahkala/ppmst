# AUTHOR: BRADY NAHKALA
# LAST REVISED: 22 June 2020
# LICENSE =====

# Copyright (C)
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# LIBRARY =====================================

# SHINY LIBRARY
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinyalert)

# mapping
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(mapview)
library(raster)
library(rgdal)
library(sp)
library(XML)
# library(webshot)
library(htmlwidgets)

# utility
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)

# STATIC DATA/FUNCTIONS =====
source("input_dfs.R")
source("rF_app.R")
# source("rf_tree.R") - CURRENT TREE IS FROM OUTDATED RF MODEL

counties <- raster::shapefile("./shp/county.shp")
counties <- spTransform(counties, "+init=epsg:4326")

# user interface ===============================
ui <-
  navbarPage(
    "Prairie Pothole Management Support Tool",
    theme = shinytheme("united"),
    
       
    # BASELINE TOOL ============================================
    tabPanel(
      "Baseline Assessment",
      dashboardPage(
        dashboardHeader(disable = TRUE),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
          # INSTRUCTIONS =====
          fluidRow(
            box(
              solidHeader = T,
              collapsible = T,
              collapsed = TRUE,
              title = "User Manual",
              status = "danger",
              height = NULL,
              width = 12,
              background = NULL,
              column(
                12,
                htmlOutput("frame")
              ),
            )
          ), 
          # SIDEBAR INPUTS ===========================
          fluidRow(
            sidebarLayout(
              sidebarPanel(
                radioButtons(
                  inputId = "pred.model",
                  label = "Prediction Model",
                  choices = list(
                    # "Representative Decision Tree",
                    "Random Forest Ensemble"
                  )
                ),
                textInput(
                  inputId = "cnty",
                  label = "1. Enter your County",
                  value = "Story"
                ),
                textInput(inputId = "field", label = "2. Enter a personal field ID", "MyFieldID"),
                radioButtons(
                  inputId = "lulc.pothole",
                  label = "3. What is the land use of the pothole?",
                  choices = list(
                    "Corn-Soybean Rotation",
                    "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                  )
                ),
                radioButtons(
                  inputId = "lulc.field",
                  label = "4. What is the land use of the field?",
                  choices = list(
                    "Corn-Soybean Rotation",
                    "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                  )
                ),
                selectInput(
                  inputId = "Drainage",
                  label = "5. Specify existing drainage",
                  choices = list(
                    "No drainage",
                    "Subsurface drain",
                    "Surface inlet to subsurface drain",
                    "Multiple inlets to subsurface drain"
                  ),
                  selected = "No drainage"
                ),
                selectInput(
                  inputId = "Tillage",
                  label = "6. Specify current tillage",
                  choices = list("Conventional", "Conservation", "No Till", "NA-Retired"),
                  selected = "Conventional"
                ),
                sliderInput(
                  "area_estimate",
                  "Decimal:",
                  label = "7. Estimate the maximum land extent of the pothole (ha)",
                  value = 1.3,
                  min = 0,
                  max = 10,
                  step = 0.1
                ),
                sliderInput(
                  "h2oshed_estimate",
                  "Decimal:",
                  label = "8. Estimate the contributing flow area to the pothole (ha)",
                  value = 8.1,
                  min = 1,
                  max = 200,
                  step = 0.1
                ),
                sliderInput(
                  "max.depth",
                  "Decimal:",
                  label = "9. Estimate the maximum flooding depth of the pothole (m)",
                  value = 0.5,
                  min = 0,
                  max = 5,
                  step = 0.1
                ),
                sliderInput(
                  "watershed.slope",
                  "10. Estimate the watershed slope as a percent (approximate with field slope if necessary).",
                  value = 1.0,
                  min = 0,
                  max = 15,
                  step = 0.1
                ),
                radioButtons(
                  inputId = "field.position",
                  label = "11. Specify the general shape of the watershed.",
                  choices = list(
                    "Circular",
                    "Ellipsoidal"
                  )
                ),
                actionButton(inputId = "SaveDefaults", label = "Copy to Analysis")
              ),
              
              # MAP PANEL ===============================
              mainPanel(
                column(
                  10,
                  offset = 1,
                  fluidRow(
                    style = "height:800px; background-color white;",
                    leafletOutput(outputId = "mymap", height = 800),
                    # POTENTIAL FUNCTIONALITY TO SCREENSHOT THE MAP AND SAVE TO REPORT ====
                    downloadButton(outputId = "MapView", label = "Save Map View")
                    # ====
                  )
                  )
                )
              )
            ), 
            # FOOTER ======
          fluidRow(
            style = "background-color:#f3f3f3;",
            column(
              4,
              h1(),
              tags$div(
                p("Prairie Pothole Management Support Tool  Copyright (C) 2020"),
                p(
                  "This program comes with ABSOLUTELY NO WARRANTY.
            This is free software, and you are welcome to redistribute it
            under certain conditions. See license documentation for details."
                )
              ),
              h1(),
            ),
            column(
              4,
              h1(),
              tags$div(
                tags$strong("Department of Agricultural and Biosystems Engineering"),
                tags$br(),
                "1340 Elings Hall",
                tags$br(),
                "605 Bissell Road",
                tags$br(),
                "Ames, IA, 50011-3270"
              )
            ),
            column(4,
                   h1(),
                   tags$strong(
                     tags$a(
                       href = "PPMST_Manual_v1.pdf",
                       "PPMST User Manual",
                       align = "center",
                       target = "_blank"
                     )
                   ),
                   # tags$div(
                   #   "If you have questions about the status",
                   #   br(),
                   #   "of the PPMST, contact:",
                   #   br(),
                   #   "kaleita@iastate.edu"
                   # ),
                   h1())
          )
          )
        )
      
      ), 
 
      # ALTERNATIVE ANALYSIS ======================================
    tabPanel(
      "Analysis",
      dashboardPage(
        skin = "red",
        dashboardHeader(disable = TRUE),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
          # CONSTANTS ====
          fluidRow(
            box(
              solidHeader = T,
              collapsible = T,
              collapsed = T,
              title = "Pothole Values Saved from Baseline Assessment",
              status = "danger",
              height = NULL,
              width = 12,
              column(
                width = 12,
                sliderInput(
                  "area_estimate1",
                  "Decimal:",
                  label = "Estimate the maximum land extent of the pothole (ha)",
                  value = 1.4,
                  min = 0,
                  max = 10,
                  step = 0.1
                ),
                sliderInput(
                  "h2oshed_estimate1",
                  "Decimal:",
                  label = "Estimate the contributing flow area to the pothole (ha)",
                  value = 8.1,
                  min = 1,
                  max = 200,
                  step = 0.1
                ),
                sliderInput(
                  "max.depth1",
                  "Decimal:",
                  label = "Estimate the maximum flooding depth of the pothole (m)",
                  value = 0.5,
                  min = 0,
                  max = 5,
                  step = 0.1
                ),
                sliderInput(
                  "watershed.slope1",
                  "Estimate the watershed slope as a percent (approximate with field slope if necessary).",
                  value = 1.0,
                  min = 0,
                  max = 15,
                  step = 0.1
                )
              )
            )
          ),
          # BASELINE ANALYSIS =====
          fluidRow(
            box(
              solidHeader = T,
              collapsible = T,
              collapsed = F,
              title = " Baseline Analysis",
              status = "danger",
              height = NULL,
              width = 12, 
              column(3,
                     uiOutput("riskicon", width="100%", height = "200px")
                     ),
              column(2, 
                     h3("Baseline Risk:"),
                     h1(textOutput("predictionDUP2"))), 
              column(7, 
                     htmlOutput("risksum"))
            )
          ),
          # MANIPULATE ALTERNATIVES =====
          fluidRow(
            box(
              solidHeader = T,
              collapsible = T,
              collapsed = T,
              title = "Learn How Management Can Affect Your Pothole (Alternative Analysis)",
              status = "danger",
              height = NULL,
              width = 12,
              div(fixedRow(
                column(2,
                       plotOutput("barplot")),
                column(
                  width = 2,
                  h3("Baseline Risk:"),
                  h1(textOutput("predictionDUP")),
                  br(),
                  tableOutput("baseinputs")
                ),
                column(2,
                       em(h3(textOutput("lab1"))),
                       em(h1(
                         textOutput("prediction1")
                       ))#,
                       # plotOutput("days1")
                       ),
                column(2,
                       em(h3(textOutput("lab2"))),
                       em(h1(
                         textOutput("prediction2")
                       ))#,
                       # plotOutput("days2")
                       ),
                column(2,
                       em(h3(textOutput("lab3"))),
                       em(h1(
                         textOutput("prediction3")
                       ))#,
                       # plotOutput("days3")
                       ),
                column(2,
                       em(h3(textOutput("lab4"))),
                       em(h1(
                         textOutput("prediction4")
                       ))#,
                       # plotOutput("days4")
                       )
              )),
              div(
                style = "padding: 0px 0px; margin-top:0%",
                tags$hr(style = "border-color: red;"),
                # ALTERNATIVE 1 =====
                column(
                  width = 3,
                  textInput(
                    inputId = "AltID1",
                    label = "Name your scenario:",
                    value = "Option 1"
                  ),
                  radioButtons(
                    inputId = "lulc.pothole1",
                    label = "What is the land use of the pothole?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  radioButtons(
                    inputId = "lulc.field1",
                    label = "What is the land use of the field?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  selectInput(
                    inputId = "Drainage1",
                    label = "Specify existing drainage",
                    choices = list(
                      "No drainage",
                      "Subsurface drain",
                      "Surface inlet to subsurface drain",
                      "Multiple inlets to subsurface drain"
                    )
                  ),
                  selectInput(
                    inputId = "Tillage1",
                    label = "Specify current tillage",
                    choices = list("Conventional", "Conservation", "No Till", "NA-Retired")
                  )
                ),
                # ALTERNATIVE 2 =====
                column(
                  width = 3,
                  textInput(
                    inputId = "AltID2",
                    label = "Name your scenario:",
                    value = "Option 2"
                  ),
                  radioButtons(
                    inputId = "lulc.pothole2",
                    label = "What is the land use of the pothole?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  radioButtons(
                    inputId = "lulc.field2",
                    label = "What is the land use of the field?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  selectInput(
                    inputId = "Drainage2",
                    label = "Specify existing drainage",
                    choices = list(
                      "No drainage",
                      "Subsurface drain",
                      "Surface inlet to subsurface drain",
                      "Multiple inlets to subsurface drain"
                    )
                  ),
                  selectInput(
                    inputId = "Tillage2",
                    label = "Specify current tillage",
                    choices = list("Conventional", "Conservation", "No Till", "NA-Retired")
                  )
                ),
                # ALTERNATIVE 3 =====
                column(
                  width = 3,
                  textInput(
                    inputId = "AltID3",
                    label = "Name your scenario:" ,
                    value = "Option 3"
                  ),
                  radioButtons(
                    inputId = "lulc.pothole3",
                    label = "What is the land use of the pothole?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  radioButtons(
                    inputId = "lulc.field3",
                    label = "What is the land use of the field?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  selectInput(
                    inputId = "Drainage3",
                    label = "Specify existing drainage",
                    choices = list(
                      "No drainage",
                      "Subsurface drain",
                      "Surface inlet to subsurface drain",
                      "Multiple inlets to subsurface drain"
                    )
                  ),
                  selectInput(
                    inputId = "Tillage3",
                    label = "Specify current tillage",
                    choices = list("Conventional", "Conservation", "No Till", "NA-Retired")
                  )
                ),
                # ALTERNATIVE 4 =====
                column(
                  width = 3,
                  textInput(
                    inputId = "AltID4",
                    label = "Name your scenario:",
                    value = "Option 4"
                  ),
                  radioButtons(
                    inputId = "lulc.pothole4",
                    label = "What is the land use of the pothole?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  radioButtons(
                    inputId = "lulc.field4",
                    label = "What is the land use of the field?",
                    choices = list(
                      "Corn-Soybean Rotation",
                      "Perennial Cover (Conservation Reserve, Grassed, etc.)"
                    )
                  ),
                  selectInput(
                    inputId = "Drainage4",
                    label = "Specify existing drainage",
                    choices = list(
                      "No drainage",
                      "Subsurface drain",
                      "Surface inlet to subsurface drain",
                      "Multiple inlets to subsurface drain"
                    )
                  ),
                  selectInput(
                    inputId = "Tillage4",
                    label = "Specify current tillage",
                    choices = list("Conventional", "Conservation", "No Till", "NA-Retired")
                  )
                )
              ),
              
              # EXECUTE ANALYSIS
              fluidRow(column(
                width = 12,
                downloadButton("Report", label = "Save Report", style = "float:right")
              ))
            )
          ),
          # DATA SUPPORTING THE RISK LEVEL =====
          fluidRow(
            box(
              solidHeader = T,
              collapsible = T,
              collapsed = TRUE,
              title = "The Data Behind the Ranking System",
              status = "danger",
              height = NULL,
              width = 12,
              p(
                "The ranking system presented follows a 10-point scale, with 10 representing
                             the highest risk and 0 representing the lowest risk of flooding."
              ),
              p(
                "The following table interprets the ranking system in terms of actual data.
                             The risk value assigned by the model (left column) is interpreted in the
                             context of flood statistics, data which came from watershed modeling.
                             These interpretations describe general trends, but there are cases where potholes
                             do not follow these trends exactly, based on their unique characteristics. Do not
                             be concerned by potholes that do not follow these trends exactly."
              ),
              tableOutput("risk_interpretation")
            )
          ),
          # FUTURE OUTPUT ====
          # fluidRow(
          #   box(
          #     solidHeader = T,
          #     collapsible = T,
          #     collapsed = T,
          #     title = "Economic Summary",
          #     status = "danger",
          #     height = NULL,
          #     width = 12,
          #     em("In Development")
          #   )
          # ),
          # fluidRow(
          #   box(
          #     solidHeader = T,
          #     collapsible = T,
          #     collapsed = T,
          #     title = "Environmental Summary",
          #     status = "danger",
          #     height = NULL,
          #     width = 12,
          #     em("In Development")
          #   )
          # ),
          # FOOTER ======
          fluidRow(
            style = "background-color:#f3f3f3;",
            column(4,
                   h1(),
                   tags$div(
                     p("Prairie Pothole Management Support Tool  Copyright (C) 2020"),
                     p(
                       "This program comes with ABSOLUTELY NO WARRANTY.
            This is free software, and you are welcome to redistribute it
            under certain conditions. See license documentation for details."
                     )
                   ),
                   h1(),),
            column(
              4,
              h1(),
              tags$div(
                tags$strong("Department of Agricultural and Biosystems Engineering"),
                tags$br(),
                "1340 Elings Hall",
                tags$br(),
                "605 Bissell Road",
                tags$br(),
                "Ames, IA, 50011-3270"
              )
            ),
            column(
              4,
              h1(),
              tags$strong(
                tags$a(
                  href = "PPMST_Manual_v1.pdf",
                  "PPMST User Manual",
                  align = "center",
                  target = "_blank"
                )
              ),
              # tags$div(
              #   "If you have questions about the status",
              #   br(),
              #   "of the PPMST, contact:",
              #   br(),
              #   "kaleita@iastate.edu"
              # ),
              h1()
            )
          )
        )
      )
    ),
    # DISCLAIMER ====================================
    tabPanel("Disclaimer",
             # GENERAL DISCLAIMER INTRODUCTION ====
             
             fluidRow(
               column(1),
               column(8,
                      tags$div(
                        h1("Disclaimer"),
                        "Last updated June 8, 2020",
                        h3("INTRODUCTION"),
                        "The information provided by the Prairie Pothole Management Support Tool
                   (“we,” “us” or “our”) on bnahkala.shinyapps.io/ppmst/ (the “Site”) is
                   for general informational purposes only.
                   All information on the Site is provided in good faith,
                   however we make no representation or warranty of any kind, express or implied,
                   regarding the accuracy, adequacy, validity, reliability, availability or
                   completeness of any information on the Site.",
                        br(),
                        br(),
                        "Under no circumstance shall we have any liability to you for any loss or
                   damage of any kind incurred as a result of the use of the site or reliance on
                   any information provided on the site. Your use of the site and your reliance on
                   any information on the site is solely at your own risk.
                   This disclaimer was created using Termly’s disclaimer generator.",
                        br(),
                        # EXTERNAL LINKS DISCLAIMER ====
                        h3("EXTERNAL LINKS DISCLAIMER FOR WEBSITE"),
                        "The Site may contain (or you may be sent through the Site
                   links to other websites or content belonging to or originating
                   from third parties or links to websites and features in banners or
                   other advertising. Such external links are not investigated, monitored,
                   or checked for accuracy, adequacy, validity, reliability, availability or completeness by us.",
                        br(), br(),
                        "We do not warrant, endorse, guarantee, or assume responsibility for the accuracy or reliability
                   of any information offered by third-party websites linked through the site or any website or feature
                   linked in any banner or other advertising. We will not be a party to or in any way be responsible for monitoring any
                   transaction between you and third-party providers of products or services.",
                        # PROFESSIONAL DISCLAIMER ====
                        h3("PROFESSIONAL DISCLAIMER"),
                        "The Site cannot and does not contain legal, economic, financial, health, or other advice. 
                   The flood risk ranking information is provided for general informational and educational purposes 
                   only and is not a substitute for professional advice.",
                        br(), br(),
                        "Accordingly, before taking any actions based upon such information, we encourage you to consult 
                   with the appropriate professionals. We do not provide any kind of legal, economic, financial, health, or other advice. 
                   The use or reliance of any information contained on this site is solely at your own risk.",
                        # OPEN STATEMENT DISCLAIMER ====
                        h1("Open-Source Declaration"),
                        p("Copyright (C) 2020"),
                        p(
                          "This program is free software: you can redistribute it and/or modify
                          it under the terms of the GNU General Public License as published by
                          the Free Software Foundation, either version 3 of the License, or
                          (at your option) any later version."
                        ),
                        p(
                          "This program is distributed in the hope that it will be useful,
                          but WITHOUT ANY WARRANTY; without even the implied warranty of
                          MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
                          GNU General Public License for more details."
                        ),
                        p(
                          "You should have received a copy of the GNU General Public License
                          along with this program.  If not, see: "
                        ), 
                        tags$a(href="https://github.com/bnahkala/ppmst/blob/master/LICENSE", "https://github.com/bnahkala/ppmst/blob/master/LICENSE"),
                        br(), 
                        br()
                      )
               ),
               column(1),
               column(
                 2,
                 fluidRow(
                   p(),
                   tags$strong(
                     tags$a(
                       href = "PPMST_Manual_v1.pdf",
                       "PPMST User Manual",
                       align = "center",
                       target = "_blank"
                     )
                   ),
                   tags$div("For access to more detailed user ", 
                            tags$br(), "information and a step by step guide.")
                 ),
                 fluidRow(
                   br(),
                   br(),
                   tags$strong(
                     tags$a(
                       href = "https://www.sciencedirect.com/science/article/abs/pii/S0378377418303950",
                       "Prairie Pothole Publications",
                       align = "center",
                       target = "_blank"
                     )
                   ),
                   p("If interested in primary modeling data.")
                 )
               )
             ),
             
             # FOOTER ======
             fluidRow(
               style = "background-color:#f3f3f3;",
               column(
                 4,
                 h1(),
                 tags$div(
                   p("Prairie Pothole Management Support Tool  Copyright (C) 2020"),
                   p(
                     "This program comes with ABSOLUTELY NO WARRANTY.
            This is free software, and you are welcome to redistribute it
            under certain conditions. See license documentation for details."
                   )
                 ),
                 h1(),
                 # h1(),
                 # img(
                 #   src = "ISU.png",
                 #   href = "https://www.iastate.edu",
                 #   height = 95,
                 #   width = 300,
                 #   style = "display: block; margin-left: auto; margin-right: auto;"
                 # ),
                 # img(
                 #   src = "coe.png",
                 #   href = "https://www.engineering.iastate.edu/",
                 #   height = 40,
                 #   width = 300,
                 #   style = "display: block; margin-left: auto; margin-right: auto;"
                 # ),
                 # h1()
               ),
               column(
                 4,
                 h1(),
                 tags$div(
                   tags$strong("Department of Agricultural and Biosystems Engineering"),
                   tags$br(),
                   "1340 Elings Hall",
                   tags$br(),
                   "605 Bissell Road",
                   tags$br(),
                   "Ames, IA, 50011-3270"
                 )
               ),
               column(4,
                      h1(),
                      tags$strong(
                        tags$a(
                          href = "PPMST_Manual_v1.pdf",
                          "PPMST User Manual",
                          align = "center",
                          target = "_blank"
                        )
                      ),
                      # tags$div(
                      #   "If you have questions about the status",
                      #   br(),
                      #   "of the PPMST, contact:",
                      #   br(),
                      #   "kaleita@iastate.edu"
                      # ),
                      h1())
             )
    )
    )      
  

# server ========================================
server <- function(input, output, session) {
  
  # RENDER WEBMAP -output$mymap- ON INPUT TAB ========================
  # made this a reactive object for potential compatibility with mapshot()
  map_reactive <- reactive({
    leaflet() %>%
      setView(lng = -93.6319,
              lat = 42.0308,
              zoom = 12) %>%
      # BASEMAPS ====
    addProviderTiles(providers$Esri.WorldImagery, group = "Aerial") %>%
      # addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
      addPolygons(
        data = counties,
        fill = F,
        stroke = T,
        color = "black"
      ) %>%
      # IA ORTHO SERVER ADDITIONS ====
    leaflet.extras2::addWMS(
      "https://ortho.gis.iastate.edu/arcgis/services/ortho/naip_2010_nc/ImageServer/WMSServer",
      layers = "naip_2010_nc",
      options = WMSTileOptions(
        format = "image/png",
        crs = "+init=epsg:4326",
        transparent = F,
        info_format = "text/html"),
      attribution = "IA ORTHO GIS SERVER",
      group = "NAIP 2010"
    ) %>%
      leaflet.extras2::addWMS(
        "https://ortho.gis.iastate.edu/arcgis/services/ortho/lidar_dem_3m/ImageServer/WMSServer?request=GetCapabilities&service",
        # "https://athene.gis.iastate.edu/arcgis/services/ortho/lidar_dem_3m/ImageServer/WMSServer",
        layers = "lidar_dem_3m",
        options = WMSTileOptions(
          format = "image/png",
          version = "1.3.0",
          crs = "+init=epsg:4326",
          transparent = T,
          info_format = "text/xml",
          tiled=F
        ),
        attribution = "IA ORTHO GIS SERVER",
        group = "3-meter DEM", 
        popupOptions = popupOptions(maxWidth = 300, closeOnClick = T)
      ) %>%
      # MAP ADD ONS ====
    addLayersControl(
      baseGroups = c(
        "Aerial",
        "NAIP 2010",
        # "Topographic",
        "3-meter DEM"
      ),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
      addScaleBar(position = 'bottomleft') %>%
      addMeasure(
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "feet",
        primaryAreaUnit = "hectares",
        secondaryAreaUnit = "acres",
        activeColor = "#8B0000",
        completedColor = "#228B22"
      ) #%>%
      # addDrawToolbar(
      #   polylineOptions = FALSE,
      #   polygonOptions = TRUE,
      #   rectangleOptions = F,
      #   circleOptions = F,
      #   markerOptions = FALSE,
      #   circleMarkerOptions = FALSE,
      #   singleFeature = TRUE,
      #   editOptions = editToolbarOptions()
      # )
  })
  
    # MAP OUTPUT =====
  output$mymap <- renderLeaflet({
    map_reactive()
  })
  
  # store the current user-created version
  # of the Leaflet map for download in 
  # a reactive expression
  user.created.map <- reactive({
    
    # call the foundational Leaflet map
    map_reactive() %>%
      
      # store the view based on UI
      setView( lng = input$map_center$lng
               ,  lat = input$map_center$lat
               , zoom = input$map_zoom
      )
    
  }) # end of creating user.created.map()
  
  # MAPSHOT ====
  output$MapView <- downloadHandler(
    filename = paste0("map"
                       , ".pdf"
    )
    
    , content = function(file) {
      mapshot( x = user.created.map()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  )
    
  
  
  # IMAGE OF POTHOLE ====
  output$riskicon <- renderUI({
    if (rf.sample.pred() >= 5) {
      
        tags$img(
          src="high.PNG", 
          style="display: block; margin-left: auto; margin-right: auto; width:100%"
        )
      
    } else if (rf.sample.pred() >= 3) {
      
        tags$img(
          src="med.PNG",
          style="display: block; margin-left: auto; margin-right: auto; width:100%"
        )
      
    } else {
     
        tags$img(
          src="low.PNG", 
          style="display: block; margin-left: auto; margin-right: auto; width:100%"
        )
      
    }
  })
  
  # RENDER EXAMPLE TABLE -output$rFInfo- IN TECHNICAL ABOUT THE TOOL SECTION =====
  
  # RENDER TABLE of RISK INTERPRETATION -output$risk_interpretation- =====
  output$risk_interpretation <- renderTable({
    riskInfo
  })
  
  # RENDER EMBEDDED PDF ====
  output$frame <- renderUI({
    tags$iframe(style="height:600px; width:100%; scrolling=yes",
                src="PPMST_Manual_v1.pdf")
  })
  
  # SAVE POTHOLE DEFAULTS -input$SaveDefaults- =================
  observeEvent(input$SaveDefaults, {
    val1 <- input$area_estimate
    val2 <- input$h2oshed_estimate
    val3 <- input$max.depth
    val4 <- input$watershed.slope
    
    updateSliderInput(
      session,
      inputId = "area_estimate1",
      val = val1,
      min = 0,
      max = 10,
      step = 0.1
    )
    updateSliderInput(
      session,
      inputId = "h2oshed_estimate1",
      val = val2,
      min = 1,
      max = 200,
      step = 0.1
    )
    updateSliderInput(
      session,
      inputId = "max.depth1",
      val = val3,
      min = 0,
      max = 5,
      step = 0.1
    )
    updateSliderInput(
      session,
      inputId = "watershed.slope1",
      val = val4,
      min = 0,
      max = 15,
      step = 0.1
    )
    
    # DEFAULTS POP-UP ====
    if (rf.sample.pred() >= 5) {
      # HIGH RISK WARNING =====
      showModal(
        modalDialog(
          title = "Warning Message", 
          tags$div(
            strong("Baseline Pothole Risk: "), 
            strong(rf.sample.pred(), style = "color:red"), 
            "out of 10.",
            br(),
            br(),
            "In current conditions, your pothole is considered high risk with regards to flooding. 
              Additional drainage or significant land retirement would likely reduce the risk of this pothole. 
              If it has existing drainage, it is likely that the flood conditions of the pothole will not significantly 
              improve with further infrastructure. ",
            br(), br(), 
            "Continue to the 'Analysis' tab."
          ),
          easyClose = T
        )
      )
    } else if (rf.sample.pred() >= 3) {
      # MEDIUM RISK WARNING =====
      showModal(
        modalDialog(
          title = "Warning Message", 
          tags$div(
            strong("Baseline Pothole Risk: "), 
            strong(rf.sample.pred(), style = "color:orange"), 
            "out of 10.",
            br(),
            br(),
            "In current conditions, your pothole is considered medium risk with regards to flooding. 
              Additional drainage may moderately improve flood conditions. Pothole retirement and conservation 
              tillage practices may be the only options available to reduce the overall risk of the pothole.",
            br(), br(), 
            "Continue to the 'Analysis' tab."
          ),
          easyClose = T
        )
      )
    } else {
      # LOW RISK WARNING =====
      showModal(
        modalDialog(
          title = "Informational Message", 
          tags$div(
            strong("Baseline Pothole Risk: "), 
            strong(rf.sample.pred(), style = "color:green"), 
            "out of 10.",
            br(),
            br(),
            "In current conditions, your pothole is considered low risk with regards to flooding. 
              Most management and drainage actions will not significantly change flood risk or change 
              the impact of flooding on crop survival, based on the range of flooding observed in modeling studies.",
            br(), br(), 
            "Continue to the 'Analysis' tab."
          ),
          easyClose = T
        )
      )
    }
    
  })
  
  # RENDER RISK SUMMARY =====
  output$risksum <- renderUI({
    if (rf.sample.pred() >= 5) {
      HTML(
        paste(
          "In current conditions, your pothole is considered high risk with regards to flooding.
        Additional drainage or full field retirement are the only actions that will significantly reduce the risk of this pothole.
        If it has existing drainage, it is likely that the flood conditions of the pothole will not
        significantly improve with further infrastructure. ",
          "A variety of factors may contribute to this ranking, as you have seen in the baseline assessment.
        Within the model, important variables include drainage, maximum flow path, tillage, and maximum watershed
        relief (estimated in this app using watershed slope). These variables tend to be used to find the largest
        differences in risk among different land use and pothole characteristics.
        High risk potholes tend to have large watershed to pothole area ratios. A high ratio means a
        lot of water is draining to this area compared to the storage space available. Furthermore, the
        model suggests steeper, shorter watersheds and deeper potholes tend to have a higher risk. Steep,
        short watersheds would reduce the tendency for precipitation to infiltrate during storm events.
        Compiled with a deeper pothole, where there is less area utilized to infiltrate or evaporate water,
        stormwater can become effectively “trapped” for longer periods of time. Finally, these deeper systems
        may be more connected to the water table, making it easier for the pothole to receive water both laterally
        through shallow groundwater connections and vertically through runoff.",
          "NOTE: None of the variables when considered alone can accurately predict the risk of a pothole.
        These are general trends observed from the data. The addition of drainage would still significantly reduce the flooding in this pothole,
        but other considerations may be appropriate based on the inherent characteristics of the pothole. ",
          sep = '<br/>'
        )
      )
    } else if (rf.sample.pred() >= 3) {
      HTML(
        paste(
          "In current conditions, your pothole is considered medium risk with regards to flooding.
        Additional drainage may moderately improve flood conditions. Pothole retirement and conservation
        tillage practices may remain the only options to reduce the magnitude of flooding.",
          "A variety of factors may contribute to this ranking, as you have seen in the baseline assessment.
        Within the model, important variables include drainage, maximum flow path, tillage, and maximum watershed
        relief (estimated in this app using watershed slope). These variables tend to be used to find the largest
        differences in risk among different land use and pothole characteristics.
        Medium risk potholes are best described relative to high and low risk scenarios. High risk potholes have
        larger watershed area to pothole area ratios and frequently do not have added drainage. They may have
        tillage practices that reflect improved soil infiltration capacity, but this provides minimal benefits
        when looking at overall risk. Medium risk potholes likely have moderate watershed areas and moderate watershed
        slopes and pothole depths. They are moderately drained, likely only with subsurface drain lines.",
        "NOTE: None of the variables when considered alone can accurately predict the risk of a pothole.
        These are general trends observed from the data. A medium risk pothole may benefit from additional drainage,
        tillage or land retirement practices.",
          sep = '<br/>'
        )
      )
    } else {
      HTML(
        paste(
          "In current conditions, your pothole is considered low risk with regards to flooding.
        Most management and drainage actions will not significantly change flood risk or change
        the impact of flooding on crop survival.",
          "A variety of factors may contribute to this ranking, as you have seen in the baseline assessment.
        Within the model, important variables include drainage, maximum flow path, tillage, and maximum watershed
        relief (estimated in this app using watershed slope). These variables tend to be used to find the largest
        differences in risk among different land use and pothole characteristics.
        Low risk potholes tend to have longer watersheds, which provides time for precipitation to infiltrate
        as it makes it way to the pothole via runoff. Other factors that suggest this pothole is low risk would
        be the low watershed area to pothole area ratio. This ratio implies that not a lot of water runs to the
        pothole compared to the surface area, and related, the volume of water it can store. However, risk is highly
        dependent on management activities as well. It is likely that this pothole is heavily drained, or is in a
        watershed with a high percentage of perennial vegetation (such as pasture). If conditions in this pothole
        consistently cause crop failure, there could be issues with plugged or mislaid tile lines or inlets. There 
          is supplementary data lower on this page where you can view what might be expected of low risk potholes, 
          to compare the flood behavior of your pothole.", 
        "NOTE: None of the variables when considered alone can accurately predict the risk of a pothole.
        These are general trends observed from the data. A low risk pothole is unlikely to become any
        less risky based on changes to drainage, tillage or land retirement
        practices without taking significant land out of production. ",
          sep = '<br/>'
        )
      )
    }
  })
  
  # REACT AND CALCULATE WATERSHED PARAMS FROM -input$watershed.slope- =====
  max.flow.path <- reactive({
    if (input$field.position == "Circular") {
      # circular
      round((2*sqrt((input$h2oshed_estimate*10000) / 3.1415)) - (sqrt((input$area_estimate*10000) / 3.1415)), 0)
    } else {
      # ellipsoidal
      round((2*sqrt((2*input$h2oshed_estimate*10000) / 3.1415)) - (sqrt((input$area_estimate*10000) / 3.1415)), 0)
    }
  })
  
  max.relief <- reactive({
      max.flow.path() * input$watershed.slope / 100
  })
  # REACT TO INPUT DATA, SAVE TO -sample.values.df()- =================
  sample.values.df <- reactive({
    data.frame(
      Name=c("Drainage",
             "Tillage",
             "Land Cover - Pothole",
             "Land Cover - Field",
             "Watershed Area to Pothole Area Ratio",
             "Maximum Watershed Relief",
             "Maximum Flow Path",
             "Maximum Pothole Depth"),
      Value=c(as.character(input$Drainage),
              as.character(input$Tillage),
              as.character(input$lulc.pothole),
              as.character(input$lulc.field),
              as.numeric(round(input$h2oshed_estimate / input$area_estimate, digits=1)),
              as.numeric(max.relief()),
              as.numeric(max.flow.path()),
              as.numeric(input$max.depth)),
      stringsAsFactors = FALSE
    )
  })
  
  # PREDICT RISK OF FLOODING in -rf.sample.pred()-, -.min, .max- and -output$prediction- =============
  rf.sample.pred <- reactive({
      randForest.app(
        as.character(input$Drainage),
        as.character(input$Tillage),
        as.character(input$lulc.pothole),
        as.character(input$lulc.field),
        as.numeric(
          round(input$h2oshed_estimate1 / input$area_estimate1, digits = 1)
        ),
        as.numeric(max.relief1()),
        as.numeric(max.flow.path1()),
        as.numeric(input$max.depth1)
      )
  })
  
  rf.sample.pred.min <- reactive({
    if (rf.sample.pred() <= 0.7) {
      "<0.1"
    } else {
      rf.sample.pred() - 0.7
    }
  })
  
  rf.sample.pred.max <- reactive({
      rf.sample.pred() + 0.7
  })
  
  output$prediction <- renderText({
      paste(rf.sample.pred.min()," to ", rf.sample.pred.max())
  })
  
  output$predictionDUP <- renderText({
    paste(rf.sample.pred.min()," to ", rf.sample.pred.max())
  })
  
  output$predictionDUP2 <- renderText({
    paste(rf.sample.pred.min()," to ", rf.sample.pred.max())
  })
  
  # MAINTAIN LOGIC DURING VARIABLE INPUT =====
  observe({
    if (max.relief1() < input$max.depth1) {
      showModal(
        modalDialog(
          title = "Error Message", 
          tags$div(
           "The depth of the pothole is currently larger than the maximum elevation change in the 
           watershed. Adjust any of the following parameters: maximum depth, watershed area, watershed
           slope, or watershed shape."
          ),
          easyClose = T
        )
      )
    } else {}
  })
  
  observe({
    if (input$area_estimate1 > input$h2oshed_estimate1) {
      showModal(
        modalDialog(
          title = "Error Message", 
          tags$div(
            "The area of the pothole is larger than its watershed area. Increase the watershed area 
            or decrease the pothole area."
          ),
          easyClose = T
        )
      )
    } else {}
  })

  # RENDER ALT SCENARIO LABELS ====
  output$lab1 <- renderText({
    paste0(input$AltID1, ":")
  })
  
  output$lab2 <- renderText({
    paste0(input$AltID2, ":")
  })
  
  output$lab3 <- renderText({
    paste0(input$AltID3, ":")
  })
  
  output$lab4 <- renderText({
    paste0(input$AltID4, ":")
  })
  # PREDICT RISK FOR ALL ALTERNATIVES =====
  max.flow.path1 <- reactive({

    if (input$field.position == "Circular") {
      # circular watershed
      round((2*sqrt((input$h2oshed_estimate1*10000) / 3.1415)) - (sqrt((input$area_estimate1*10000) / 3.1415)), 0)
    } else {
      # ellipsoidal
      round((2*sqrt((2*input$h2oshed_estimate1*10000) / 3.1415)) - (sqrt((input$area_estimate1*10000) / 3.1415)), 0)
    }
  })
  
  max.relief1 <- reactive({
    max.flow.path1() * input$watershed.slope1 / 100
  })
  
    # ALT 1 -rf.alternative.pred1()- with -.min,. .max- ====
  rf.alternative.pred1 <- reactive({
      randForest.app(
        as.character(input$Drainage1),
        as.character(input$Tillage1),
        as.character(input$lulc.pothole1),
        as.character(input$lulc.field1),
        as.numeric(
          round(input$h2oshed_estimate1 / input$area_estimate1, digits = 1)
        ),
        as.numeric(max.relief1()),
        as.numeric(max.flow.path1()),
        as.numeric(input$max.depth1)
      )
  })
  
  rf.alt1.pred.min <- reactive({
    if (rf.alternative.pred1() <= 0.7) {
      "<0.1"
    } else {
      rf.alternative.pred1() - 0.7
    }
  })
  
  rf.alt1.pred.max <- reactive({
    rf.alternative.pred1() + 0.7
  })

    # ALT 2 -rf.alternative.pred2()- with -.min,. .max- ====
  rf.alternative.pred2 <- reactive({
      randForest.app(
        as.character(input$Drainage2),
        as.character(input$Tillage2),
        as.character(input$lulc.pothole2),
        as.character(input$lulc.field2),
        as.numeric(
          round(input$h2oshed_estimate1 / input$area_estimate1, digits = 1)
        ),
        as.numeric(max.relief1()),
        as.numeric(max.flow.path1()),
        as.numeric(input$max.depth1)
      )
  })
  
  rf.alt2.pred.min <- reactive({
    if (rf.alternative.pred2() <= 0.7) {
      "<0.1"
    } else {
      rf.alternative.pred2() - 0.7
    }
  })
  
  rf.alt2.pred.max <- reactive({
    rf.alternative.pred2() + 0.7
  })
  
    # ALT 3 -rf.alternative.pred3()- with -.min,. .max- ====
  rf.alternative.pred3 <- reactive({
      randForest.app(
        as.character(input$Drainage3),
        as.character(input$Tillage3),
        as.character(input$lulc.pothole3),
        as.character(input$lulc.field3),
        as.numeric(
          round(input$h2oshed_estimate1 / input$area_estimate1, digits = 1)
        ),
        as.numeric(max.relief1()),
        as.numeric(max.flow.path1()),
        as.numeric(input$max.depth1)
      )
  })
  
  rf.alt3.pred.min <- reactive({
    if (rf.alternative.pred3() <= 0.7) {
      "<0.1"
    } else {
      rf.alternative.pred3() - 0.7
    }
  })
  
  rf.alt3.pred.max <- reactive({
    rf.alternative.pred3() + 0.7
  })
  
    # ALT 4 -rf.alternative.pred4()- with -.min,. .max- ====
  rf.alternative.pred4 <- reactive({
      randForest.app(
        as.character(input$Drainage4),
        as.character(input$Tillage4),
        as.character(input$lulc.pothole4),
        as.character(input$lulc.field4),
        as.numeric(
          round(input$h2oshed_estimate1 / input$area_estimate1, digits = 1)
        ),
        as.numeric(max.relief1()),
        as.numeric(max.flow.path1()),
        as.numeric(input$max.depth1)
      )
  })
  
  rf.alt4.pred.min <- reactive({
    if (rf.alternative.pred4() <= 0.7) {
      "<0.1"
    } else {
      rf.alternative.pred4() - 0.7
    }
  })
  
  rf.alt4.pred.max <- reactive({
    rf.alternative.pred4() + 0.7
  })
  
  
    # ALT OUTPUT -output$prediction##- ====
  output$prediction1 <- renderText({
      paste(rf.alt1.pred.min()," to ", rf.alt1.pred.max())
  })
  
  output$prediction2 <- renderText({
    paste(rf.alt2.pred.min()," to ", rf.alt2.pred.max())
  })
  
  output$prediction3 <- renderText({
    paste(rf.alt3.pred.min()," to ", rf.alt3.pred.max())
  })
  
  output$prediction4 <- renderText({
    paste(rf.alt4.pred.min()," to ", rf.alt4.pred.max())
  })
  
  # RENDER SUMMARY BARPLOT -output$barplot- AND INPUTS TABLE -output$baseinputs- IN ANALYSIS PAGE ====
  
  output$barplot <- renderPlot({
    ggplot(react.pred.df(),
           aes(x = Scenario, y = Risk.Prediction, fill = Risk.Prediction)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_gradient2(
        low = "#66a182", 
        mid = "#edae49",
        high = "#d1495b",
        midpoint = 4) +
      theme(text = element_text(size = 14)) +
      theme(axis.text.x = element_text(angle = 45)) +
      theme(legend.position = "none") +
      labs(
        x = "",
        y="Risk Prediction")
  })
  
  output$baseinputs <- renderTable({
    data.frame(
      "Inputs"=c(
        input$lulc.pothole,
        input$lulc.field,
        input$Drainage,
        input$Tillage
      )
    )
  })
  
  # RENDER SUMMARY BOXPLOTS -output$daysXX- IN ALTERNATIVES =====
  # baserow <- reactive({
  #   as.numeric(findInterval(rf.sample.pred(), riskMatrix$Risk))
  # })
  # 
  # altrow1 <- reactive({
  #   as.numeric(findInterval(rf.alternative.pred1(), riskMatrix$Risk))
  # })
  # altrow2 <- reactive({
  #   as.numeric(findInterval(rf.alternative.pred2(), riskMatrix$Risk))
  # })
  # altrow3 <- reactive({
  #   as.numeric(findInterval(rf.alternative.pred3(), riskMatrix$Risk))
  # })
  # altrow4 <- reactive({
  #   as.numeric(findInterval(rf.alternative.pred4(), riskMatrix$Risk))
  # })
  
  # EXAMPLE BOXPLOT. TO REPLICATE, CHANGE ALTROW() AND OUTPUT OBJECT INTEGER
  # output$days1 <- renderPlot({
  #   ggplot(riskMatrix[altrow1(), ],
  #     aes(
  #       x = as.factor(Risk),
  #       ymin = days.min,
  #       lower = days.Q1,
  #       middle = days.med,
  #       upper = days.Q3,
  #       ymax = days.max
  #     )
  #   ) +
  #     geom_boxplot(stat = "identity")+
  #     theme_minimal()+
  #     theme(axis.text.x = element_blank())+
  #     ylim(0, 150)+
  #     labs(
  #       x=NULL,
  #       y="Annual Days Flooded"
  #     )+
  #   theme(plot.margin=unit(c(0,0,0,0),"mm"))
  # }, height=200)

  # MAINTAIN LOGIC DURING ALTERNATIVE ASSESSMENT INPUTS (CONTINUITY) =====
  observe({
    if (input$lulc.field == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$lulc.pothole == "Corn-Soybean Rotation" |
        input$lulc.field1 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$lulc.pothole1 == "Corn-Soybean Rotation" | 
        input$lulc.field2 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$lulc.pothole2 == "Corn-Soybean Rotation" | 
        input$lulc.field3 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$lulc.pothole3 == "Corn-Soybean Rotation" | 
        input$lulc.field4 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$lulc.pothole4 == "Corn-Soybean Rotation") {
      
      showModal(
        modalDialog(
          title = "Warning Message", 
          tags$div(
            "If the field is in perennial cover, it is likely the pothole is in perennial cover. Please change one of the inputs. "
          ),
          easyClose = T
        )
      )
    } else {}
  })
  
  observe({
    if ((input$lulc.field == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$Tillage != "NA-Retired") |
        (input$lulc.field1 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$Tillage1 != "NA-Retired") | 
        (input$lulc.field2 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$Tillage2 != "NA-Retired") | 
        (input$lulc.field3 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$Tillage3 != "NA-Retired") | 
        (input$lulc.field4 == "Perennial Cover (Conservation Reserve, Grassed, etc.)" & input$Tillage4 != "NA-Retired")) {
      
      showModal(
        modalDialog(
          title = "Warning Message", 
          tags$div(
            "If the field is in perennial cover, make sure the NA is checked for tillage."
          ),
          easyClose = T
        )
      )
      
    } else if ((input$lulc.field == "Corn-Soybean Rotation" & input$Tillage == "NA-Retired") |
               (input$lulc.field1 == "Corn-Soybean Rotation" & input$Tillage1 == "NA-Retired") | 
               (input$lulc.field2 == "Corn-Soybean Rotation" & input$Tillage2 == "NA-Retired") | 
               (input$lulc.field3 == "Corn-Soybean Rotation" & input$Tillage3 == "NA-Retired") | 
               (input$lulc.field4 == "Corn-Soybean Rotation" & input$Tillage4 == "NA-Retired")) {
      
      showModal(
        modalDialog(
          title = "Warning Message", 
          tags$div(
            "If the field is actively farmed, ensure you selected the correct tillage practice in the field."
          ),
          easyClose = T
        )
      )
    }
  })
  
 
  # DATA FRAME OF ALL CURRENT INPUTS/PREDICTIONS -react.df()- and -react.pred.df()- =====
  react.df <- reactive({
    data.frame(
      "Scenario" = c(
        "Baseline",
        input$AltID1,
        input$AltID2,
        input$AltID3,
        input$AltID4
      ),
      "CountyID" = c(rep(input$cnty, 5)),
      "FieldID" = c(rep(input$field)),
      "Pothole Area" = c(rep(input$area_estimate, 5)),
      "Watershed Area" = c(rep(input$h2oshed_estimate, 5)),
      "Maximum Depth" = c(rep(input$max.depth, 5)),
      "Watershed Relief" = c(rep(max.relief(), 5)),
      "Maximum Flow Path" = c(rep(max.flow.path(), 5)),
      "LULC of Pothole" = c(
        input$lulc.pothole,
        input$lulc.pothole1,
        input$lulc.pothole2,
        input$lulc.pothole3,
        input$lulc.pothole4
      ),
      "LULC of Field" = c(
        input$lulc.field,
        input$lulc.field1,
        input$lulc.field2,
        input$lulc.field3,
        input$lulc.field4
      ),
      "Drainage" = c(
        input$Drainage,
        input$Drainage1,
        input$Drainage2,
        input$Drainage3,
        input$Drainage4
      ),
      "Tillage" = c(
        input$Tillage,
        input$Tillage1,
        input$Tillage2,
        input$Tillage3,
        input$Tillage4
      )
    )
  })
  
  react.pred.df <- reactive({
    data.rdf <- data.frame(
      "Scenario" = c(
        "Baseline",
        input$AltID1,
        input$AltID2,
        input$AltID3,
        input$AltID4
      ),
      "CountyID" = c(rep(input$cnty, 5)),
      "FieldID" = c(rep(input$field)),
      "Risk Prediction" = c(
        rf.sample.pred(),
        rf.alternative.pred1(),
        rf.alternative.pred2(),
        rf.alternative.pred3(),
        rf.alternative.pred4()
      ),
      "Minimum Risk" = c(
        rf.sample.pred.min(),
        rf.alt1.pred.min(),
        rf.alt2.pred.min(),
        rf.alt3.pred.min(),
        rf.alt4.pred.min()
      ),
      "Maximum Risk" = c(
        rf.sample.pred.max(),
        rf.alt1.pred.max(),
        rf.alt2.pred.max(),
        rf.alt3.pred.max(),
        rf.alt4.pred.max()
      )
    )
    data.rdf$Scenario <- factor(data.rdf$Scenario, levels=c(
      "Baseline",
      input$AltID1,
      input$AltID2,
      input$AltID3,
      input$AltID4
    ))
    return(data.rdf)
  })
  # GENERATE ALT ANALYSIS REPORT -output$Report- =====
  # download handler taken from external source and adapted
  output$Report <- downloadHandler(
    filename = "report.html",
    # or "report.pdf, but I (BAN) am having trouble
    # with the pdf generator functions (pdflatex, miktex)"
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = T)
      
      # parameters to pass to Rmd Document
      params <- list(
        i = react.df(),
        f = input$field,
        p = react.pred.df(),
        a = riskInfo#,
        # m = map.shot() - ideally, a screenshot of the map after the user has edited
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# CALL SHINY APP ===========================
shinyApp(ui = ui, server = server)

