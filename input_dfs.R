
# Default input data into R shiny App

pothole_defaults = c(2.5, 30, 0.5, 4.5, 300)

sampleInfo <- data.frame(
  Name=c("Drainage",
         "Tillage",
         "Land Cover - Pothole",
         "Land Cover - Field",
         "Watershed Area to Pothole Area Ratio",
         "Maximum Watershed Relief",
         "Maximum Flow Path",
         "Maximum Pothole Depth"),
  Value=c("High, Medium, Low or None",
          "Conventional, Conservation, No-Till, or NA",
          "Corn-Soybean Rotation or Retired",
          "Corn-Soybean Rotation or Retired",
          "A Ratio, between 3 and 25",
          "A number, in meters, between 1 and 10",
          "A number, in meters, between 30 and 2500",
          "A number, in meters, between 0.3 and 2.0"),
  stringsAsFactors = FALSE
)

riskMatrix <- read.csv(file = "./data/improvement_data.csv", header=T, sep=",")

riskInfo <- data.frame(
  Risk_Level = c(0.1, 0.25, 1, 1.5, 2, 3, ">5"),
  area60pct = c(
    "40-50%",
    "40-50%",
    "40-70%",
    "50-70%",
    "50-70%",
    "60-80%",
    "65-80%"
  ),
  area30pct = c(
    "80%",
    "80%",
    "80-90%",
    "85-90%",
    "85-90%",
    "85-95%",
    "90-95%"
  ),
  days = c(
    "3-12",
    "3-22",
    "3-28",
    "5-28",
    "5-30",
    "7-58",
    "16-58"
  ),
  Condition = c(
    "Heavy Drainage, Entire Field Retired",
    "Heavy Drainage, Entire Field Retired or Improved Tillage Practices with Retired Pothole",
    "Light to Heavy Drainage, Retired Pothole, Conventional or Improved Tillage",
    "Light to Moderate Drainage, Retired Pothole, Conventional or Improved Tillage",
    "Light to Moderate Drainage, Retired or Planted, Conventional or Conservation Tillage",
    "Light to No Drainage, Planted, Conventional  or Conservation Tillage",
    "No Drainage, Planted, Any Tillage Practice"
  ),
  stringsAsFactors = FALSE
)
colnames(riskInfo) <-
  c(
    "Risk Level",
    "60% of the pothole floods ___ % of years",
    "30% of the pothole floods ___ % of years",
    "Expected annual number of days with standing water",
    "Representative Conditions"
  )

