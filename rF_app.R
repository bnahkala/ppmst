# BRADY NAHKALA
# LAST REVISED: 12 MAY 2020

# # PURPOSE AND NOTES: =============================================== 
# Generate the random forest classification algorithm  
# to be used by input data in the web application.
# 
# 
# LIBRARY ============================================================
library(dplyr)
library(tidyr)
library(randomForest)

simulation_names <- data.frame("Drain0" = c("Drain0", "Drain0CT", "Drain0NT", "RetiredPothole", "ConTillRetired", "NoTillRetired","RetiredAll"),
                               "Drain1" = c("Drain1", "ConTillDrain1", "Drain1NT", "Drain1Retired", "Drain1RetiredCT", "Drain1RetiredNT","Drain1RetiredAll"),
                               "Drain2" = c("Drain2", "ConTillDrain2", "Drain2NT", "Drain2Retired", "Drain2RetiredCT", "Drain2RetiredNT","Drain2RetiredAll"),
                               "Drain3" = c("Drain3", "ConTillDrain3", "Drain3NT", "Drain3Retired", "Drain3RetiredCT", "Drain3RetiredNT","Drain3RetiredAll"))
sim_nms <- simulation_names %>%
  gather()

# RANDOM FOREST ======================================================
randForest.app <- function(d, t, lp, lf, capa, mwr, mfl, md) {
  
  # REFERENCE DATA
  drain_list <- c("No drainage","Subsurface drain","Surface inlet to subsurface drain","Multiple inlets to subsurface drain")
  till_list <- c("Conventional", "Conservation", "No Till", "NA-Retired")
  lulc.pothole_list <- c("Corn-Soybean Rotation", "Perennial Cover (Conservation Reserve, Grassed, etc.)")
  lulc.field_list <- c("Corn-Soybean Rotation", "Perennial Cover (Conservation Reserve, Grassed, etc.)")
  rsq <- function(x,y) cor(x,y)^2
  
  if (d == drain_list[1]) {
    d <- "none"
  } else if (d == drain_list[2]) {
    d <- "low"
  } else if (d == drain_list[3]) {
    d <- "medium"
  } else if (d == drain_list[4]) {
    d <- "high"
  } else {}
  
  if (t == till_list[1]) {
    t <- "conventional"
  } else if (t == till_list[2]) {
    t <- "contill"
  } else if (t == till_list[3]) {
    t <- "notill"
  } else if (t == till_list[4]) {
    t <- "N/A"
  } else {}
  
  if (lp == lulc.pothole_list[1]) {
    lp <- "CS"
  } else {
    lp <- "R"
  } 
  
  if (lf == lulc.pothole_list[1]) {
    lf <- "CS"
  } else {
    lf <- "R"
  } 
  
  data.df <-
    read.csv('./data/rFdata.csv',
             header = TRUE,
             sep = ",")
  data.df <- data.df %>%
    filter(scen %in% sim_nms$value)
  data.df <- within(data.df, rm(catm3, catm6, scen, Pothole))
  
  
  new.data <- data.frame(
    "Drainage"=d,
    "Tillage"=t,
    "lulc.pothole"=lp,
    "lulc.field"=lf,
    "capa.ratio"=capa,
    "max.h2oshed.relief"=mwr,
    "max.flow.path"=mfl,
    "max.depth"=md
  )
  
  
  m <- 6
  n <- 500
  mxnd <- 15
  set.seed(22)
  rows.train.df <-
    sample(nrow(data.df), 0.7 * nrow(data.df), replace = FALSE) # list of row vals only
  train.df <- data.df[rows.train.df,]
  valid.df <- data.df[-rows.train.df,]
  
  model <- randomForest(
    rankm ~ Drainage +
      Tillage +
      lulc.pothole +
      lulc.field +
      capa.ratio +
      max.h2oshed.relief +
      max.flow.path +
      max.depth,
    data = data.df,
    ntree = n,
    mtry = m,
    maxnode = mxnd,
    replace = TRUE,
    importance = TRUE,
    proximity = TRUE
  )
  
  # really janky way to ensure that the input formats are the same with the new data, but should
  # be fine within a self-contained function
  data.df$Drainage[1] <- d
  data.df$Tillage[1] <- t
  data.df$lulc.pothole[1] <- lp
  data.df$lulc.field[1] <- lf
  data.df$capa.ratio[1] <- capa
  data.df$max.h2oshed.relief[1] <- mwr
  data.df$max.flow.path[1] <- mfl
  data.df$max.depth[1] <- md
  
  new.pred <- round(predict(model, data.df[1 ,]), digits = 1)
  
  return(new.pred)
}

