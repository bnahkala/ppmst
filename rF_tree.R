# AUTHOR: BRADY NAHKALA
# LAST REVISED: 11 JUN 2020
# PURPOSE: implement the single representative tree 
# from a random forest model of prairie pothole flood risk

# CAUTION: this decision tree is out of date (wrong model)
# as of 6.11.2020

# naming from app.R

# REFERENCE DATA
drain_list <- c("No drainage","Subsurface drain","Surface inlet to subsurface drain","Multiple inlets to subsurface drain")
till_list <- c("Conventional", "Conservation", "No Till", "NA-Retired")
lulc.pothole_list <- c("Corn-Soybean Rotation", "Perennial Cover (Conservation Reserve, Grassed, etc.)")
lulc.field_list <- c("Corn-Soybean Rotation", "Perennial Cover (Conservation Reserve, Grassed, etc.)")


# APPLY RANDOM FOREST REPRESENTATIVE TREE
classify_pothole <- function(d, t, lup, luf, capa, mxd, wr, mxfp) {
  # RANDOM FOREST DATA SPLITS
  wr1 <- 4.95
  mxfp1 <- 300
  mxfp2 <- 197
  capa1 <- 5.6
  capa2 <- 9.45
  
  # first level
  if (d == drain_list[1]) {
    # second level, risky side
    if (mxfp > mxfp1) {
      # third level, risky side
      if (luf == lulc.field_list[1]) {
        # fourth level, risky side
        if (capa >= capa1) {
          val <- 3.725
          
        } else {
          val <- 2.276
        }
        
      } else {
        val <- 0.728
        
      }
    } else {
      # third level, risky side
      if (mxfp > mxfp2) {
        # fourth level, risky side
        if (luf == lulc.field_list[1]) {
          val <- 8.332
          
        } else {
          val <- 4.603
          
        }
      } else {
        # fourth level, risky side
        if (luf == lulc.field_list[1]) {
          val <- 4.707
          
        } else {
          val <- 1.211
          
        }
      }
    }
  } else {
    # second level, safe side
    if (d == drain_list[2]) {
      # third level, safe side
      if (capa >= capa2) {
        val <- 3.793
        
      } else {
        # fourth level, safe side
        if (wr >= wr1) {
          val <- 1.837
          
        } else {
          val <- 0.818
          
        }
      }
    } else {
      # third level, safe side
      if (mxfp >= mxfp1) {
        # fourth level, safe side
        if (t == till_list[1]) {
          val <- 0.530
          
        } else {
          val <- 0.249
          
        }
      } else {
        # fourth level, safe side
        if (t == till_list[3] | t == till_list[4]) {
          val <- 0.943
          
        } else {
          # fifth level, safe side
          if (wr >= wr1) {
            val <- 0.451
            
          } else {
            val <- 0.097
            
          }
        }
      }
    }
  }
  
  val <- round(val, digits = 1)
  return(val)
}

