#======================================================#
# Code for automatically calculating moonlight indexes #
# Author: Yuri Niella                                  #
#======================================================#

# Packages required for analysis
library(suncalc)
library(lunar)
library(crayon)

# x1 = Longitude of gear set
# y1 = Latitude of gear set
# x2 = Longitude of gear retrieval/catch
# y2 = Latitude of gear retrieval/catch
# date1 = Date and time of gear set in UTC (as.POSIXct format = "%Y-%m-%d %H:%M:%S")
# date2 = Date and time of gear retrieval/catch in UTC (as.POSIXct format = "%Y-%m-%d %H:%M:%S")

MoonIndex <- function (x1, y1, x2, y2, date1, date2) {

  if (length(date1) != length(date2)) {
    stop("Set and retrieval/catch date vectors should have the same length.")    
  }
  if (length(x1) != length(x2)) {
      stop("Set and retrieval/catch position vectors should have the same length.")    
  }

  #------------------------------------------------------------#
  # Calculate duration of night-time period during fishing set #
  #------------------------------------------------------------#

  Circadian.set <- NULL
  Circadian.catch <- NULL
  Nighttime <- NULL
  Set.save <- NULL

  cat("Calculating percentage of night-time per fishing set", fill = 1)
  pb <-  txtProgressBar(min = 0, max = length(date1),  
                        initial = 0, style = 3, width = 60)
  
  for (i in 1:length(date1)) {

    aux1 <- getSunlightTimes(date = as.Date(date1[i]), lon = x1[i], lat = y1[i],
                             keep=c("dawn","sunrise","sunset","dusk","night"), 
                             tz= "UTC")

    aux2 <- getSunlightTimes(date = as.Date(date2[i]), lon = x2[i], lat = y2[i],
                             keep=c("dawn","sunrise","sunset","dusk","night"), 
                             tz= "UTC")

    ### Circadian period
    if(date1[i] < aux1$dawn){
      aux.set <- "Night"
    }  
    if(date2[i] < aux2$dawn){
      aux.catch <- "Night"
    }
    
    if(date1[i] >= aux1$dawn & date1[i] < aux1$sunrise){
      aux.set <- "Dawn"
    }
    if(date2[i] >= aux2$dawn & date2[i] < aux2$sunrise){
      aux.catch <- "Dawn"
    }  
    
    if(date1[i] >= aux1$sunrise & date1[i] < aux1$sunset){
      aux.set <- "Day"
    }
    if(date2[i] >= aux2$sunrise & date2[i] < aux2$sunset){
      aux.catch <- "Day"
    }

    if(date1[i] >= aux1$sunset & date1[i] < aux1$night){
      aux.set <- "Dusk"
    }
    if(date2[i] >= aux2$sunset & date2[i] < aux2$night){
      aux.catch <- "Dusk"
    }

    if(date1[i] >= aux1$night){
      aux.set <- "Night"
    }
    if(date2[i] >= aux2$night){
      aux.catch <- "Night"
    }

    Circadian.set <- c(Circadian.set, aux.set)
    Circadian.catch <- c(Circadian.catch, aux.catch)


    ### Percentage of night-time during fishing set
    set.tot <- as.numeric(difftime(date2[i], date1[i], units = "mins")) # Set duration in minutes

    if (set.tot > 2800) { # Fishing set longer than 48 hours!
      aux.night <- NA
    } else {

    ### Set and retrieval on the same day

    if (as.Date(date1[i]) == as.Date(date2[i]) &
        aux.set == "Day" & aux.catch == "Day") { # Set during daytime only
      
        aux.night <- 0
    }

    if (as.Date(date1[i]) == as.Date(date2[i]) &
      aux.set != "Night" & aux.catch == "Night") { # Set during day but retrieve during the night 

      sunset <- as.numeric(difftime(aux1$night, date1[i], units = "mins"))
      aux.night <- (set.tot - sunset) / set.tot 
      }
    

    if (as.Date(date1[i]) == as.Date(date2[i]) &
      aux.set == "Night" & aux.catch != "Night") { # Set during night but retrieve during the day 

      sunrise <- as.numeric(difftime(date2[i], aux2$dawn, units = "mins"))
      aux.night <- (set.tot - sunrise) / set.tot 
    }

    if (as.Date(date1[i]) == as.Date(date2[i]) &
        aux.set == "Night" & aux.catch == "Night") { # Set during night and retrieve during night
        
        if (date1[i] > aux1$night & date2[i] > aux2$night) { # Both occurred after night period!
         aux.night <- 1
        } 

        if (date1[i] < aux1$dawn & date2[i] > aux2$night) { # Set early night and retrieve late night
          
          sunrise <- as.numeric(difftime(aux1$dawn, date1[i], units = "mins"))
          sunset <- as.numeric(difftime(aux1$night, aux1$dawn, units = "mins"))

          aux.night <- (set.tot - (sunrise + sunset)) / set.tot 
        }
      } 


    ### Set and retrieval on different days

    if (as.Date(date1[i]) != as.Date(date2[i]) &
        aux.set != "Night" & aux.catch != "Night") { # Set during daytime only
      
        sunset <- as.numeric(difftime(aux1$night, date1[i], units = "mins"))
        sunrise <- as.numeric(difftime(date2[i], aux2$dawn, units = "mins"))
        aux.night <- (set.tot - (sunset + sunrise)) / set.tot 
    }

    if (as.Date(date1[i]) != as.Date(date2[i]) &
        aux.set != "Night" & aux.catch == "Night") { # Set during day but retrieve during the night 
      
        if (date2[i] < aux2$dawn) { # Retrieve at early night 
          sunset <- as.numeric(difftime(aux1$night, date1[i], units = "mins"))
          aux.night <- (set.tot - sunset) / set.tot
        }

        if (date2[i] > aux2$night) { # Retrieve at late night 
          sunset <- as.numeric(difftime(aux1$night, date1[i], units = "mins"))
          sunrise <- as.numeric(difftime(aux2$night, aux2$dawn, units = "mins"))
          aux.night <- (set.tot - (sunset + sunrise)) / set.tot
        }
    }

    if (as.Date(date1[i]) != as.Date(date2[i]) &
        aux.set == "Night" & aux.catch != "Night") { # Set during night but retrieve during the day

        if (date1[i] < aux1$dawn) { # Set at early night 
          sunset <- as.numeric(difftime(aux1$night, aux1$dawn, units = "mins"))
          sunrise <- as.numeric(difftime(aux2$dawn, date2[i], units = "mins"))
          aux.night <- (set.tot - (sunset + sunrise)) / set.tot
        }

        if (date1[i] > aux1$night) { # Set at late night 
          sunrise <- as.numeric(difftime(date2[i], aux2$dawn, units = "mins"))
          aux.night <- (set.tot - sunrise) / set.tot
        }
    }

    if (as.Date(date1[i]) != as.Date(date2[i]) &
        aux.set == "Night" & aux.catch == "Night") { # Set during night and retrieve during night

        if (date1[i] > aux1$night & date2[i] < aux2$dawn) { # Late night set x early night retrieve
          aux.night <- 1
        } else {
          aux.night <- NA # Multiple nights!
        }
    }
  }
 
  Nighttime <- c(Nighttime, aux.night)
  Set.save <- c(Set.save, set.tot)
  setTxtProgressBar(pb, i)
  }

  close(pb)
  rm(i, aux1, aux2, sunset, sunrise)

  if (anyNA(Nighttime) == TRUE) {
    cat(red(paste0("Warning: ", length(which(is.na(Nighttime) == TRUE)),  " fishing sets include multiple nights and will have missing moonlight indexes.")), fill = 1)
  }


  #------------------------------------------------#
  # Calculate average moonlight during fishing set #
  #------------------------------------------------#

  # Moon.light1 <- NULL # Set
  # Moon.light2 <- NULL # Ret
  Moon.light <- NULL  # Average light

  cat("Obtaining daily percentages of moon illumination for night-time periods", fill = 1)
  pb <-  txtProgressBar(min = 0, max = length(date1),  
                        initial = 0, style = 3, width = 60)
  
  for (i in 1:length(date1)) {

    if (as.Date(date1[i]) == as.Date(date2[i])) {   # Same day!
      aux1 <- lunar.illumination(as.Date(date1[i]))
    }

    if (as.Date(date1[i]) != as.Date(date2[i])) {   # Different days!
      aux1 <- mean(c(lunar.illumination(as.Date(date1[i])), lunar.illumination(as.Date(date2[i]))), na.rm = TRUE)
    }

    Moon.light <- c(Moon.light, aux1)

  setTxtProgressBar(pb, i)
  }
  close(pb)
  rm(i, aux1)


  #---------------------------------------#
  # Maximum moon angle during fishing set #
  #---------------------------------------#

  Moon.angle <- NULL  

  cat("Obtaining maximum moon angle during fishing set", fill = 1)
  pb <-  txtProgressBar(min = 0, max = length(date1),  
                        initial = 0, style = 3, width = 60)

  for (i in 1:length(date1)) {

    # Angle at set
    aux <- seq(date1[i], date2[i], by = 1800) # Time interval every 30 min
    aux <- c(aux, date2[i]) # Include ret time!
    angle.save <- NULL

    for(ii in 1:length(aux)) {
      aux1 <- getMoonPosition(date = aux[ii], lon = x1[i], lat = y1[i])
      aux1 <- aux1$altitude * 57.2958 # Convert angle from radians to degrees
      angle.save <- c(angle.save, aux1)
    }

    aux2 <- max(angle.save, na.rm = TRUE)

    if (aux2 < 0) { # Moon only bellow the horizon!
      aux2 <- 0
    }

    Moon.angle <- c(Moon.angle, aux2)

    setTxtProgressBar(pb, i)
  }
  close(pb)
  rm(i, aux, aux1, aux2)


  #---------------------------#
  # Calculate moonlight index #
  #---------------------------#
  
  MI <- NULL

  cat("Calculating moonlight indexes!", fill = 1)
  pb <-  txtProgressBar(min = 0, max = length(date1),  
                        initial = 0, style = 3, width = 60)

  for (i in 1:length(date1)) {
    aux <- (Moon.light[i] * Moon.angle[i]) * Nighttime[i]
    MI <- c(MI, aux)

    setTxtProgressBar(pb, i)
  }
  close(pb)
  rm(i, aux)
  MI <- MI/100 # Set range to be from 0 to 1

  #----------------------------#
  # Save output as a dataframe #
  #----------------------------#
  df <- data.frame(Set.time.UTC = date1, Ret.time.UTC = date2, Set.period = Circadian.set, Ret.period = Circadian.catch, 
    Light = Moon.light, Angle = Moon.angle, Nighttime = Nighttime, MI = MI)

  return(df)
}

