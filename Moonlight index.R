#======================================================#
# Code for automatically calculating moonlight indexes #
# Author: Yuri Niella                                  #
#======================================================#

# Packages required for analysis
library(suncalc)
library(lunar)
library(ggplot2)
library(ggpubr)

# height = maximum height of the mountain that limits the time of moonlight influence. 
# data = fishing dataset (dataframe)

### Testing

data <- subset(df.sdl, select = c("lon_beg", "lat_beg", "dateTime_set.UTC", "Time.catch.UTC"))
data$Moon.side[data$Moon.side == "West"] <- 3071
data$Moon.side[data$Moon.side == "East"] <- 0
data$Moon.side <- as.numeric(data$Moon.side)
names(data) <- c("Lon", "Lat", "Time.set", "Time.ret")
data$Time.ret[9102] <- data$Time.ret[9102] + 86400 # Correct retrieval time wrong!


MoonVar <- function (data) {

  # Quality checks
  if (length(data$Time.set) != length(data$Time.ret)) {
    stop("Set and retrieval/catch date vectors should have the same length.")    
  }
  if (length(data$Lon) != length(data$Lat)) {
    stop("Set and retrieval/catch position vectors should have the same length.")    
  }
  if (length(which(data$Time.ret < data$Time.set)) > 0) {
    stop("Retrieval/catch times can't happen before set times.")
  }
  
  #=================================================#
  # Identify periods of exclusively daytime fishing #
  #=================================================#
  Circadian.set <- NULL
  Circadian.ret <- NULL 
  Time.lapse <- NULL
  Time.gear <- NULL

  cat("Identifying sets during exclusively daytime periods", fill = 1)
  pb <-  txtProgressBar(min = 0, max = nrow(data), initial = 0, style = 3, width = 60)
  for (i in 1:nrow(data)) {

    # When set and retrieval on same day
    if (as.Date(data$Time.set[i]) == as.Date(data$Time.ret[i])) {
      time.aux <- "Same"
    } else {
      time.aux <- "Different"
    }

    aux1 <- getSunlightTimes(date = as.Date(data$Time.set[i]), lon = data$Lon[i], lat = data$Lat[i],
                             keep = c("dawn", "sunrise", "sunset", "dusk", "night"), tz = "UTC")
    aux2 <- getSunlightTimes(date = as.Date(data$Time.ret[i]), lon = data$Lon[i], lat = data$Lat[i],
                             keep = c("dawn", "sunrise", "sunset", "dusk", "night"), tz = "UTC")

    aux.time <- as.numeric(difftime(time1 = data$Time.ret[i], time2 = data$Time.set[i], units = "hours"))

    ## Circadian period
    if(data$Time.set[i] < aux1$dawn){
      aux.set <- "Pre-dawn"
    }  
    if(data$Time.ret[i] < aux2$dawn){
      aux.catch <- "Pre-dawn"
    }
    if(data$Time.set[i] >= aux1$dawn & data$Time.set[i] < aux1$sunrise){
      aux.set <- "Dawn"
    }
    if(data$Time.ret[i] >= aux2$dawn & data$Time.ret[i] < aux2$sunrise){
      aux.catch <- "Dawn"
    }  
    if(data$Time.set[i] >= aux1$sunrise & data$Time.set[i] < aux1$sunset){
      aux.set <- "Day"
    }
    if(data$Time.ret[i] >= aux2$sunrise & data$Time.ret[i] < aux2$sunset){
      aux.catch <- "Day"
    }
    if(data$Time.set[i] >= aux1$sunset & data$Time.set[i] < aux1$night){
      aux.set <- "Dusk"
    }
    if(data$Time.ret[i] >= aux2$sunset & data$Time.ret[i] < aux2$night){
      aux.catch <- "Dusk"
    }
    if(data$Time.set[i] >= aux1$night){
      aux.set <- "After-dusk"
    }
    if(data$Time.ret[i] >= aux2$night){
      aux.catch <- "After-dusk"
    }

    Circadian.set <- c(Circadian.set, aux.set)
    Circadian.ret <- c(Circadian.ret, aux.catch)
    Time.lapse <- c(Time.lapse, aux.time)
    Time.gear <- c(Time.gear, time.aux)

    setTxtProgressBar(pb, i)    
  }
  close(pb)
  df.aux <- data.frame(Set = Circadian.set, Ret = Circadian.ret, Hour = Time.lapse, Time = Time.gear)
  # Sets only during daytime
  index1 <- which(df.aux$Time == "Same" & df.aux$Set != "Pre-dawn" & df.aux$Ret != "After-dusk")
  cat(paste0("M: A total of ", length(index1), " sets were deployed exclusively during day light and will be removed."), fill = 1)
  # Sets longer than a day
  index2 <- which(df.aux$Hour > 24 & df.aux$Time == "Different") 
  cat(paste0("M: A total of ", length(index2), " sets were longer than 24-h and will be also removed."), fill = 1)
  
  # Save output and remove sets:
  index <- sort(c(index1, index2)) 
  data$Circadian.set <- Circadian.set
  data$Circadian.set <- factor(data$Circadian.set, levels = c("Pre-dawn", "Dawn", "Day", "Dusk", "After-dusk"))
  data$Circadian.ret <- Circadian.ret
  data$Circadian.ret <- factor(data$Circadian.ret, levels = c("Pre-dawn", "Dawn", "Day", "Dusk", "After-dusk"))
  data$Soak.time <- Time.lapse
  data <- data[-index, ]


  #======================================================================#
  # Find periods of maximum moon angle in the sky during gear deployment #
  #======================================================================#
  data$Angle <- NA
  data$Max.moon.time <- data$Time.ret

  cat("Identifying times of maximum moon angle during fishing sets", fill = 1)
  pb <-  txtProgressBar(min = 0, max = nrow(data), initial = 0, style = 3, width = 60)
  for (i in 1:nrow(data)) {

    aux <- seq(data$Time.set[i], data$Time.ret[i], by = 1800) # Time interval every 30 min!
    aux <- c(aux, data$Time.ret[i]) # Include retrieval/catch time!

    angle.save <- NULL

    for(ii in 1:length(aux)) {
      aux1 <- getMoonPosition(date = aux[ii], lon = data$Lon[i], lat = data$Lat[i])
      aux1 <- aux1$altitude # * 57.2958 # From radians to degrees
      angle.save <- c(angle.save, aux1)
    }

    aux2 <- max(angle.save, na.rm = TRUE)
    if (aux2 < 0) { # Moon only bellow the horizon!
      aux2 <- 0
    }

    data$Angle[i] <- aux2
    data$Max.moon.time[i] <- aux[which(angle.save == max(angle.save))]
    setTxtProgressBar(pb, i)
  }
  close(pb)


  #==========================#
  # Obtaining moon variables #
  #==========================#
  data$Illumin <- NA
  data$Phase <- NA 
  data$Distance <- NA
  data$Moon.set.time <- data$Max.moon.time
  data$Parallatic <- NA
  data$Azimuth <- NA

  cat("Downloading moon variables", fill = 1)
  pb <-  txtProgressBar(min = 0, max = nrow(data), initial = 0, style = 3, width = 60)
  for (i in 1:nrow(data)) {

    # Get data
    aux1 <- getMoonIllumination(date = data$Max.moon.time[i])
    aux2 <- getMoonPosition(date = data$Max.moon.time[i], lon = data$Lon[i], lat = data$Lat[i])

    aux3 <- getMoonTimes(date = as.Date(data$Max.moon.time[i]) - 1, lon = data$Lon[i], lat = data$Lat[i])
    aux4 <- getMoonTimes(date = as.Date(data$Max.moon.time[i]), lon = data$Lon[i], lat = data$Lat[i])
    aux5 <- getMoonTimes(date = as.Date(data$Max.moon.time[i]) + 1, lon = data$Lon[i], lat = data$Lat[i])
    aux.time <- rbind(aux3, aux4, aux5)
    aux.time$Time <- difftime(aux.time$set, data$Max.moon.time[i], units = "hours")
    if (length(which(is.na(aux.time$Time) == TRUE)) > 0) {
      aux.time <- aux.time[-which(is.na(aux.time$Time) == TRUE), ]
    }
    aux.time$Time.abs <- aux.time$Time
    aux.time$Time.abs[aux.time$Time.abs < 0] <- aux.time$Time.abs[aux.time$Time.abs < 0] * -1

    data$Illumin[i] <- aux1$fraction
    data$Phase[i] <- aux1$phase
    data$Distance[i] <- aux2$distance
    data$Moon.set.time[i] <- aux.time$set[which(aux.time$Time.abs == min(aux.time$Time.abs))]
    data$Parallatic[i] <- aux2$parallacticAngle
    data$Azimuth[i] <- aux2$azimuth

    setTxtProgressBar(pb, i)
  }
  close(pb)


  #==========================#
  # Generate diagnostic plot #
  #==========================#
  cat("M: Saving diagnostic plot.", fill = 1)

  # Set durations:
  plot1 <- ggplot(data = data, aes(x = Soak.time)) + theme_bw() + labs(x = "Soak time (hours)", y = "Density") +
    geom_histogram(aes(y=..density..), fill = "dodgerblue", binwidth = 1, na.rm = TRUE) + 
    geom_density(na.rm = TRUE) + xlim(c(0, 23)) +
    geom_vline(xintercept = mean(df.moon$Soak.time), linetype = "dashed", size = 1)

  # Time difference between maximum angle to retrieval:
  data$Time.max.ret.h <- as.numeric(difftime(data$Time.ret, data$Max.moon.time, units = "hours"))
  plot2 <- ggplot(data = data, aes(x = Time.max.ret.h)) + theme_bw() + labs(x = "Time between maximum angle and retrieval (hours)", y = "") +
    geom_histogram(aes(y=..density..), fill = "dodgerblue", binwidth = 1, na.rm = TRUE) + 
    geom_density(na.rm = TRUE) +
    geom_vline(xintercept = mean(data$Time.max.ret.h), linetype = "dashed", size = 1) 

  # Maximum moon angles during set:
  plot3 <- ggplot(data = data, aes(x = Angle)) + theme_bw() + labs(x = "Maximum moon angle during set (rad)", y = "") +
    geom_histogram(aes(y=..density..), fill = "dodgerblue", binwidth = 0.1, na.rm = TRUE) + 
    geom_density(na.rm = TRUE) +
    geom_vline(xintercept = mean(data$Angle), linetype = "dashed", size = 1) 

  ### Save output 
  plot.save <- ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)
  output <- list(data = data, set.index = index, diagnostic = plot.save)
  return(output)
}

 


###################################################
# Testing the code!
moon.data <- MoonVar(data)  
df.moon <- moon.data[[1]]
summary(df.moon)
moon.data$diagnostic

kable(head(moon.data$data), format = "markdown")

ggsave("diagnostic.png", width = 12, height = 3)


### Generate histograms of variables distribution:
library(ggplot2)
library(ggpubr)





















