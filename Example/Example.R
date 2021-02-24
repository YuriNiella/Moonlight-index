#============================================#
# Calculating standardized moonlight indexes # 
# Author: Yuri Niella                        #
#============================================#

load("Moonlight_index.Rdata") 


  #======================================================================#
  # Find periods of maximum moon angle in the sky during gear deployment #
  #======================================================================#
  data$Max.moon.time <- data$Time.ret
  data$Angle <- NA

  cat("Identifying times of maximum moon angle during fishing sets", fill = 1)
  pb <-  txtProgressBar(min = 0, max = nrow(data), initial = 0, style = 3, width = 60)
  for (i in 1:nrow(data)) {

    aux.temp <- as.numeric(difftime(data$Night.end[i], data$Night.start[i], units = "mins"))
    if (aux.temp < 30) {
      aux <- c(data$Night.start[i], data$Night.end[i]) # If lower than 30 min, use only night-time and retrieval angles!
    } else {
      aux <- seq(data$Night.start[i], data$Night.end[i], by = 1800) # Time interval every 30 min
      aux <- c(aux, data$Time.ret[i]) # Include retrieval/catch time!
    }
    
    angle.save <- NULL
    for(ii in 1:length(aux)) {
      aux1 <- getMoonPosition(date = aux[ii], lon = data$Lon[i], lat = data$Lat[i])
      aux1 <- aux1$altitude * 57.2958 # From radians to degrees
      angle.save <- c(angle.save, aux1)
    }

    aux2 <- max(angle.save, na.rm = TRUE)
    if (aux2 < 0) { # Moon bellow the horizon!
      aux2 <- 0
    }

    data$Angle[i] <- aux2
    data$Max.moon.time[i] <- aux[which(angle.save == max(angle.save))]
    setTxtProgressBar(pb, i)
  }
  close(pb)


  #--------------------------#
  # Obtaining moon variables #
  #--------------------------#
  data$Illumination <- NA
  data$Phase <- NA 
  data$Distance <- NA
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

    # Get proportion of lunar illumination 
    aux.shift <- as.numeric(substr(data$Max.moon.time, 12, 13))
    if (aux.shift > 12) {
      aux.shift <- aux.shift - 12
    }
    aux <- as.numeric(substr(data$Max.moon.time, 15, 16)) / 60
    aux.shift <- aux.shift + aux

    # Save data:
    data$Illumination[i] <- aux1$fraction
    data$Phase[i] <- aux1$phase
    data$Distance[i] <- aux2$distance
    data$Parallatic[i] <- aux2$parallacticAngle * 57.2958 # From radians to degrees
    data$Azimuth[i] <- aux2$azimuth * 57.2958 # From radians to degrees
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  #-------------------------#
  # Create diagnostic plots #
  #-------------------------#
  plot1 <- ggplot() + theme_bw() +
    geom_histogram(data = data, aes(x = Soak.time), binwidth = 2, position="identity", fill = "midnightblue") +
    geom_vline(xintercept = mean(data$Soak.time), linetype = "dashed", size = 1, colour = "white") + 
    labs(x = "Soak time (hours)", y = "Frequency", title = paste0("mean = ", substr(mean(data$Soak.time), 1, 5), " h")) +
    scale_x_discrete(limits = seq(0, 24, 4))

  plot2 <- ggplot() + theme_bw() +
    geom_histogram(data = data, aes(x = Angle), binwidth = 5, position="identity", fill = "midnightblue") +
    geom_vline(xintercept = mean(data$Angle), linetype = "dashed", size = 1, colour = "white") + 
    labs(x = "Maximum moon angle (°)", y = "Frequency", title = paste0("mean = ", substr(mean(data$Angle), 1, 5), "°"))
    
  plot.diag <- ggarrange(plot1, plot2, ncol = 2)
      
  # Save outputs
  output <- list(data = data, bad_set.index = index, plot.diag = plot.diag)
  return(output)
}


### CALCULATING TOTAL MOON LIGHT INDEXES
#' x = Lunar illumination (%)
#' y = Maximum angle (degrees)
#' z = Cloud cover (%)
#' 
MoonCalc <- function(x, y, z) {
  (x * sin(y * 0.0174532925)) * 1 * ((1 - z) / 1)
}


### CALCULATING WEIGHTED MOON LIGHT INDEXES
#' data = Dateframe returned by MoonVar() 
#' angle = Angle that the moon has to overcome 
#' 
MoonIndex <- function(data, angle) {

  data$MoonIndex <- NA

  cat("Calculating weighted moonlight index", fill = 1)
  pb <-  txtProgressBar(min = 0, max = nrow(data), initial = 0, style = 3, width = 60)
  for (i in 1:nrow(data)) {

    if (data$Angle[i] == 0 |
      data$Angle[i] < angle) {
      data$MoonIndex[i] <- 0
    } else {

      data$MoonIndex[i] <- MoonCalc(x = data$Illumination[i], y = data$Angle[i], z = data$Cloud[i])
    }

    setTxtProgressBar(pb, i)
  }
  close(pb)

  return(data)
}



###################################################
# Testing the code!


data <- subset(df.sdl, select = c("lon_beg", "lat_beg", "dateTime_set.UTC", "Time.catch.UTC", "Cloud"))
names(data) <- c("Lon", "Lat", "Time.set", "Time.ret", "Cloud")
data$Cloud <- data$Cloud/100
data$Time.ret[9123] <- data$Time.ret[9123] + 86400 # Correct retrieval time wrong!

# Get lunar variables:
moon.data <- MoonVar(data)  
df.moon <- moon.data[[1]]

# Calculate weighted moonlight index:
df.moon.index <- MoonIndex(data = df.moon, angle = 8.31) # Using a 8.31 angle!

# Merge both datasets:
index <- moon.data[[2]]
df.moon <- df.sdl[-index, ]
df.moon$MoonIndex <- df.moon.index$MoonIndex

# Analysis:
mod <- gam(CCE ~ s(MoonIndex, k = 5), family = ziP)
mod1 <- glmmTMB(CCE ~ MoonIndex + (1 | Year), ziformula = ~1, family = binomial, data = df.moon)
mod2 <- gam(CCE ~ s(MoonIndex, k = 5), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod3 <- gamm(CCE ~ s(MoonIndex, nsst, k = 15), offset = log(df.moon$effort_h), 
  family = binomial, data = df.moon, random=list(Year = ~1))

AIC(mod1, mod2, mod3$lme) # Check better model: ZIGLMM!!!


## MODELLING:

mod1 <- gam(TIG ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod2 <- gam(CCE ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod3 <- gam(SPL ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod4 <- gam(RCD ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod5 <- gam(STI2 ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod6 <- gam(GBA ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)
mod7 <- gam(NXI ~ MoonIndex + s(Year, bs = "re"), offset = log(df.moon$effort_h), family = binomial, data = df.moon)

summary(mod1)
summary(mod2)
summary(mod3) # Not significant!
summary(mod4)
summary(mod5)
summary(mod6) # Not significant!
summary(mod7) 


# PLOT:
plot1 <- visreg(mod1, xvar = "MoonIndex", partial = FALSE, gg = TRUE, rug = TRUE, type = "contrast",
       line = c(lwd = 2, col = cmocean("haline")(7)[1]), 
       fill.par = c(fill = rgb(col2rgb(cmocean('haline')(7)[1])[1,], 
  col2rgb(cmocean('haline')(7)[1])[2,], 
  col2rgb(cmocean('haline')(7)[1])[3,], max = 255, alpha = 125))) + 
  theme_bw() + labs(x = "Moonlight index (%)", y = "Standardized residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-5, 2.5))

plot2 <- visreg(mod2, xvar = "MoonIndex", partial = FALSE, gg = TRUE, rug = TRUE, type = "contrast",
       line = c(lwd = 2, col = cmocean("haline")(7)[2]), 
       fill.par = c(fill = rgb(col2rgb(cmocean('haline')(7)[2])[1,], 
  col2rgb(cmocean('haline')(7)[2])[2,], 
  col2rgb(cmocean('haline')(7)[2])[3,], max = 255, alpha = 125))) + 
  theme_bw() + labs(x = "Moonlight index (%)", y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-5, 2.5))

plot3 <- visreg(mod4, xvar = "MoonIndex", partial = FALSE, gg = TRUE, rug = TRUE, type = "contrast",
       line = c(lwd = 2, col = cmocean("haline")(7)[4]), 
       fill.par = c(fill = rgb(col2rgb(cmocean('haline')(7)[4])[1,], 
  col2rgb(cmocean('haline')(7)[4])[2,], 
  col2rgb(cmocean('haline')(7)[4])[3,], max = 255, alpha = 125))) + 
  theme_bw() + labs(x = "Moonlight index (%)", y = "Standardized residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-5, 2.5))

plot4 <- visreg(mod5, xvar = "MoonIndex", partial = FALSE, gg = TRUE, rug = TRUE, type = "contrast",
       line = c(lwd = 2, col = cmocean("haline")(7)[5]), 
       fill.par = c(fill = rgb(col2rgb(cmocean('haline')(7)[5])[1,], 
  col2rgb(cmocean('haline')(7)[5])[2,], 
  col2rgb(cmocean('haline')(7)[5])[3,], max = 255, alpha = 125))) + 
  theme_bw() + labs(x = "Moonlight index (%)", y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-5, 2.5))

plot5 <- visreg(mod7, xvar = "MoonIndex", partial = FALSE, gg = TRUE, rug = TRUE, type = "contrast",
       line = c(lwd = 2, col = cmocean("haline")(7)[7]), 
       fill.par = c(fill = rgb(col2rgb(cmocean('haline')(7)[7])[1,], 
  col2rgb(cmocean('haline')(7)[7])[2,], 
  col2rgb(cmocean('haline')(7)[7])[3,], max = 255, alpha = 125))) + 
  theme_bw() + labs(x = "Moonlight index (%)", y = "") +
  geom_hline(yintercept = 0, linetype = "dashed") + ylim(c(-5, 2.5))


ggarrange(plot1, plot2, NA, plot3, plot4, plot5, 
  nrow = 2, ncol = 3, labels = c("A", "B", "", "C", "D", "E"))
ggsave("Output/Final plots/Analysis Reunion/Moon_mod.png", width = 20, height = 13, units ="cm", dpi = 300)





