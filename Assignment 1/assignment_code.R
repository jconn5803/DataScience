library(tidyverse)

# Have R read the CSV File
birdDat <- read.csv("Assignemnt_1_data.csv")

##### DATA WRANGLING #####

# Change the years to the actual year
birdDat$year <- birdDat$year + 1969

#Create a dataframe for rural starling birds
starlingDat <- birdDat %>% 
  select(-c(housesparrow,northing,easting,temperature)) %>%  #Removes unneccasary columns
  filter(subrur == "-1") %>% # Filters for rural areas
  group_by(year) %>% #Groups all the samples in a year together
  mutate(sites_sampled = n_distinct(site)) %>% 
  mutate(yearlycount = sum(starling)) #Sums them together

#Create a dataframe for rural sparrow birds
sparrowDat <- birdDat %>% 
  filter(subrur == "-1") %>%
  select(-c(starling,northing,easting,temperature,subrur)) %>%  #Removes unneccasary columns
  
  
  group_by(year) %>% 
  mutate(sites_sampled = n_distinct(site)) %>% 
  mutate(yearlycount = sum(housesparrow)) 
  
 
sparrowDat <- sparrowDat %>% 
  select(year, sites_sampled, yearlycount) %>% 
  mutate(sparrowpersite = yearlycount/sites_sampled) %>% 
  distinct()

starlingDat <- starlingDat %>% 
  select(year, sites_sampled, yearlycount) %>% 
  mutate(starlingpersite = yearlycount/sites_sampled) %>% 
  distinct()
  
#Run a linear model
sparrow_lr <- lm(sparrowpersite ~ year, data = sparrowDat)
par(mfrow = c(2,2))
plot(sparrow_lr) # Plot to check linearity assumptions

#Repeat for starling
starling_lr <- lm(starlingpersite ~ year, data = starlingDat)
plot(starling_lr)

#Log transform the dependent variable
starlingDat <- starlingDat %>% 
  mutate(log = log(starlingpersite))
sparrowDat <- sparrowDat %>% 
  mutate(log = log(sparrowpersite))

#Log transofrmed linear model
log_sparrow_lr <- lm(log ~ year, data = sparrowDat)
plot(log_sparrow_lr) #plot to check linearity assumption

#Show coeffcients
summary(log_sparrow_lr)$coefficients

#Check p-value
if (summary(log_sparrow_lr)$coefficients[2,4] < 0.005){
  print('Reject null hypothesis for sparrows')
}else{
  print('Accept Null Hypothesis')
}

#same as above for starlings
log_starling_lr <- lm(log ~ year, data = starlingDat)
par(mfrow =c(2,2))
plot(log_starling_lr)

summary(log_starling_lr)$coefficients

if (summary(log_starling_lr)$coefficients[2,4] < 0.005){
  print('Reject null hypothesis for starlings')
}else{
  print('Accept Null Hypothesis')
}

####NON PARAMETRIC BOOTSTRAP####

#A function which does a non parametric bootstrap calculating the slopes of the linear models
lm_bootstrap <- function(nboot, df){
  
  bootstrap_slopes <- rep(0,nboot) #Initialise vector to store slopes
  
  
  for (i in 1:nboot){
    resampled_indices <- sample(1:nrow(df[5]), nrow(df[5]), replace = TRUE)
    
    # Extract the resampled data and independent variables
    resampled_data <- df[resampled_indices,5] #5th index accesses the log values
    resampled_years <- df[resampled_indices,1]#1st index accesses the years
    #Put into a vector
    resampled_data_vec <- pull(resampled_data, log)
    resampled_years_vec <- pull(resampled_years,year)
    
    # Fit a linear model to the resampled data
    lm_model <- lm(resampled_data_vec ~ resampled_years_vec)
    
    # Store the slope coefficient from the model
    bootstrap_slopes[i] <- coef(lm_model)["resampled_years_vec"]
    
    
    
  }
  return(bootstrap_slopes)
}

# Run the bootstraps for both species
sparrow_boot <-lm_bootstrap(1000,sparrowDat)
starling_boot <- lm_bootstrap(1000,starlingDat)

# Obtain the 99% CIs for the bootstraps
sparrow_conf_ints <- quantile(sparrow_boot,c(0.005,0.995))
starling_conf_ints <- quantile(starling_boot,c(0.005,0.995))

#Histogram of bootstraps
par(mfrow = c(1,1))
hist(sparrow_boot, main = paste("Linear Coefficients of Transformed Sparrow Data"), xlab = "Linear Coefficients" ,xlim = c(-0.045,-0.01))
abline(v = sparrow_conf_ints,col ="red", lwd = 3 )#Sparrow Confidence intervals
hist(starling_boot,main = paste("Linear Coefficients of Transformed Starling Data"), xlab = "Linear Coefficients" ,xlim = c(-0.045,-0.01) )
abline(v = starling_conf_ints,col ="red", lwd = 3 )#Starling confidence intervals

###Conclusive Plot
#Sparrow model coeffs
a = summary(log_sparrow_lr)$coefficients[1]
b = summary(log_sparrow_lr)$coefficients[2]
#Starling model coeffs
c = summary(log_starling_lr)$coefficients[1]
d = summary(log_starling_lr)$coefficients[2]

x<- seq(1970,2010,length.out = 1000) # x values to plot model estimate lines on 

plot(sparrowDat$year, sparrowDat$sparrowpersite,pch = 19, col = "green", ylim = c(0,20), xlab = "Year", ylab = "Birds per Site", main = paste("Plot of Birds per Site in A Given Year with Parametric Linear Models"))
lines(x,exp(a+b*x),col = "green") #Sparrow model
points(starlingDat$year, starlingDat$starlingpersite, pch = 19, col = "blue")
lines(x,exp(c+d*x),col = "blue")#Starling model




               


