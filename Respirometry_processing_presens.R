##Author Craig Norrie - cnorrie@uw.edu
#Last modified: May 2024 to make this easier to apply to multiple experiments
#This code will read in data from a respirometery files that wwere produced using the JPG lab presense respirometer
#one file containeing each probe for each run should exist and a master log file with the info for each animal should exist
#Templates for the log file is in the repo names "Resplog_Template_CSV.csv"
#the respirometry data files should be named in order. i.e. the first file should correspond to run #1


###
experiment_name <- "Multistressor_2024"
# Load packages ------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(lubridate)
library(respR) # I had to install this from github devtools::install_github("januarharianto/respR")
library(here)

# Read in all the data ----------------------------------------------------
rawfiles <- list.files(here("Raw_data"), 
                       all.files = FALSE) #Creates a vector with all the filenames from respirometry that are stored in the "raw_data" folder. 
respdat_all <- data.frame()#Creates an empty data frame to store respirometry data

#The for loop here will read in each of the data files 
for(i in 1:length(rawfiles)){
  tempdat <- read.csv(file = here("Raw_data", rawfiles[i]), skip = 1)
  tempdat <- slice(tempdat, 1:(n() - 2))#removes the last two rows becasue it is junk
  tempdat$run <- i#add a new column based on the number sheet that it is. i.e. run 1, run 2. **important that sheets are named in order of resplog**
  respdat_all <- rbind(respdat_all, tempdat)#used plyr::rbind instead of dplyr::bind_rows because this deals with duplicate column names
  rm(tempdat)
}

##The next section of the code will split the single large data frame into individual DFs for each run. They will be named in the order they were read in. 
# This code could be made more efficent by renaming the sheets in the initial read in and not putting them all in the same DF.
respdat_all$run <- as.factor(respdat_all$run)
runlist <- levels(respdat_all$run)#Gets a list of all the run numbers. Check that this all your runs you were expecting are here. I.e. if you did 28 runs the max number should be 28. 
for(i in 1:length(runlist)){
  df <- respdat_all %>% select(delta_t,Channel,Value, run) %>% filter(run == runlist[i])
  df <- df %>% select(delta_t,Channel,Value) %>% pivot_wider(names_from = Channel, values_from = Value)
  df$delta_t <- as.numeric(df$delta_t)
  assign(paste('run_',i,sep=''),df) #creates a df that should match what your initial
}
#create an empty df to store processed data from all runs****Just do this once at the start of the experiment
allratesdat <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("probe", "rate", "run"))))
# Calculate oxygen uptake rates using the RespR package -------------------
###https://januarharianto.github.io/respR/index.html
#Set the run number here
run_no <- 2
run_num <- get(paste("run_", run_no, sep = "")) #Put the number of the run that you are caclulcating rate for here
df.name <- paste(run_no) ##Update this each time you update the above
#Inspect the runs to determine an appropriate start and end time
inspect(run_num, time = 1, oxygen = 2:ncol(run_num))
#Set the start and end times over the period of o2 consumption based on inspection of data
start_time <- 10
end_time <- 40

######YOU SHOULD NOT HAVE TO CHANGE ANY OF THE BELOW CODE FOR EACH RUN
#Calculate oxygen uptake levels for each probe over the specified period
#What is the rate over a specific 25 minute period?” from = 4, to = 29, by = "time"
#Create an empty data frame to save the results of the loop
respdf <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("probe", "rate"))))
#i should be in the first column of the df and tempoxydf$summary$rate should be in the second
for(i in 2:ncol(run_num)){
  tempoxy <- inspect(run_num, time = 1, oxygen = i)
  oxrate <- calc_rate(tempoxy, from = start_time, to = end_time)#loop this for each probe in each run and save the data
  tempoxydf <- data.frame(matrix(ncol=3,nrow=1, dimnames=list(NULL, c("probe", "rate","run"))))#Creates a temporary df to store results
  tempoxydf$probe <- colnames(run_num)[i]
  tempoxydf$rate <- oxrate$summary$rate
  tempoxydf$run <- df.name
  respdf <- rbind(respdf, tempoxydf)
}
#rename the df to the name of teh run that you just used
assign(paste('rates_',df.name,sep=''),respdf)#names the df with the run name

##Add all this data to one big data frame 

allratesdat <- rbind(allratesdat, respdf)

# Match the respiometery log with the rates -------------------------------
#Read in your respirometry log - make sure that it is in the appropiate folder
resplog <- read.csv(here("Resplog_CSV.csv"))
#ensure data classes are the same - not really necessary
resplog$probe <- as.factor(resplog$probe)
resplog$run <- as.factor(resplog$run)
allratesdat$run <- as.factor(allratesdat$run)
allratesdat$probe <- as.factor(allratesdat$probe)
#Merge the data to one df
animalratedat <- merge(allratesdat, resplog, by = c("probe","run"))
animalratedat$Ploidy <- as.factor(animalratedat$Ploidy)
animalratedat$Treatment <- as.factor(animalratedat$Treatment)
###############Code below here may need to be checked. This is to show how to background correct.

Merge the data to one df
animalratedat <- merge(allratesdat, resplog, by = c("probe","run"))
animalratedat <- animalratedat %>% select(Tank, Colour, Treatment, rate, Oyster.tissue.dry.mass)
#Correct for blanks
blanks <- animalratedat %>% filter(Colour == "Blank") %>% group_by(Tank) %>% summarise(meanbgrd = mean(rate)) %>% select(Tank, meanbgrd)
animalratedat <- merge(animalratedat, blanks, by = "Tank")
animalratedat$bgrdcorrectedrate <- animalratedat$rate - animalratedat$meanbgrd



