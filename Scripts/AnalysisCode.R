library(ggplot2)
library(patchwork)
library(tidyverse)
library(quantmod)
library(dplyr)
library(ggrepel) 
library(segmented)
library(segmented)

#combining data
{
  setwd("~/Documents/R/ThesisCrab/CrabHRData")
  
  #creates data frame with all fulldata (heartrates of all trials), doesn't include trial 30 currently
  fulldata <- list.files(pattern = "fulldata.csv") 
  heartrates <- do.call(rbind, lapply(fulldata, read.csv))
  
  #loads and assigns ID to each invidividual
  #my_data <- read.csv("ThesisCombined.csv")
  #my_data$snail_id = paste(my_data$Trial, my_data$Input, sep="_")
  meta <- read.csv("MetaHR.csv")
  
  heartrates$uniqueID <- paste(heartrates$Input, heartrates$Trial, sep="_")
  meta$uniqueID <- paste(meta$Input, "Trial", sep = "_")
  meta$uniqueID <- paste(meta$uniqueID, meta$Trial, sep = "")
  
  #removes all discards
  heartrates=subset(heartrates,confidence!="Discard")
  
  #error by NAs? -> possible remove columns with NAs
  #heartrates = subset(heartrates, select = -c(freq, start_time, end_time, recording_length, time))
  
  #combine heartrates df and meta df
  heartrates=merge(heartrates, meta, by="uniqueID", all.x = TRUE) 
}

{
  #individuals that Arrehneius linear regression code won't work on, but I can fix
  
  #removal from dataset individuals
}

#translates to ABTs
library(weathermetrics) #Install this package if you dont have it already
heartrates$x=1000/celsius.to.kelvin(heartrates$temperature) #creates x axis with temp in 1000/kelvin
heartrates$y=log(heartrates$Heartrate+1) # creates y axis with nat log of hr

individuallist <-unique(heartrates$uniqueID)

#par(mfrow=c(4,4))

#selgemented function (ABT)
for(i in individuallist[1:length(individuallist)]) {
  indiv=subset(heartrates,heartrates$uniqueID == i)
  out.lm<-lm(y~x, data=indiv)
  print("-----------------------------------------") #I added these "print" lines to create annotations in the console, but they can be removed
  print(i)
  print("
        ")
  os<-selgmented(out.lm, Kmax=3, type="bic", bonferroni=TRUE)
  plot(y~x, data=indiv, main=i)
  plot(os, add=T, lwd=1.6)
  points(os, add=T, lwd=2,col=c("red","blue","green","yellow"),cex=3)
}

a=0

#selgemented function (regulatr data)
for(i in individuallist[1:length(individuallist)]) {
  indiv=subset(heartrates,heartrates$uniqueID == i)
  print(i)
  out.lm<-lm(Heartrate~temperature, data=indiv)
  print("-----------------------------------------") #I added these "print" lines to create annotations in the console, but they can be removed
  print(i)
  print("
        ")
  os<-selgmented(out.lm, Kmax=4, type="bic", bonferroni=TRUE)
  plot(Heartrate~temperature, data=indiv, main=i)
  plot(os, add=T, lwd=1.6)
  points(os, add=T, lwd=2,col=c("red","blue","green","yellow"),cex=3)
}

#In the below script, the "segmented' function fits a different number of breakpoints for each individual as specified in an "n.breaks" column on the datasheet
#use this once you have determined the number of breakpoints for each individual, and want to double check you picked the right amount
for(i in individuallist[1:length(individuallist)]) {
  indiv=subset(heartrates,heartrates$uniqueID==i)
  out.lm<-lm(y~x, data=indiv)
  os<-segmented(out.lm, npsi=first(indiv$Breakpoints))
  plot(y~x,data=indiv, main=first(indiv$uniqueID))
  plot(os, add=T, lwd=1.6)
  points(os, add=T, lwd=2,col=c("red","blue","green","yellow"),cex=3)
}

#collect data from regression lines huge code from here
{
  library(weathermetrics)
  
  heartrates$Individual=heartrates$uniqueID
  
  #divide your data into three different files based on the number of breakpoints (specified in the column "n.breaks.arrhenius")
  singleonly=subset(heartrates,Breakpoints=="1")
  tripleonly=subset(heartrates,Breakpoints=="3")
  doubleonly=subset(heartrates,Breakpoints=="2")
  
  #Create seperate individual lists for each file 
  singleonly <-unique(singleonly$Individual)
  tripleonly <-unique(tripleonly$Individual)
  doubleonly <-unique(doubleonly$Individual)
  
  #script for single break individuals
  #---------------
  
  # 3. run loop - subsets the data frame for each sample and adds the results to the lists for lm(), segmented(), 
  # predicted values from segmented lines, and breakpoints contained under $psi of the segmented results 
  
  #create/ clear storage files
  fit <- list()
  fit.seg <- list()
  predicted.seg <- list()
  breakpoints <- data.frame(Psi=as.numeric(),ABT=as.numeric(),SE=as.numeric())
  
  #script to calculate predicted heartrate at breakpoint, and slopes, in single bp individuals
  
  {breakpointhr <- list()
    endslope <- list()
    initialslope <- list()
    breakpointslist  <- list()
    intercepts <- list()
  } #creat lists to hold data
  
  for(i in singleonly[1:length(singleonly)]) {
    fit.seg.A1<-list()
    fit[[i]] <- lm(y~x, data=subset(heartrates,heartrates$Individual==i))
    fit.seg[[i]]<-segmented(fit[[i]], npsi=1, data=heartrates, na.action=na.exclude)
    breakpoints[i,] <- fit.seg[[i]]$psi
    fit.seg.A1<-fit.seg[[i]]
    breakpointhr[[i]]= fit.seg.A1$coefficients[1] + fit.seg.A1$coefficients[2] * fit.seg.A1$psi[2] #gives y1)
    endslope[[i]]=fit.seg.A1$coefficients[2] # initial slope
    initialslope[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]) # initial slope (need to change code for double endpoints)
  } #run script
  
  library (plyr)
  library(weathermetrics)
  
  {
    breakpointhr <- ldply (breakpointhr, data.frame)
    endslope <- ldply (endslope, data.frame)
    initialslope <- ldply (initialslope, data.frame)
    
    breakpoints$Individual <- rownames(breakpoints) 
    breakpoints$Individual=as.factor(breakpoints$Individual)
    colnames(breakpointhr)[1] <- "Individual"
    colnames(endslope)[1] <- "Individual"
    colnames(endslope)[2] <- "endslope"
    colnames(initialslope)[1] <- "Individual"
    colnames(initialslope)[2] <- "initialslope"
    colnames(breakpointhr)[2] <- "finalbphr"
    
    breakpointslist=merge(breakpoints, breakpointhr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, endslope, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, initialslope, by="Individual", all = T) # merge breakpoint data with breakpointhr and slopes
    breakpointslist$ABT=1000/breakpointslist$ABT
    breakpointslist$ABT=kelvin.to.celsius(breakpointslist$ABT) #converts predicted breakpoint back to celsius
    breakpointslist$finalbphr=exp(breakpointslist$finalbphr)
  }
  singlebreakpointlist=breakpointslist
  
  
  #script for double break individuals
  #---------------
  
  # 3. run loop - subsets the data frame for each sample and adds the results to the lists for lm(), segmented(), 
  # predicted values from segmented lines, and breakpoints contained under $psi of the segmented results 
  
  #create/ clear storage files
  
  library(segmented)
  
  {fit <- list()
    fit.seg <- list()
    predicted.seg <- list()
    breakpoints <- data.frame(Psi=as.numeric(),Psi1=as.numeric(),ABT=as.numeric(),ABT1=as.numeric(),SE1=as.numeric(),SE=as.numeric())
    finalbphr <- list()
    breakpoint1hr  <- list()
    slope1 <- list()
    endslope <- list()
    initialslope <- list()
    breakpointslist  <- list()
    intercepts <- list()
  } #creat lists to hold data
  
  for(i in doubleonly[1:length(doubleonly)]) {
    fit.seg.A1<-list()
    fit[[i]] <- lm(y~x, data=subset(heartrates,heartrates$Individual==i))
    fit.seg[[i]]<-segmented(fit[[i]], npsi=2, data=heartrates, na.action=na.exclude)
    breakpoints[i,] <- fit.seg[[i]]$psi
    fit.seg.A1<-fit.seg[[i]]
    finalbphr[[i]]= fit.seg.A1$coefficients[1] + fit.seg.A1$coefficients[2] * fit.seg.A1$psi[3] #gives finalbphr)
    endslope[[i]]=fit.seg.A1$coefficients[2] # endslope (fixed)
    slope1[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]) #slope1 (fixed)
    initialslope[[i]]=(fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3]+fit.seg.A1$coefficients[4]) # initial slope
    breakpoint1hr[[i]]= fit.seg.A1$coefficients[1] -(fit.seg.A1$coefficients[3])*fit.seg.A1$psi[3] + (fit.seg.A1$coefficients[2]+fit.seg.A1$coefficients[3])*fit.seg.A1$psi[4] #gives y2
    
  } 
  
  #run script
  
  
  library (plyr)
  
  {finalbphr <- ldply (finalbphr, data.frame)
    breakpoint1hr <- ldply (breakpoint1hr, data.frame)
    slope1 <- ldply (slope1, data.frame)
    endslope <- ldply (endslope, data.frame)
    initialslope <- ldply (initialslope, data.frame)
    breakpoints$Individual <- rownames(breakpoints) 
    breakpoints$Individual=as.factor(breakpoints$Individual)
    colnames(finalbphr)[1] <- "Individual"
    colnames(slope1)[1] <- "Individual"
    colnames(slope1)[2] <- "slope1"
    colnames(endslope)[1] <- "Individual"
    colnames(endslope)[2] <- "endslope"
    colnames(initialslope)[1] <- "Individual"
    colnames(initialslope)[2] <- "initialslope"
    colnames(finalbphr)[1] <- "Individual"
    colnames(finalbphr)[2] <- "finalbphr"
    colnames(breakpoint1hr)[1] <- "Individual"
    colnames(breakpoint1hr)[2] <- "breakpoint1hr"
    
    breakpointslist=merge(breakpoints, finalbphr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, breakpoint1hr, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, slope1, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, endslope, by="Individual", all = T)
    breakpointslist=merge(breakpointslist, initialslope, by="Individual", all = T) # merge breakpoint data with breakpointhr and slopes
    breakpointslist$ABT=1000/breakpointslist$ABT
    breakpointslist$ABT=kelvin.to.celsius(breakpointslist$ABT)
    breakpointslist$ABT1=1000/breakpointslist$ABT1
    breakpointslist$ABT1=kelvin.to.celsius(breakpointslist$ABT1)
    breakpointslist$breakpoint1hr=exp(breakpointslist$breakpoint1hr)
    breakpointslist$finalbphr=exp(breakpointslist$finalbphr)
  }
  
  doublebreakpointlist=breakpointslist
  
  #script for triple break individuals
  #---------------deleted since no individuals had 3 BPs
  
  #(check that the correct ABTs from the double and triple breakpoint individuals are placed into the same column as ABTs for the single breakpoint individuals)
  {
    singlebreakpointlist$breakpoints="single"
    doublebreakpointlist$breakpoints="double"
    allbreakpointlist=merge(singlebreakpointlist,doublebreakpointlist, all = T)
    #allbreakpointlist contains all the info :)
  }
  
}
#may need to divide individuals up by species and trial then analyze heart rate and breaking points
allbreakpointlist2 = allbreakpointlist #backup heartrate data

#heartrates2 has all meta data backed up
#heartrates3 has just flatline rows of all individuals
heartrates = merge(heartrates, allbreakpointlist)
heartrates2 = heartrates

#--------------------------------------------------

flatlines <- subset(heartrates, heartrates$confidence == "Zero")

{
  #exceptions - temperature recording
  flatlines <- subset(flatlines, flatlines$uniqueID != "1_Trial6")
  flatlines <- subset(flatlines, flatlines$uniqueID != "1_Trial14")
  flatlines <- subset(flatlines, flatlines$uniqueID != "1_Trial20")
  
  #Trial 3 input 1 - FLT is later, I saw it with my own eyes too tired to go thru it now (HS)
  #Trial 21 input 0 - FLT difficult to parse thru noise, ABT reliable (CM)
  #Trial 17 input 0 - FLT difficult to parse thru noise, ABT reliable (CM)
}

{
  flatlines <- subset(flatlines, flatlines$uniqueID != "1_Trial3")
  flatlines <- subset(flatlines, flatlines$uniqueID != "0_Trial21")
  flatlines <- subset(flatlines, flatlines$uniqueID != "0_Trial17")
}

library(emmeans)



table(flatlines$Species, flatlines$Sex)

#basic graphs overviewing ABT and FLT among the three species
ggplot(flatlines, aes(x = Species, y = temperature, fill = Species)) + geom_boxplot()

ggplot(flatlines, aes(x = Species, y = ABT, fill = Species)) + geom_boxplot()

#simple modeling for ABT and flatlines
mod <- glm(temperature ~ Species, data = flatlines)
summary(mod)
par(mfrow = c(2,2))
plot(mod)
emm <- emmeans(mod, ~ Species)
pairs(emm)

mod <- glm(ABT ~ Species, data = flatlines)
summary(mod)
par(mfrow = c(2,2))
plot(mod)
emm <- emmeans(mod, ~ Species)
pairs(emm)

#removing individuals whose data did not get recorded (temperature) properly results in a signficant difference between green crabs and shore crabs
#removing individuals removes significance while keeping them keeps that signifiance between the two groups


ggplot(flatlines, aes(x = WW, y = temperature, color = Species, label = uniqueID)) + 
  geom_point()+
  geom_text(hjust=0, vjust=0)

ggplot(flatlines, aes(x = WW, y = ABT, color = Species, label = uniqueID)) + 
  geom_point()+
  geom_text(hjust=0, vjust=0)


#3 obvious outliers, 1-3, 0-17, 0-21
