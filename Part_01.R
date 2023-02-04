#Task_Week_03

# Set the Working Directory (WD)
setwd("C:/Users/user/Desktop/R_Training/Data")
getwd()

# Install Library
library(openxlsx)
library(readxl)

# import dataset from Excel

study_1 = read.xlsx("Tree_height.xlsx")
str(study_1)
View(study_1)


# import CSV file
getwd()
study_3 = read.csv("C:/Users/user/Desktop/R_Training/Data/Tree_height1.csv")
str(study_3)
View(study_3)

# subset of dataset

str(study_1)            # to see the datframe
unique(study_1)         #to see the all comun and row
range(study_1$year)     # to see the range of the column
sort(study_1$year)      # to se the all input in the coulmun

# study_1[n,]           #see the n'th row
# study_1[,n]           # see the n'th column
study_1[2,]             # see the 2nd row of study_1 dataset
study_2[,4]             # see 4th coulumn of the study_2 dataset

# create a subset from the main file study_1 with the year >2007
unique(study_1)

#subset

A = subset(study_1,study_1$year>2007)
unique(A$year)
unique(study_1$year)

# create a subset from the main file study_1 with the year >2005 & year <2010
A2= subset(study_1,study_1$year>2005 & study_1$year<2010)
unique(A2$year)

# Figure in R 
# Create a scatterplot in R

library(plotrix)
study_2 = read_excel("Tree_height.xlsx",sheet ="study_2")
View(study_2)
str(study_2)

# create a scatter plot with trend line(lm)
par(mfrow=c(1,1))
#mfrow = multiple figure per row
#mfcol = multiple figure per column
plot(study_2$Elevation_m, study_2$Diameter_cm,pch=10,col="Blue")
# col = colour
abline(lm((Diameter_cm)~(Elevation_m), data = study_2),col="red")
#~ tilde symbol, used to define the relationship between the 
#dependent variable and the independent variables

plot(study_2$species_richness,study_2$Elevation_m,pch=12,col="red")
abline(lm((species_richness)~(Elevation_m), data = study_2),col="yellow5")


# Plot 2 figure in a row
par(mfrow==c(1,2))
plot(study_2$Elevation_m,study_2$Height_m,pch=02,col="coral")
abline(lm((Elevation_m)~(Height_m),data=study_2)col="red")
plot(study_2$species_richness,study_2$Height_m,pch=12,col="green")
abline(lm((species_richness)~(Height_m),data=study_2),col="red")

#plot 2 figure in a column
par(mfrow=c(2,1))
plot(study_2$Height_m,study_2$Elevation_m,pch=10,col="red")
abline(lm((Elevation_m)~(Height_m),data=study_2),col="black")
plot(study_2$species_richness,study_2$Elevation_m,pch=12,col="coral")

#plot 4 figure 
par(mfrow=c(2,3))
plot(study_2$Height_m,study_2$Elevation_m,pch=10,col="red")
abline(lm((Elevation_m)~(Height_m),data=study_2),col="black")
plot(study_2$species_richness,study_2$Elevation_m,pch=12,col="coral")
plot(study_2$Height_m,study_2$Elevation_m,pch=10,col="red")
abline(lm((Elevation_m)~(Height_m),data=study_2),col="black")
plot(study_2$species_richness,study_2$Elevation_m,pch=12,col="coral")

plot(study_2$species_richness,study_2$Elevation_m,pch=12,col="coral")
plot(study_2$Height_m,study_2$Elevation_m,pch=10,col="red")
abline(lm((Elevation_m)~(Height_m),data=study_2),col="black")
plot(study_2$species_richness,study_2$Elevation_m,pch=12,col="coral")

library(sciplot)
str(study_2)

#create barplot by group
str(study_1)
par(mfrow=c(1,1))
bargraph.CI(x.factor = study_area,        # independent value, data in x axix
            response = species_richness,  # dependent value, data in y axis
            ylim = c(0,45),               # ylim: axis limit
            ylab = "No of Species",       # ylab= labeling x axis
            xlab = "Study Area",          # xlab = labelling y axis
            las=1,                        #las = x and y axis labelling style, it has three position(1,2,3) 1 = both side are perfect
            data =study_2,
            col = c("red","green","coral"),
            main = "p=0.434"
            ) 

str(study_1)
bargraph.CI(
  x.factor = treatment,
  response = biomass,
  group = plant,
  ylab = "Treatment of the Study",
  xlab = " Biomass",
  ylim = c(0,250),
  las =1,
  data = study_1,
  col = c ("black","coral"),
  main ="Barplot",
  legend = T
  
)

# Create line plot

str(study_1)
par(mfrow=c(1,1))
lineplot.CI(
  (year),
  biomass,
  ylim = c(70,250),
  group = plant,
  xlab = "Year",
  ylab = "Biomass",
  x.cont = T,
  legend = T,
  data = study_1,
  main = " Line Plot",
  col = c("red3","green","green3")
)
