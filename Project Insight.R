install.packages("RColorBrewer")
install.packages('dplyr')
library(RColorBrewer)
library(readr)
library(dplyr)

project.insight=read.csv("http://datasets.barrymonk.com/MATH3440/MCU.csv")
View(project.insight)
summary(project.insight$superhero_activity)
mean(project.insight$superhero_activity)

quantile(project.insight$villain_activity, 0.80)
quantile(project.insight$superhero_activity, 0.80)

#Histogram showing Advanced Tech is right skewed
hist(project.insight$advanced_tech, main= "Advanced Tech Histogram",
     xlab = "", col= brewer.pal(n=4, name = "Pastel1"), las=2)

#Horizontal box plot identifying any outliers present in distribution
#of advanced tech measurements
boxplot(project.insight$advanced_tech, main= "Advanced Tech Measurements Boxplot", 
        horizontal = TRUE, col="dodgerblue")

#Bar plot displaying frequency of incidents involving mystical arts relative to 
#their locations
par(mar=c(5,6,4,4) + 0.1)
barplot(project.insight$mystical_arts, names.arg = project.insight$location,
        main = "Frequency of Mystical Arts Relative to their Locations",
        xlab = "Mystical Arts Count", ylab = "", las= 1, horiz = TRUE,
        col = "Blue", xlim = c(0, 60), cex.names = .8)

#Pie chart of random locations sample and the concentration of advanced tech.
#relative to this sample
rand.location<- sample(project.insight$location, 5, replace = FALSE)
Nlocation<- filter(project.insight, location %in% rand.location)
pie(Nlocation$superhero_activity, labels = Nlocation$location, main = 'Random
    Locations with Respective Advanced Technologies', col = brewer.pal(n=5, name="Accent"))

#Additional annual report of villain activity and the new 80th percentile
new_villain<- c(127.48 + project.insight$villain_activity)
quantile(new_villain, 0.80)

#Create two new data sets, one showing only locations with infinity stones,
#and the other showing locations with no infinity stones
stone<- filter(project.insight, project.insight$infinity_stones == '1')
View(stone)
nostone<- filter(project.insight, project.insight$infinity_stones == '0')
View(nostone)

#Scatter plot of superhero activity vs villain activity in locations without
#infinity stones.
plot(nostone$superhero_activity, nostone$villain_activity, pch = 16,
     las= 1, xlab = 'Superhero Activity', ylab = 'Villain Activity', 
     xlim = c(25,60), ylim = c(0,50))
cor(nostone$superhero_activity, nostone$villain_activity)