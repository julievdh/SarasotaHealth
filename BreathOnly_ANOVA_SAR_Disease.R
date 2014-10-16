# Disease Plot
# Load Data
# SHOULD ONLY COMPARE WITHIN DECK/WATER? 
breaths <- read.table("c:/Users/Julie/Documents/BreathSounds/BreathOnly_SAR_34.csv", header=TRUE, sep = ",")

# Convert these to factor values
An <- factor(breaths$Animal)
Cond <- factor(breaths$State)
Pneum <- factor(breaths$Pneumo.ON.OFF)
Dis <- factor(breaths$Disease)

# Load Libraries
library(agricolae)

# RUN ANOVAS
a1E <- aov(breaths$e.time_window ~ Dis + Cond + Pneum)
a2E <- aov(breaths$e.EFD ~ Dis + Cond + Pneum)
a3E <- aov(breaths$e.RMS ~ Dis + Cond + Pneum)
a4E <- aov(breaths$e.PP ~ Dis + Cond + Pneum)
a5E <- aov(breaths$e.Fcent ~ Dis + Cond + Pneum)

a1I <- aov(breaths$i.time_window ~ Dis + Cond + Pneum)
a2I <- aov(breaths$i.EFD ~ Dis + Cond + Pneum)
a3I <- aov(breaths$i.RMS ~ Dis + Cond + Pneum)
a4I <- aov(breaths$i.PP ~ Dis + Cond + Pneum)
a5I <- aov(breaths$i.Fcent ~ Dis + Cond + Pneum)

# plot
pdf("DiseaseEffect_SAR_2.pdf",width = 4,height = 8)
par(mfrow=c(5,2))
par(mar = c(0.5,4,0.5,2))
boxplot(breaths$e.Fcent ~ Dis, ylim = c(000,3200), 
ylab = "Centroid Frequency (Hz)",axes = F,col = "grey")
axis(2)
tDis <- HSD.test(a5E,'Dis')
text(c(1,2,3,4),3200,tDis$groups$M)


boxplot(breaths$i.Fcent ~ Dis, ylim = c(000,3200), 
axes = F)
tDis <- HSD.test(a5I,'Dis')
text(c(1,2,3,4),3200,tDis$groups$M)

boxplot(breaths$e.PP ~ Dis, ylim = c(170,220), 
ylab = "PP Amplitude (dB re 1 uPa)",axes = F,col = "gray")
axis(2)
tDis <- HSD.test(a4E,'Dis')
text(c(1,2,3,4),220,tDis$groups$M)

boxplot(breaths$i.PP ~ Dis, ylim = c(170,220),axes = F)
tDis <- HSD.test(a4I,'Dis')
text(c(1,2,3,4),220,tDis$groups$M)

boxplot(breaths$e.RMS ~ Dis, ylim = c(150,190), 
ylab = "RMS Amplitude (dB re 1 uPa)",axes = F,col = "gray")
axis(2)
tDis <- HSD.test(a3E,'Dis')
text(c(1,2,3,4),190,tDis$groups$M)

boxplot(breaths$i.RMS ~ Dis, ylim = c(150,190),axes = F)
tDis <- HSD.test(a3I,'Dis')
text(c(1,2,3,4),190,tDis$groups$M)

boxplot(breaths$e.EFD ~ Dis, ylim = c(85,130), 
ylab = "Energy Flux Density (Pa^2s)",axes = F,col = "gray")
axis(2)
tDis <- HSD.test(a2E,'Dis')
text(c(1,2,3,4),130,tDis$groups$M)

boxplot(breaths$i.EFD ~ Dis, ylim = c(85,130), axes = F)
tDis <- HSD.test(a2I,'Dis')
text(c(1,2,3,4),130,tDis$groups$M)

par(mar = c(6,4,0.5,2))
boxplot(breaths$e.time_window ~ Dis,ylim = c(0,3.4), 
ylab = "95% Energy Duration", axes = F,col = "gray")
axis(1, at = c(0,1,2,3),las = 2,labels = c(" ","Healthy","Diseased"," "))
axis(2)
tDis <- HSD.test(a1E,'Dis')
text(c(1,2,3,4),3.4,tDis$groups$M)

par(mar = c(6,4,0.5,2))
boxplot(breaths$i.time_window ~ Dis,ylim = c(0,3.4), axes = F)
axis(1, at = c(0,1,2,3),las = 2, labels = c(" ","Healthy","Diseased"," "))

tDis <- HSD.test(a1I,'Dis')
text(c(1,2,3,4),3.4,tDis$groups$M)


dev.off()