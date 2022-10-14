install.packages("palmerpenguins") # You only need to do this once
install.packages("multimode")
install.packages("LaplacesDemon")
library(multimode)
library(palmerpenguins)
library(diptest)
library(LaplacesDemon)
data("penguins")
penguins = na.omit(penguins) # Removes missing rows

library(ggplot2)
my.student.number = 220602758 # Replace this with your student number
set.seed(my.student.number)
my.penguins = penguins[sample(nrow(penguins), 100), ]
######################################################################


hist(my.penguins$bill_length_mm,col="cyan",
     prob=FALSE)
#curve(dnorm(x, mean=mean(my.penguins$bill_length_mm), 
#sd=sd(my.penguins$bill_length_mm)), col="darkred", 
#      lwd=2, add=TRUE)
var_name=c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")

qq_plot <- function(variab,var_name){
  qqnorm(variab, frame = FALSE,main=var_name)
  qqline(variab, col = "steelblue", lwd = 2)
}
#checking normality of vaiables from qq plot
par(mfrow=c(2,2))
for (k in var_name) {
  qq_plot(t(my.penguins[k]),k)
}


#correaltion between varibales
newdat <- my.penguins[,c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")]
cormat <- round(cor(newdat),2)


######################################################################
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

#melted_cormat = cormat
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var2, Var1, label = value),
            color = "white", size = 4)

#writing file in csv
#write.csv(my.penguins,"", row.names = TRUE)

hist_plot <- function(variab,k,p=TRUE){
  hist(variab,col="cyan", prob=p,main=k)
  if(p){
    curve(dnorm(x, mean=mean(variab), sd=sd(variab)), col="darkred", lwd=2, add=TRUE)
  }
}
######################################################################
#checking normality of vaiables from qq plot
par(mfrow=c(2,2))
for (k in var_name) {
  hist_plot(t(my.penguins[k]),k,p=FALSE)
}


box_plot <- function(variab,k){
  boxplot(variab,main=k)
}
#checking normality of vaiables from qq plot
par(mfrow=c(2,2))
for (k in var_name) {
  box_plot(my.penguins[k],k)
}
#####################################################################
l_quant=0.01
u_quant=0.99
quant_checking <- function(variab,l_quant,u_quant,k){
  lower_bound <- quantile(variab, l_quant)
  upper_bound <- quantile(variab, u_quant)
  outlier_ind <- which(variab < lower_bound | variab > upper_bound)
  print(variab[,outlier_ind])
}
#checking normality of vaiables from qq plot

for (k in var_name) {
  quant_checking(t(my.penguins[k]),l_quant,u_quant,k)
}

#####################################################################
male_data=my.penguins[my.penguins$sex=="male",]
female_data=my.penguins[my.penguins$sex=="female",]

par(mfrow=c(2,2))
for (k in var_name) {
  qq_plot(t(male_data[k]),paste(k,"MALE",sep=" "))
}

par(mfrow=c(2,2))
for (k in var_name) {
  box_plot(female_data[k],paste(k,"FWMALE",sep=" "))
}

par(mfrow=c(2,2))
for (k in var_name) {
  hist_plot(t(male_data[k]),k)
}


#################################################
#fit dist
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("distrMod")

library(fitdistrplus)
library(stats4)
library(MASS)
# for other necessary test or graphical tools
library(survival)
library(actuar)
library(distrMod)

dist_plot <- function(variab,k){
  plotdist(variab, histo = TRUE, demp = TRUE,breaks = "default")
}

dist_plot(female_data$bill_depth_mm,k)
dS <- dip(female_data$bill_depth_mm, full = "all", debug = TRUE)
plot(dS)

#dist_plot(my.penguins$bill_depth_mm,k)



#descdist(male_data$bill_length_mm,boot=100)
dummy=female_data
dummy= dummy[dummy$bill_depth_mm < 15 | dummy$bill_depth_mm>17,]

dist_plot(dummy$bill_depth_mm,k)
hist_plot(dummy$bill_depth_mm,"dcsc")


spec=c( "Gentoo","Chinstrap","Adelie")
isld=c("Biscoe","Dream","Torgersen")

female_data_gen_spec=female_data[female_data$species=="Gentoo",]
female_data_chi_spec=female_data[female_data$species=="Chinstrap",]
female_data_Ade_spec=female_data[female_data$species=="Adelie",]

female_data_Biscoe_isld=female_data[female_data$island=="Biscoe",]
female_data_Dream_isld=female_data[female_data$island=="Dream",]
female_data_Torgersen_isld=female_data[female_data$island=="Torgersen",]


dist_plot(female_data_gen_spec$bill_depth_mm,k)
dist_plot(female_data_chi_spec$bill_depth_mm,k)
dist_plot(female_data_Ade_spec$bill_depth_mm,k)


dist_plot(female_data_Biscoe_isld$bill_depth_mm,k)
dist_plot(female_data_Dream_isld$bill_depth_mm,k)
dist_plot(female_data_Torgersen_isld$bill_depth_mm,k)



for(sp in spec){
  nam=paste("male_data",sp,sep="_")
  assign(nam,male_data[male_data$species==sp,])                         
}

qq_plot(t(female_data_gen_spec$bill_depth_mm),k)

dist_plot(male_data_Chinstrap$flipper_length_mm,k)

