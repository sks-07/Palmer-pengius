install.packages("palmerpenguins") # You only need to do this once
install.packages("multimode")
install.packages("LaplacesDemon")
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("distrMod")

################################# library import#############################################
library(multimode)
library(palmerpenguins)
library(diptest)
library(LaplacesDemon)
library(ggplot2)
library(reshape2)
library(fitdistrplus)
library(stats4)
library(MASS)
library(survival)
library(actuar)
library(distrMod)
library(plyr)
############################### Generating dataset ##########################################

my.student.number = 220602758 # Replace this with your student number
set.seed(my.student.number)
my.penguins = penguins[sample(nrow(penguins), 100), ]
###############################  variables  ##################################################

var_name=c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")
l_quant=0.01
u_quant=0.99

#################################  functions #################################################

qq_plot <- function(variab,var_name){
  qqnorm(variab, frame = FALSE,main=var_name)
  qqline(variab, col = "steelblue", lwd = 2)
}


hist_plot <- function(variab,k,p=TRUE){
  hist(variab,col="cyan", prob=p,main=k)
  if(p){
    curve(dnorm(x, mean=mean(variab), sd=sd(variab)), col="darkred", lwd=2, add=TRUE)
  }
}


box_plot <- function(variab,k){
  boxplot(variab,main=k)
}

quant_checking <- function(variab,l_quant,u_quant,k){
  lower_bound <- quantile(variab, l_quant)
  upper_bound <- quantile(variab, u_quant)
  outlier_ind <- which(variab < lower_bound | variab > upper_bound)
  print(variab[,outlier_ind])
}


dist_plot <- function(variab,k){
  plotdist(variab, histo = TRUE, demp = TRUE,breaks = "default")
}

#################################  dataset variables ###########################################

#general species data
data_gen_spec=my.penguins[my.penguins$species=="Gentoo",]
data_chi_spec=my.penguins[my.penguins$species=="Chinstrap",]
data_Ade_spec=my.penguins[my.penguins$species=="Adelie",]

#general island data
data_Biscoe_isld=my.penguins[my.penguins$island=="Biscoe",]
data_Dream_isld=my.penguins[my.penguins$island=="Dream",]
data_Torgersen_isld=my.penguins[my.penguins$island=="Torgersen",]


#male data
male_data=my.penguins[my.penguins$sex=="male",]
#species
male_data_gen_spec=male_data[male_data$species=="Gentoo",]
male_data_chi_spec=male_data[male_data$species=="Chinstrap",]
male_data_Ade_spec=male_data[male_data$species=="Adelie",]
#island
male_data_Biscoe_isld=male_data[male_data$island=="Biscoe",]
male_data_Dream_isld=male_data[male_data$island=="Dream",]
male_data_Torgersen_isld=male_data[male_data$island=="Torgersen",]

#female data
female_data=my.penguins[my.penguins$sex=="female",]
#species
female_data_gen_spec=female_data[female_data$species=="Gentoo",]
female_data_chi_spec=female_data[female_data$species=="Chinstrap",]
female_data_Ade_spec=female_data[female_data$species=="Adelie",]
#island
female_data_Biscoe_isld=female_data[female_data$island=="Biscoe",]
female_data_Dream_isld=female_data[female_data$island=="Dream",]
female_data_Torgersen_isld=female_data[female_data$island=="Torgersen",]

#################################  laalaalaaaaaaa ###########################################


par(mfrow=c(2,2))
for (k in var_name){
  plot(density(t(my.penguins[k])),main=k)
}

ggplot(data=my.penguins, mapping = aes(x = bill_length_mm),color=sex)+geom_density(color="darkblue", fill="lightblue")


#######
mu <- ddply(my.penguins, "sex", summarise, grp.mean=mean(bill_length_mm))
head(mu)

ggplot(data=my.penguins, mapping = aes(x = bill_length_mm),color=sex)+geom_density()+
geom_vline(data=mu, aes(xintercept= grp.mean, color=sex),
           linetype="dashed")

mu <- ddply(my.penguins, "species", summarise, grp.mean=mean(bill_length_mm))
head(mu)

ggplot(data=my.penguins, mapping = aes(x = bill_length_mm),color=species)+geom_density(color="darkblue")+
  geom_vline(data=mu, aes(xintercept= grp.mean, color=species),
             linetype="dashed")
######



