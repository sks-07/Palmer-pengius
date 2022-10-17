install.packages("palmerpenguins") # You only need to do this once
install.packages("multimode")
install.packages("LaplacesDemon")
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("distrMod")
install.packages("flexsurv")
install.packages("patchwork")
install.packages("ggpub")
install.packages("mixtools")
################################# library import#############################################
library(multimode)
library(mixtools)
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
library(flexsurv)
library(patchwork)
library(ggpubr)
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

compare <- function(varg){
  gammafit  <-  fitdist(varg, "gamma")
  weibullfit  <-  fitdist(varg, "weibull")
  lnormfit  <-  fitdist(varg, "lnorm") 
  #gengammafit  <-  fitdistrplus::fitdist(varg, "gengamma",
  #                                       start=function(d) list(mu=mean(d),
   #                                                             sigma=sd(d),
   #                                                             Q=0))
  #qqcomp(list(gammafit, weibullfit, lnormfit, gengammafit),
  #       legendtext=c("gamma", "lnorm", "weibull", "gengamma"))
  
  par(mfrow = c(2, 2))
  plot.legend <- c("Weibull", "lognormal", "gamma")
  denscomp(list(weibullfit, lnormfit, gammafit), legendtext = plot.legend)
  qqcomp(list(weibullfit, lnormfit, gammafit), legendtext = plot.legend)
  cdfcomp(list(weibullfit, lnormfit, gammafit), legendtext = plot.legend)
  ppcomp(list(weibullfit, lnormfit, gammafit), legendtext = plot.legend)
  print(weibullfit$aic)
  print(gammafit$aic)
  print(lnormfit$aic)
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

ggplot(data=my.penguins, mapping = aes(x = bill_length_mm),color=species)+
geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +geom_vline(data=mu, aes(xintercept= grp.mean, color=species),
                                                     linetype="dashed")

ggplot(data=my.penguins, mapping = aes(x = bill_length_mm),color=species) + 
  geom_density() +facet_grid(species ~ .)+geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
                                                     linetype="dashed")


#-------------------------------------------------------
data_gen=my.penguins
spec="species"
#assign("color_spec",species)

mu1 <- ddply(data_gen, spec, summarise, grp.mean=mean(bill_length_mm))
head(mu)

q1=ggplot(data=data_gen, mapping = aes(x = bill_length_mm),color=species)+geom_density(color="darkblue")+
  geom_vline(data=mu1, aes(xintercept= grp.mean, color=species),
             linetype="dashed")

mu2 <- ddply(data_gen, spec, summarise, grp.mean=mean(bill_depth_mm))
head(mu)

q2=ggplot(data=data_gen, mapping = aes(x = bill_depth_mm),color=species)+geom_density(color="darkblue")+
  geom_vline(data=mu2, aes(xintercept= grp.mean, color=species),
             linetype="dashed")
mu3 <- ddply(data_gen, spec, summarise, grp.mean=mean(flipper_length_mm))
head(mu)

q3=ggplot(data=data_gen, mapping = aes(x = flipper_length_mm),color=species)+geom_density(color="darkblue")+
  geom_vline(data=mu3, aes(xintercept= grp.mean, color=species),
             linetype="dashed")
mu4 <- ddply(data_gen, spec, summarise, grp.mean=mean(body_mass_g))
head(mu)

q4=ggplot(data=data_gen, mapping = aes(x = body_mass_g),color=species)+geom_density(color="darkblue")+
  geom_vline(data=mu4, aes(xintercept= grp.mean, color=species),
             linetype="dashed")
q1+q2+q3+q4+plot_annotation(title = "100 Penguins Data")



######

par(mfrow=c(1,3))
box_plot(data_gen_spec$bill_length_mm,"Gentoo Bill Length")
box_plot(data_chi_spec$bill_length_mm,"Chinstrap Bill Length")
box_plot(data_gen_spec$bill_length_mm,"Adelie Bill Length")
boxplot(body_mass_g ~ species, data = my.penguins)

#par(mfrow=c(1,2))
p1=ggplot(my.penguins, aes(y=bill_length_mm,x=species,fill=sex)) + geom_boxplot(outlier.colour="black")
p2=ggplot(my.penguins, aes(y=bill_depth_mm,x=species,fill=sex)) + geom_boxplot(outlier.colour="black")
p3=ggplot(my.penguins, aes(y=flipper_length_mm,x=species,fill=sex)) + geom_boxplot(outlier.colour="black")
p4=ggplot(my.penguins, aes(y=body_mass_g,x=species,fill=sex)) + geom_boxplot(outlier.colour="black")

p1+p2+p3+p4


p1=ggplot(my.penguins, aes(y=bill_length_mm,x=island,fill=sex)) + geom_boxplot(outlier.colour="black")
p2=ggplot(my.penguins, aes(y=bill_depth_mm,x=island,fill=sex)) + geom_boxplot(outlier.colour="black")
p3=ggplot(my.penguins, aes(y=flipper_length_mm,x=island,fill=sex)) + geom_boxplot(outlier.colour="black")
p4=ggplot(my.penguins, aes(y=body_mass_g,x=island,fill=sex)) + geom_boxplot(outlier.colour="black")

p1+p2+p3+p4

box_plot(female_data_chi_spec$bill_length_mm,"data_gen_spec")


dist_plot(my.penguins$bill_length_mm,k)
descdist(my.penguins$bill_length_mm)

varg=my.penguins$bill_depth_mm
compare(varg)


dS <- dip(female_data$bill_depth_mm, full = "all", debug = TRUE)
plot(dS)

out <- normalmixEM(my.penguins$bill_length_mm, k=2, epsilon = 1e-03, fast=TRUE)

summary(out)
#mode test
modetest(data_gen_spec$bill_length_mm)
locmodes(my.penguins$bill_length_mm,mod0=2,display=TRUE)
is.unimodal(my.penguins$bill_length_mm)
is.multimodal(my.penguins$bill_length_mm)
is.bimodal(my.penguins$bill_length_mm)
is.trimodal(my.penguins$bill_length_mm)
bimodality_coefficient(my.penguins$bill_length_mm)
