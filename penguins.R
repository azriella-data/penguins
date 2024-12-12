#Palmer Penguins
#Azriella Turkdogan
#12/11/2024


library(tidyverse)
library(lme4)
library(palmerpenguins)
library(car)
library(multcomp)

data(package = 'palmerpenguins')
head(penguins)

ggplot(data=na.omit(penguins), aes(x=species, y= body_mass_g, fill=sex))+
  geom_boxplot()+
  xlab("Species")+
  ylab("Body Mass (grams)")+
  ggtitle("Penguins Species vs. Body Mass")

ggplot(data=na.omit(penguins), aes(x=species, y= flipper_length_mm, fill=sex))+
  geom_boxplot()+
  xlab("Species")+
  ylab("Flipper Length (mm)")+
  ggtitle("Penguins Species vs. Flipper Length")

ggplot(data=na.omit(penguins), aes(x=body_mass_g, y= flipper_length_mm, color=species))+
  geom_point(aes(shape=sex))+
  geom_smooth(data = penguins, 
              method ="lm", alpha = 0, show.legend = FALSE, linetype = 'solid') +
  xlab("Penguin Mass (grams)")+
  ylab("Flipper Length (mm)")+
  ggtitle("Penguin Weight vs. Flipper Length")

#Anova - Flipper
anova_flipper = aov(penguins$flipper_length_mm ~ penguins$species, data =penguins)
summary(anova_flipper)

#Check of normality assumption
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(anova_flipper$residuals, main = "Flipper Length Distribution", xlab = "flipper length residuals")
# QQ-plot
qqPlot(anova_flipper$residuals,id = FALSE, ylab = "flipper length residuals", main = "qqplot")

#Tukey Test - Flipper
TukeyHSD(anova_flipper, conf.level=.95) 
plot(TukeyHSD(anova_flipper, conf.level=.95), las = 2)

#Anova - Body Mass
anova_mass = aov(penguins$body_mass_g ~ penguins$species, data =penguins)
summary(anova_mass)

#Check of normality assumption
par(mfrow = c(1, 2)) # combine plots
# histogram
hist(anova_mass$residuals, main = "Body Mass Distribution", xlab = "body mass residuals")
# QQ-plot
qqPlot(anova_mass$residuals,id = FALSE, ylab = "body mass residuals", main = "qqplot")


#Tukey Test - Body Mass
TukeyHSD(anova_mass, conf.level=.95) 
plot(TukeyHSD(anova_mass, conf.level=.95), las = 2)


# Adelie - Linear Model
adelie <- filter(penguins, penguins$species == "Adelie")
adelie_lm <- lm(adelie$flipper_length_mm ~ adelie$body_mass_g)
coeff=coefficients(adelie_lm)
eq1 = paste0("y = ", round(coeff[2],3), "x + ", round(coeff[1],3), " mm/g") #linear equation

###Plot Linear Model
plot(adelie$body_mass_g, adelie$flipper_length_mm, xlab = "Body Mass (g)",
     ylab = "Flipper Length (mm)", main = "Adelie: Flipper Length vs. Weight")
abline(adelie_lm)
mtext(eq1, 3, line=0)
summary(adelie_lm)

# Chinstrap - Linear Model
chinstrap <- filter(penguins, penguins$species == "Chinstrap")
chinstrap_lm <- lm(chinstrap$flipper_length_mm ~ chinstrap$body_mass_g)
coeff2=coefficients(chinstrap_lm)
eq2 = paste0("y = ", round(coeff2[2],3), "x + ", round(coeff2[1],3), " mm/g") #linear equation

###Plot Linear Model
plot(chinstrap$body_mass_g, chinstrap$flipper_length_mm, xlab = "Body Mass (g)",
     ylab = "Flipper Length (mm)", main = "Chinstrap: Flipper Length vs. Weight")
abline(chinstrap_lm)
mtext(eq2, 3, line=0)
summary(chinstrap_lm)


#Gentoo - Linear Model
gentoo <- filter(penguins, penguins$species == "Gentoo")
gentoo_lm <- lm(gentoo$flipper_length_mm ~ gentoo$body_mass_g)
coeff3=coefficients(gentoo_lm)
eq3 = paste0("y = ", round(coeff3[2],3), "x + ", round(coeff3[1],3), " mm/g") #linear equation

###Plot Linear Model
plot(gentoo$body_mass_g, gentoo$flipper_length_mm, xlab = "Body Mass (g)",
     ylab = "Flipper Length (mm)", main = "Gentoo: Flipper Length vs. Weight")
abline(gentoo_lm)
mtext(eq3, 3, line=0)
summary(gentoo_lm)


### Citations

# ```         
# R Core Team (2024). _R: A Language and Environment for Statistical Computing_. R   Foundation for Statistical Computing, Vienna, Austria.   https://www.R-project.org/.
# ```
# 
# ```         
# Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes   A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J,   Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H   (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43),   1686. doi:10.21105/joss.01686 https://doi.org/10.21105/joss.01686.
# ```
# 
# ```         
# Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear   Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48.   doi:10.18637/jss.v067.i01.
# ```
# 
# ```         
# Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago   (Antarctica) penguin data. R package version 0.1.0.   https://allisonhorst.github.io/palmerpenguins/. doi: 10.5281/zenodo.3960218.
# ```
# 
# ```         
# Fox J, Weisberg S (2019). _An R Companion to Applied Regression_, Third edition.   Sage, Thousand Oaks CA. https://www.john-fox.ca/Companion/. 
# ```
# 
# ```         
# Hothorn T, Bretz F, Westfall P (2008). “Simultaneous Inference in General   Parametric Models.” _Biometrical Journal_, *50*(3), 346-363. 
# ```
