library(bbmle)
library(AICcmodavg)
library(vegan)

# ----------------------------------------------------------------------------
#scale variables
# ----------------------------------------------------------------------------
data(varechem)
data(varespec)
colnames(varechem)

dat <- list() 
dat$N <- scale(varechem$N)
dat$P <- scale(varechem$P)
dat$K <- scale(varechem$K)
dat$Ca<- scale(varechem$Ca)
dat$Mg <- scale(varechem$Mg)
dat$S <- scale(varechem$S)
dat$Al <- scale(varechem$Al)
dat$Fe <- scale(varechem$Fe)
dat$Mn <- scale(varechem$Mn)
dat$Zn <- scale(varechem$Zn)
dat$Mo <- scale(varechem$Mo)
dat$Baresoil <- scale(varechem$Baresoil)
dat$Humdepth <- scale(varechem$Humdepth)
dat$pH <- scale(varechem$pH)
dat$divers <- apply(ifelse(varespec>0,1,0),1,sum)
# dat

# ----------------------------------------------------------------------------
#Subset of models for species richness
# ----------------------------------------------------------------------------
cand.models<-list()
cand.models[[1]] <- glm(divers ~ N + P + Fe,               data=dat, family=poisson)
cand.models[[2]] <- glm(divers ~ Zn + K + Mo,              data=dat, family=poisson)
cand.models[[3]] <- glm(divers ~ Baresoil + Humdepth + pH, data=dat, family=poisson)
cand.models[[4]] <- glm(divers ~ K + Ca + S,               data=dat, family=poisson)
cand.models[[5]] <- glm(divers ~ Al + Mn + Mo,             data=dat, family=poisson)

#Vector of names
modnames<-paste("mod", 1:length(cand.models),sep="")

#AICc table
aic.table <- aictab(cand.set= cand.models, modnames= modnames, sort=TRUE)
print(aic.table,digits=4,LL=TRUE) #LL = log likelyhood

#null model - AIC value
richnull <- glm(divers ~ 1 , data= dat , family= poisson)
richnull
anova( richnull , cand.models[[3]] )
AIC( richnull , cand.models[[3]] )



# ----------------------------------------------------------------------------
##Mazerolle (2006) frog water loss example
# ----------------------------------------------------------------------------
data(dry.frog)

##setup a subset of models of Table 1
Cand.models <- list( )
Cand.models[[1]] <- lm(log_Mass_lost ~ Shade + Substrate + cent_Initial_mass + Initial_mass2,                   data = dry.frog)
Cand.models[[2]] <- lm(log_Mass_lost ~ Shade + Substrate + cent_Initial_mass + Initial_mass2 + Shade:Substrate, data = dry.frog)
Cand.models[[3]] <- lm(log_Mass_lost ~                     cent_Initial_mass + Initial_mass2,                   data = dry.frog)
Cand.models[[4]] <- lm(log_Mass_lost ~ Shade +             cent_Initial_mass + Initial_mass2,                   data = dry.frog)
Cand.models[[5]] <- lm(log_Mass_lost ~         Substrate + cent_Initial_mass + Initial_mass2,                   data = dry.frog)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

##generate AICc table
aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)



# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
## Not run: 
##Burnham and Anderson (2002) flour beetle data
data(beetle)
##models as suggested by Burnham and Anderson p. 198          
Cand.set <- list( )
Cand.set[[1]] <- glm(Mortality_rate ~ Dose, family =
                       binomial(link = "logit"), weights = Number_tested,
                     data = beetle)
Cand.set[[2]] <- glm(Mortality_rate ~ Dose, family =
                       binomial(link = "probit"), weights = Number_tested,
                     data = beetle)
Cand.set[[3]] <- glm(Mortality_rate ~ Dose, family =
                       binomial(link ="cloglog"), weights = Number_tested,
                     data = beetle)

##check c-hat
c_hat(Cand.set[[1]])
c_hat(Cand.set[[2]])
c_hat(Cand.set[[3]])
##lowest value of c-hat < 1 for these non-nested models, thus use
##c.hat = 1 

##set up named list
names(Cand.set) <- c("logit", "probit", "cloglog")

##compare models
##model names will be taken from the list if modnames is not specified
res.table <- aictab(cand.set = Cand.set, second.ord = FALSE)
##note that delta AIC and Akaike weights are identical to Table 4.7
print(res.table, digits = 2, LL = TRUE) #print table with 2 digits and
##print log-likelihood in table
print(res.table, digits = 4, LL = FALSE) #print table with 4 digits and
##do not print log-likelihood

## End(Not run)
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------



# ----------------------------------------------------------------------------
##two-way ANOVA with interaction
# ----------------------------------------------------------------------------
data(iron)
##full model
m1 <- lm(Iron ~ Pot + Food + Pot:Food, data = iron)
##additive model
m2 <- lm(Iron ~ Pot + Food, data = iron)
##null model
m3 <- lm(Iron ~ 1, data = iron)

##candidate models
Cand.aov <- list(m1, m2, m3)
Cand.names <- c("full", "additive", "null")
aictab(Cand.aov, Cand.names)


