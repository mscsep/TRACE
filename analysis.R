## Analysis script for 
## "The behavioral phenotype of early life adversity: a 3-level meta-analysis of preclinical studies"

## Author: Valeria Bonapersona
## Supervision: Caspar J van Lissa

## for questions, contact Valeria @ v.bonapersona-2@umcutrecht.nl

# Environment Preparation -------------------------------------------------
rm(list = ls()) #clean environment

#libraries
library(dplyr) #general
library(ggplot2) #for graphs
library(metafor) #for meta-analysis
library(caret) #for crossvalidation
library(metaforest) #for exploratory analysis

#import dataset (already processed with datasetPreparation)

load("data.RData")


# General -----------------------------------------------------
#exclusion studies analysed only at a systematic review level
data %>%
  filter(domain != "noMeta") %>%
  droplevels() -> dat

##effect size general information
hist(dat$yi, breaks = 100) 

mean(dat$yi) 
sd(dat$yi) 
min(dat$yi) 
max(dat$yi) 

length(dat[dat$yi > -2 & dat$yi < 2,]$each)/length(dat$yi) 


##sample sizes general information
dat$nTot <- dat$nC + dat$nE 
hist(dat$nTot, breaks = 100) 

mean(dat$nTot) 
sd(dat$nTot) 
min(dat$nTot) 
max(dat$nTot) 

# in ~90% comparisons, control and experimental group's sample sizes differ less than 20%
length(dat[abs((dat$nC - dat$nE) / dat$nTot) < .2,]$each) / length(dat$each)


# Selection dataset by sex ---------------------------------------------------------
#create only male dataset
data %>%
  filter(sex == "M", domain != "noMeta") %>%
  droplevels() -> dat

#create only female dataset
#data %>%
 # filter(sex == "F", domain != "noMeta") %>%
#  droplevels() -> dat



# Selection dataset for sensitivity ---------------------------------------------------------
## No potential outliers that are influential (Viechtbauer & Cheung) >> check out first "outliers and extreme values"
#dat %>%
 # filter(outInf != 1) %>%
  #droplevels() -> dat


## No outliers (according to Tabachnick & Fidell (2013) definition: "z-score above +3.29 or below -3.29"
#dat %>% 
 # filter(!abs((dat$yi - tapply(dat$yi, dat$domain, 
  #                           mean, na.rm = TRUE)[dat$domain]) /
   #     (dat$yi - tapply(dat$yi, dat$domain, 
    #                   sd, na.rm = TRUE)[dat$domain])) > 3.29) %>%
#  droplevels -> dat


## Only blinded and randomized studies >> very few >> check below sensitivity analysis performed
#dat %>%
 # filter(blindRand == 1) %>%
  #group_by(domain, hit2Grouped) %>%
  #summarise(length(each),
   #         length(unique(id)))


## Only tests reported by more than 4 publication
#dat$domTest <- as.factor(paste0(dat$domain, dat$testAuthorGrouped))
#dat$enough <- 0
#for (each in levels(dat$domTest)) {
#  if (length(unique(dat[dat$domTest == each,]$id)) > 4) { #id = publication, #each = each comparison
 #   dat[dat$domTest == each,]$enough <- 1
#  } 
#}

#dat %>% filter(enough == 1) -> dat 


# Model -------------------------------------------------------------------

mod <- rma.mv(yi, vi,
              random = list(~1 | each, ~1 | exp),
              mods   = ~domain:hit2Grouped - 1 ,
              method = "REML",
              data = dat)
summary(mod)

##RQ 1: Effects of ELS on behavioral domains

anxiety    <- anova(mod, L = c(.5,0,0,0,
                               .5,0,0,0)) 
sLearning  <- anova(mod, L = c(0,.5,0,0,
                               0,.5,0,0))
nsLearning <- anova(mod, L = c(0,0,.5,0,
                               0,0,.5,0))
social     <- anova(mod, L = c(0,0,0,.5,
                               0,0,0,.5))


##RQ #2: presence of hit enhances effects
mainHit <- anova(mod, L = c(-.25,-.25,-.25,-.25,
                             .25, .25, .25, .25))

##posthoc RQ #2: posthoc testing to check which domain is most affected
anxietyHit    <- anova(mod, L = c(-1,0,0,0,
                                   1,0,0,0))
sLearningHit  <- anova(mod, L = c(0,-1,0,0,
                                  0, 1,0,0))
nsLearningHit <- anova(mod, L = c(0,0,-1,0,
                                  0,0, 1,0))
socialHit     <- anova(mod, L = c(0,0,0,-1,
                                  0,0,0, 1))


##Summary results organized in table
resultMain <- data.frame(matrix(data = NA, nrow = 17, ncol = 8))

colnames(resultMain) <- c("test", "ci.lb", "ci.ub", 
                          "effectsize", "se", "Zvalue",  
                          "Pvalue", "Pvalue_bonfCorr")

resultMain[,1] <- c("anxiety", "sLearning", "nsLearning", "social", 
                    "hit", 
                    "anxietyHit", "sLearningHit", "nsLearningHit", "socialHit",
                    "anxietyNo", "sLearningNo", "nsLearningNo", "socialNo",
                    "anxietyYes", "sLearningYes", "nsLearningYes", "socialYes")

resultMain[,4] <- round(c(anxiety$Lb, sLearning$Lb, nsLearning$Lb, social$Lb,
                          mainHit$Lb,
                          anxietyHit$Lb, sLearningHit$Lb, nsLearningHit$Lb, socialHit$Lb,
                          mod$beta), digits = 4) #effect size
resultMain[,5] <- round(c(anxiety$se, sLearning$se, nsLearning$se, social$se,
                          mainHit$se,
                          anxietyHit$se, sLearningHit$se, nsLearningHit$se, socialHit$se,
                          mod$se), digits = 4) #se 
resultMain[,6] <- round(c(anxiety$zval, sLearning$zval, nsLearning$zval, social$zval,
                          mainHit$zval,
                          anxietyHit$zval, sLearningHit$zval, nsLearningHit$zval, socialHit$zval,
                          mod$zval), digits = 4) #zvalues
resultMain[,7] <- round(c(anxiety$pval, sLearning$pval, nsLearning$pval, social$pval,
                          mainHit$pval,
                          anxietyHit$pval, sLearningHit$pval, nsLearningHit$pval, socialHit$pval,
                          mod$pval), digits = 4) #pvalues
resultMain[,2] <- round(resultMain[,4] - (resultMain[,5] * 1.96), digits = 4) #CI lower 
resultMain[,3] <- round(resultMain[,4] + (resultMain[,5] * 1.96), digits = 4) #CI upper
resultMain[,8] <- round(c(p.adjust(resultMain[ 1,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[ 2,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[ 3,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[ 4,7], method = "bonferroni", n = 4),
                          resultMain[5,7],
                          p.adjust(resultMain[ 6,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[ 7,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[ 8,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[ 9,7], method = "bonferroni", n = 4),
                          p.adjust(resultMain[10,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[11,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[12,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[13,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[14,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[15,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[16,7], method = "bonferroni", n = 8),
                          p.adjust(resultMain[17,7], method = "bonferroni", n = 8)), digits = 4) #pvalue bonf correction


# Boxplot domain ---------------------------------------------------
##results in graph to ease interpretation
invMult <- c("nsLearning", "social",
             "nsLearningHit", "socialHit",
             "nsLearningNo","socialNo",
             "nsLearningYes", "socialYes") #results that need correction in direction effect

#correct direction effect size
resultMain$effectsizeCorrected <- ifelse(resultMain$test %in% invMult,
                                         resultMain$effectsize * (-1), resultMain$effectsize)

#invert ci.lb and ci.ub where necessary
resultMain$ci.lb_Corrected <- ifelse(resultMain$test %in% invMult,
                                     0 - resultMain$ci.ub, resultMain$ci.lb)
resultMain$ci.ub_Corrected <- ifelse(resultMain$test %in% invMult,
                                     0 - resultMain$ci.lb, resultMain$ci.ub)

#reorder test factors
resultMain$test <- as.factor(resultMain$test)
resultMain$test <- factor(resultMain$test, levels = c("anxiety", "sLearning", "nsLearning", "social",
                                                      "anxietyHit", "sLearningHit","nsLearningHit", "socialHit",
                                                      "anxietyNo", "anxietyYes", "sLearningNo","sLearningYes",
                                                      "nsLearningNo","nsLearningYes", "socialNo","socialYes",
                                                      "hit")) 

#bargraph domains
ggplot(resultMain[resultMain$test %in% c("anxiety", "sLearning", "nsLearning", "social"),], 
       aes(x = test, y = effectsizeCorrected)) +
  #ylim(-1,1) +
  ylab("Standardized mean difference (CI)") +
  ggtitle("Effects of ELS on behavior",
          subtitle = "Results") +
  theme_bw() +
  geom_bar(stat = "identity", 
           fill = "light grey", 
           color = "black") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_errorbar(aes(ymin = ci.lb_Corrected, 
                    ymax = ci.ub_Corrected),
                width = .6)
##save in tiff
#dev.copy(tiff,'MAB_figures/MAB_male_domains.tiff') #males
#dev.copy(tiff,'MAB_figures/MAB_female_domains.tiff') #females
#dev.off()



#domains + hit
resultMain$domain <- as.factor(c("anxiety", "sLearning", "nsLearning", "social", "hit", "anxiety", "sLearning", "nsLearning", "social", "anxiety", "sLearning", "nsLearning", "social", "anxiety", "sLearning", "nsLearning", "social"))

ggplot(resultMain[resultMain$test %in% c("anxietyNo", "anxietyYes", "sLearningNo","sLearningYes",
                                         "nsLearningNo","nsLearningYes", "socialNo","socialYes"),], 
       aes(x = test, y = effectsizeCorrected)) +
  #ylim(-1,1) +
  ylab("Standardized mean difference (CI)") +
  ggtitle("Effects of ELS on behavior",
          subtitle = "Results") +
  theme_bw() + 
  geom_bar(stat = "identity", 
           fill = "light grey", 
           color = "black") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_errorbar(aes(ymin = ci.lb_Corrected, 
                    ymax = ci.ub_Corrected),
                width = .6)
##save in tiff
#dev.copy(tiff,'MAB_figures/MAB_male_domainsHit.tiff') #males
#dev.copy(tiff,'MAB_figures/MAB_female_domainsHit.tiff') #females
#dev.off()


# Save results ------------------------------------------------------------
##Main analysis
#write.csv(resultMain, file = "MAB_male_mainResults.csv") #save results main analysis males
#write.csv(resultMain, file = "MAB_female_mainResults.csv") #save results main analysis females

##sensitivity analysis
###sensitivity potential outliers & influential 
#write.csv(resultMain, file = "MAB_male_sensitivity_outInf.csv") 
#write.csv(resultMain, file = "MAB_female_sensitivity_outInf.csv") 

###sensitivity only blinded and randomized papers
#write.csv(resultMain, file = "MAB_male_sensitivity_blindRand.csv") 
#write.csv(resultMain, file = "MAB_female_sensitivity_blindRand.csv")  

###sensitivity only tests reported by >4 publications
#write.csv(resultMain, file = "MAB_male_sensitivity_enough.csv") 
#write.csv(resultMain, file = "MAB_female_sensitivity_enough.csv")  

# Study of distribution of variance -------------------------------------------------------

#2-level model with both sigmas constrained to zero
mod_noWithnoBet <- rma.mv(yi, vi, 
                     random = list(~1 | each, ~1 | exp),
                     mod = ~domain:hit2Grouped -1,
                     sigma2 = c(0, 0),
                     digits = 3,
                     method = "REML",
                     data = dat)

#likelihood ratio test to determine the significance of the between-study variance
anova(mod, mod_noWithnoBet)

#2-level model without within-study variance >> between-study variance is estimated
mod_noWith <- rma.mv(yi, vi, 
                     random = list(~1 | each, ~1 | exp),
                     mod = ~domain:hit2Grouped -1,
                     sigma2 = c(0, NA),
                     digits = 3,
                     method = "REML",
                     data = dat)

#likelihood ratio test for significance of the between-study variance
anova(mod, mod_noWith)

#2-level model without between-study variance >> within-study variance is estimated
mod_noBet <- rma.mv(yi, vi, 
                    random = list(~1 | each, ~1 | exp),
                    mod = ~domain:hit2Grouped -1,
                    sigma2 = c(NA, 0),
                    digits = 3,
                    method = "REML",
                    data = dat)

#ikelihood ratio test for significance of the within-study variance
anova(mod, mod_noBet)


#determining how the total variance is distributed over the 3 levels of the meta-analytic model
#adapted from Assink(2016)

n <- length(dat$vi)
list.inverse.variances <- 1 / (dat$vi)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (dat$vi ^ 2)
sum.inverse.variances.square <- sum(list.inverse.variances.square)

numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances - sum.inverse.variances.square
estimated.sampling.variance <- numerator / denominator

I2_1 <- (estimated.sampling.variance) / (mod$sigma2[1] +
                                           mod$sigma2[2] + estimated.sampling.variance)
I2_2 <- (mod$sigma2[1]) / (mod$sigma2[1] +
                             mod$sigma2[2] + estimated.sampling.variance)
I2_3 <- (mod$sigma2[2]) / (mod$sigma2[1] +
                             mod$sigma2[2] + estimated.sampling.variance)

modVariance_1 <- I2_1 * 100
modVariance_2 <- I2_2 * 100
modVariance_3 <- I2_3 * 100




# Crossvalidation ---------------------------------------------------------
modelInfo <- list(label = "Meta-Analysis via Mixed-Effects Models",
                  library = "metafor",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "method",
                                          class = "character",
                                          label = "Which method to use"),
                  grid = function(x, y, len = NULL, search = "grid")
                    data.frame(method = "REML"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(is.null(wts))
                    {
                      stop("Tuning an rma() model requires passing a vector of sampling variances to the 'weights' parameter when calling train().")
                    }
                    #x <- if(is.matrix(x)) x else as.matrix(x)
                    args <- list(...)
                    if(!is.null(args[["random"]])){
                      multilevel_vars <- sapply(args[["random"]], function(x){all.vars(as.formula(x))})
                      # See if RMA accepts both data and mods arguments. If not, change the FORMULA for rma to include only non-multilevel_vars
                      dat <- x[, multilevel_vars]
                      x <- x[, -match(multilevel_vars, colnames(x))]
                      x <- sapply(x, as.numeric)
                      out <- rma.mv(yi = y, V = wts, mods = x, method = param$method, data = dat, random = args[["random"]])
                      out
                    } else {
                      out <- rma(yi = y, vi = wts, mods = x, method = param$method, ...)
                      out
                    }
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    #print(rownames(modelFit$b)[-match("intrcpt", rownames(modelFit$b))])
                    model_vars <- rownames(modelFit$b)[-match("intrcpt", rownames(modelFit$b))]
                    #if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    newdata <- newdata[, match(model_vars, colnames(newdata))]
                    newdata <- sapply(newdata, as.numeric)
                    #print(colnames(newdata))
                    predict.rma(modelFit, newdata)$pred
                  },
                  prob = NULL,
                  predictors = function(x, ...) rownames(x$b)[-match("intrcpt", rownames(x$b))],
                  tags = c("Meta-analysis", "Linear Regression", "Accepts Case Weights"),
                  varImp = function(object, ...) {
                    values <- object$zval
                    varImps <-  abs(values[-1])
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    rnames <- rownames(object$b)[-match("intrcpt", rownames(object$b))]
                    if(!is.null(rnames)) rownames(out) <- rnames
                    out
                  },
                  sort = function(x) x)

# Preparation dataset for crossvalidation
dataCross <- dat[,c("domain", "hit2Grouped", "yi", "vi", "each", "exp")]

tmp_mods <- model.matrix(~domain:hit2Grouped, dataCross)

# Remove the intercept column:
tmp_mods <- model.matrix(~domain:hit2Grouped, dataCross)[,-1]


# Now apply to your data:
fit_control <- trainControl(## 10-fold grouped CV
  method = "cv",
  index = groupKFold(dataCross$exp, k = 10))


X <- cbind(tmp_mods, dataCross[, c("each", "exp")])
set.seed(573)
modCrossTrain <- train(y = dataCross$yi,
                       x = X,
                       weights = dataCross$vi,
                       random = list(~1 | each, ~1 | exp),
                       method = modelInfo,
                       trControl = fit_control)


modCrossTrain$results
modCrossTrain$finalModel
modCrossTrain 


# Males: outliers and extreme values ----------------------------
hist(dat$yi, breaks = 50) 

#identification influential cases following Viechtbauer & Cheung(2010)'s definition: "the studentized deleted residual > 1.96"
#available in rma.uni model of metafor package
modUni <- rma.uni(yi, vi,
                  mods = ~domain:hit2Grouped - 1,
                  method = "REML",
                  digits = 3,
                  data = dat)

dat$potInf <- 0
dat$potOut <- 0 

## It may take a while! Willing to skip? go to ##manual
y <- rstudent(modUni)
inf <- influence(modUni) # Calculate the influence diagnostics
#plot(inf, layout = c(8,1)) 
dat[which(inf$is.infl),]$potInf <- 1 # Influential cases
dat[which(abs(y$z) > 1.96),]$potOut <- 1 # Values that are potential outliers


## Manual
# Influential cases manual
#dat[dat$each %in% c(64,  65, 159, 202, 283, 295, 297, 311, 312, 358, 379,
 #                   431, 444, 468, 483, 503, 549, 552, 554, 555, 557, 561,
  #                  599, 603, 604, 633, 666, 672, 687, 690, 699, 710),]$potInf <- 1 #MALES (manual for when influence() not run)

# Potential outliers manual
#dat[dat$each %in% c(2, 3, 4, 18, 21, 143, 199, 200, 215, 230, 238,
#                    287, 333, 396, 407, 429, 460, 483, 516, 519,
 #                   561, 573, 577, 578, 615, 666, 671, 675,
  #                  686, 687, 689, 690, 699, 725, 731),]$potOut <- 1 #MALES (manual for when influence() not run)


dat %>% 
  group_by(potInf) %>% 
  summarize(length(each), 
            length(unique(exp)),
            length(unique(id)))

dat %>% 
  group_by(potOut) %>% 
  summarize(length(each), 
            length(unique(exp)),
            length(unique(id)))

# Cases that are potential outliers and influential
dat$outInf <- ifelse(dat$potOut == 1 & dat$potInf == 1, 1, 0) 

dat %>% 
  group_by(outInf) %>% 
  summarize(length(each), 
            length(unique(exp)),
            length(unique(id)))




# Females: outliers and extreme values ----------------------------
hist(dat$yi, breaks = 50) #distribution effect sizes

#identification influential cases following Viechtbauer & Cheung(2010)'s definition: "the studentized deleted residual > 1.96"
#available in rma.uni model of metafor package
modUni <- rma.uni(yi, vi,
                  mods = ~domain:hit2Grouped - 1,
                  method = "REML",
                  digits = 3,
                  data = dat)

dat$potInf <- 0
dat$potOut <- 0 

##it may take a while! Willing to skip? go to ##manual
y <- rstudent(modUni)
inf <- influence(modUni) #calculate the influence diagnostics
#plot(inf, layout = c(8,1)) 
dat[which(inf$is.infl),]$potInf <- 1 #influential cases
dat[which(abs(y$z) > 1.96),]$potOut <- 1 #values that are potential outliers

##manual
#influential cases manual
#dat[dat$each %in% c(188, 296, 298, 359,
 #                   380, 448, 485, 548),]$potInf <- 1 #FEMALES (manual for when inf not run)

#potential outliers manual
#dat[dat$each %in% c(73, 156, 158, 188, 190, 361, 448, 461,
 #                   463, 464, 465, 485, 533, 721, 726),]$potOut <- 1 #FEMALES (manual for when inf not run)


dat %>% 
  group_by(potInf) %>% 
  summarize(length(each), 
            length(unique(exp)),
            length(unique(id)))

dat %>% 
  group_by(potOut) %>% 
  summarize(length(each), 
            length(unique(exp)),
            length(unique(id)))

#cases that are potential outliers and influential
dat$outInf <- ifelse(dat$potOut == 1 & dat$potInf == 1, 1, 0) 

dat %>% 
  group_by(outInf) %>% 
  summarize(length(each), 
            length(unique(exp)),
            length(unique(id)))



# Sensitivity analysis blinded & randomized -------------------------------

modBR <- rma.mv(yi, vi,
              random = list(~1 | each, ~1 | exp),
              mods   = ~blindRand,
              method = "REML",
              data = dat)
summary(modBR)

# PublicationBias ---------------------------------------------------------
funnel(mod, xlab = "Residual Value", back = "white") #funnel plot for qualitative evaluation publication bias
#dev.copy(tiff,'MAB_figures/MAB_male_funnel.tiff')##save for males
#dev.copy(tiff,'MAB_figures/MAB_female_funnel.tiff')##save for females
#dev.off()

#random effects model for evaluation of publication bias
modRanEff <- rma.uni(yi, vi,
                     #random = list(~1 | each, ~1 | exp),
                     mods = ~domain:hit2Grouped - 1,
                     method = "REML",
                     digits = 3,
                     data = dat)

Egger <- regtest(modRanEff, ret.fit = TRUE) #Egger's regression random effects (mixed not available)
Egger

#Begg's test
beggs <- regtest(dat$yi, dat$vi)
beggs


#file drawer analysis (fail and safe)
fsn <- fsn(yi = yi, vi = vi, 
           data = dat, 
           subset = c(domain, hit2Grouped), type = "Rosenthal")
fsn

#trim and fill
nomod <- rma.uni(yi, vi,
                 #random = list(~1 | each, ~1 | exp),
                 #mods = ~domain:hit2Grouped - 1,
                 method = "REML",
                 digits = 3,
                 data = dat)
trim <- trimfill(nomod) #0 papers >> what does it mean?
trim



# MetaForest: dataset preparation --------------------------------------------------------------
data$yi <- as.numeric(data$yi)
#data$exp <- as.factor(data$exp) #if I do not do this, the model does not run

#createDataset
data %>% 
  select(yi, year,
         species, speciesStrainGrouped, origin, sex, ageWeek, #animals
         
         model, mTimeLength, #models
         mCageGrouped, mLDGrouped, mControlGrouped,
         
         hit2Grouped, 
         
         domain, 
         testAuthorGrouped, 
         #varAuthorGrouped, >>removed because some levels have only a few values
         waterT, testLDGrouped, 
         freezingType, retentionGrouped, 
         
         seqGeneration, baseline, allocation, 
         #housing,  #remove because only 1 level
         blindExp, control, outAss,
         bias, blindRand, vi, exp) %>% 
  filter(domain != "noMeta") %>% 
  droplevels() -> datForest


# MetaForest: Tuning ------------------------------------------------------------------
##Attention: Tuning assumes that convergence is reached

set.seed(3238) #set seeds to a random number


# Set up 10-fold grouped CV
fit_control <- trainControl(method = "cv", index = groupKFold(datForest$exp, k = 10))

# Set up a custom tuning grid for the three tuning parameters of MetaForest
rf_grid <- expand.grid(whichweights = c("random", "fixed", "unif"),
                       mtry = c(2, 4, 6),
                       min.node.size = c(2, 4, 6))

# Train the model
cv.mf.cluster <- train(y = datForest$yi, x = datForest[,-1],#from x remove yi and each
                       study = "exp", method = ModelInfo_mf(),
                       trControl = fit_control,
                       tuneGrid = rf_grid)
cv.mf.cluster

#cross validated R2 - .12, sd= .09
cv.mf.cluster$results[which(cv.mf.cluster$results$whichweights == "unif" & 
                              cv.mf.cluster$results$mtry == 4 & 
                              cv.mf.cluster$results$min.node.size == 2),] #details with the "best" model


cv.mf.cluster$finalModel #R2oob = .09, tau2 = .46

#convergence plot
plot(cv.mf.cluster$finalModel) 
#dev.copy(tiff,'MAB_figures/MAB_metaforest_convergencePlot.tiff')
#dev.off()

VarImpPlot(cv.mf.cluster$finalModel)
#dev.copy(tiff,'MAB_figures/MAB_metaforest_varImportance.tiff')
#dev.off()
##remove variables with negative variable importance?

# Metaforest plots --------------------------------------------------
variable <- c("year","species","speciesStrainGrouped","origin","sex","ageWeek",
              "model","mTimeLength","mCageGrouped","mLDGrouped","mControlGrouped",
              "hit2Grouped","domain","testAuthorGrouped","waterT","testLDGrouped",
              "freezingType","retentionGrouped","seqGeneration","baseline","allocation",
              "blindExp","control","outAss","bias","blindRand") 

a <- 1

for (i in c(1:length(variable))) {
  
  thisMod <- variable[i]
  WeightedScatter(datForest, yi = "yi", vi = "vi", vars = thisMod, summarize = TRUE)
  ggsave(paste0("MAB_figures/MAB_metaforest_WeightedScatter_", thisMod, ".tiff"))
  
  PartialDependence(cv.mf.cluster$finalModel, vars = thisMod) 
  ggsave(paste0("MAB_figures/MAB_metaforest_partDep_", thisMod, ".tiff"))
  
  a <- 1
  
} # Plots saved in your folder

