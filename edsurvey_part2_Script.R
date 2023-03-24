############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
# to load the package	
library(EdSurvey)	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
t19 <- readTIMSS("~/TIMSS/2019/", countries="usa", grade=8)	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
summary2(sdf, "composite")	
summary2(t19, "mmat")	
############################################### Slide 8	
summary2(sdf, "composite", weightVar = NULL)	
summary2(t19, "mmat", weightVar = NULL)	
############################################### Slide 9	
summary2(sdf, "b017451")	
summary2(sdf, "b017451", omittedLevels = TRUE)	
############################################### Slide 10	
summary2(t19, "btbm19cc.math", weight="matwgt")	
summary2(t19, "btbm19cc.math",  weightVar="matwgt",	
         omittedLevels = TRUE)	
############################################### Slide 11	
############################################### Slide 12	
es1 <- edsurveyTable(composite ~ dsex + b017451, data = sdf)	
############################################### Slide 13	
library(knitr)	
library(kableExtra)	
kable(es1$data, format="html") %>%	
  kable_styling(font_size = 16)	
############################################### Slide 14	
es2 <- edsurveyTable(composite ~ dsex + b017451, data = sdf, pctAggregationLevel = 0)	
kable(es2$data, format="html") %>%	
  kable_styling(font_size = 16)	
############################################### Slide 15	
es1 <- edsurveyTable(mmat ~ bsbg01 + btbg02.math,	
                     data = t19, weightVar="matwgt")	
############################################### Slide 16	
kable(es1$data, format="html") %>%	
  kable_styling(font_size = 16)	
############################################### Slide 17	
es2 <- edsurveyTable(mmat ~ bsbg01 + btbg02.math, data = t19,	
                     pctAggregationLevel = 0, weightVar="matwgt")	
kable(es2$data, format="html") %>%	
  kable_styling(font_size = 16)	
############################################### Slide 18	
############################################### Slide 19	
############################################### Slide 20	
edexercise <- edsurveyTable(mmat ~ bsbg01 + bsdgslm,	
                           data = t19)	
edexercise	
############################################### Slide 21	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
lm1 <- lm.sdf(composite ~ dsex + b013801,	
              weightVar = 'origwt', data = sdf)	
summary(lm1)	
############################################### Slide 25	
lm1 <- lm.sdf(mmat ~ bsbg01 + bsdgslm,	
              weightVar = 'totwgt', data = t19)	
summary(lm1)	
############################################### Slide 26	
############################################### Slide 27	
lmexercise2 <- lm.sdf(mmat ~ bsbg04 + btbg06e.math,	
                      weightVar = 'matwgt', data = t19)	
summary(lmexercise2)	
############################################### Slide 28	
############################################### Slide 29	
############################################### Slide 30	
mmlA <- mml.sdf(algebra ~ dsex, data=sdf, weightVar="origwt")	
############################################### Slide 31	
############################################### Slide 32	
summary(mmlA)	
############################################### Slide 33	
lmA <- lm.sdf(algebra ~ dsex, data=sdf, weightVar="origwt")	
summary(lmA)	
t19$scoreDict	
table(sdf$m143601)	
scoreDict <- sdf$scoreDict	
subset(scoreDict, key %in% "m143601")	
table(sdf$m143501)	
subset(scoreDict, key %in% "m143501")	
dichotParamTab <- sdf$dichotParamTab	
subset(dichotParamTab, ItemID %in% "m143601")	
polyParamTab <- sdf$polyParamTab	
subset(polyParamTab, ItemID %in% "m143501")	
sdf$testData	
t19$testData	
############################################### Slide 34	
############################################### Slide 35	
# all time on screen variables (math and science)	
vn <- searchSDF(data=t19, c("time", "on", "screen"))$variableName	
# just math time on screen variables	
vnm <- vn[substr(vn,1,2) == "me"]	
t19$totalTimeMat <- pmax(5, apply(t19[,vnm], 1, sum, na.rm=TRUE)/60)	
vns <- vn[substr(vn,1,2) == "se"]	
t19$totalTimeSci <- pmax(5, apply(t19[,vns], 1, sum, na.rm=TRUE)/60)	
############################################### Slide 36	
lmA <- lm.sdf(mmat ~ bsbg01 + totalTimeMat + totalTimeSci,	
              data=t19, weightVar="totwgt")	
############################################### Slide 37	
summary(lmA)	
############################################### Slide 38	
mmlA <- mml.sdf(mmat ~ bsbg01 + totalTimeMat + totalTimeSci,	
                data=t19,	
                weightVar="totwgt")	
summary(mmlA)	
############################################### Slide 39	
summary(mmlA)	
############################################### Slide 40	
t19$totalTimeMatZ <- (t19$totalTimeMat - mean(t19$totalTimeMat))/sd(t19$totalTimeMat)	
t19$totalTimeSciZ <- (t19$totalTimeSci - mean(t19$totalTimeSci))/sd(t19$totalTimeSci)	
mmlB <- mml.sdf(mmat ~ bsbg01 + totalTimeMatZ + totalTimeSciZ,	
                data=t19,	
                weightVar="totwgt")	
############################################### Slide 41	
summary(mmlB)	
############################################### Slide 42	
t192 <- drawPVs(mmlB, data=t19, npv=5L)	
t192	
############################################### Slide 43	
lm2 <- lm.sdf(mmat_dire ~ bsbg01 + totalTimeMat + totalTimeSci,	
              data=t192)	
summary(lm2)	
############################################### Slide 44	
lm1a <- lm.sdf(mmat ~ bsbg01 + totalTimeMat + totalTimeSci,	
               data=t192)	
summary(lm1a)$coefmat	
lm1b <- lm.sdf(mmat_dire ~ bsbg01 + totalTimeMat + totalTimeSci,	
               data=t192)	
summary(lm1b)$coefmat	
############################################### Slide 45	
############################################### Slide 46	
mmlExercise1 <- mml.sdf(mmat ~ btbm19a.math, data = t19,	
                        weightVar="matwgt")	
summary(mmlExercise1)	
############################################### Slide 47	
############################################### Slide 48	
############################################### Slide 49	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 50	
############################################### Slide 51	
