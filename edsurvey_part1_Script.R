############################################### Slide 1	
############################################### Slide 2	
############################################### Slide 3	
############################################### Slide 4	
############################################### Slide 5	
############################################### Slide 6	
############################################### Slide 7	
############################################### Slide 8	
############################################### Slide 9	
############################################### Slide 10	
############################################### Slide 11	
############################################### Slide 12	
#install EdSurvey	
# you may need to get rtools	
install.packages("EdSurvey")	
# to load the package	
library(EdSurvey)	
############################################### Slide 13	
vignette("introduction", package="EdSurvey")	
help(package = "EdSurvey")	
############################################### Slide 14	
############################################### Slide 15	
############################################### Slide 16	
############################################### Slide 17	
sdf <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))	
t19 <- readTIMSS("~/TIMSS/2019/", countries="usa", grade=8)	
############################################### Slide 18	
math17 <- readNAEP("//path_to_directory/Data/M48NT2AT.dat")	
############################################### Slide 19	
############################################### Slide 20	
############################################### Slide 21	
############################################### Slide 22	
############################################### Slide 23	
############################################### Slide 24	
############################################### Slide 25	
############################################### Slide 26	
############################################### Slide 27	
############################################### Slide 28	
############################################### Slide 29	
show(sdf)	
############################################### Slide 30	
show(t19)	
############################################### Slide 31	
dim(sdf)	
dim(t19)	
############################################### Slide 32	
colnames(sdf)	
############################################### Slide 33	
colnames(t19)	
############################################### Slide 34	
searchSDF("education", sdf)	
searchSDF("b003501", sdf, levels = TRUE)	
############################################### Slide 35	
levelsSDF("b018201", sdf)	
############################################### Slide 36	
View(showCodebook(sdf))	
############################################### Slide 37	
table(sdf$b003501)	
sdf$mother_hs_grad <- ifelse(sdf$b003501 %in% 	
                               c("Graduated H.S.","Some ed after H.S."),1,0)	
table(sdf$mother_hs_grad)	
############################################### Slide 38	
showPlausibleValues(sdf)	
showPlausibleValues(sdf, verbose = TRUE)	
############################################### Slide 39	
showWeights(sdf)	
showWeights(sdf, verbose = TRUE)	
############################################### Slide 40	
searchSDF("education", t19)	
searchSDF("bsdgedup", t19, levels = TRUE)	
############################################### Slide 41	
levelsSDF("bsdgedup", t19)	
############################################### Slide 42	
View(showCodebook(t19))	
############################################### Slide 43	
table(t19$bsdgedup)	
t19$parent_university <- ifelse(t19$bsdgedup %in% 	
                                  c("UNIVERSITY OR HIGHER"),1,0)	
table(t19$parent_university)	
############################################### Slide 44	
showPlausibleValues(t19)	
showPlausibleValues(t19, verbose = TRUE)	
############################################### Slide 45	
showWeights(t19)	
############################################### Slide 46	
showWeights(t19, verbose = TRUE)	
############################################### Slide 47	
############################################### Slide 48	
############################################### Slide 49	
############################################### Slide 50	
############################################### Slide 51	
############################################### Slide 52	
dtN <- getData(sdf, varnames = c('dsex', 'sdracem', 'b018201', 'b017451',	
                                   'composite', 'geometry', 'origwt'),	
              addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 53	
dtT <- getData(t19, varnames = c('bsdgedup', 'mmat', 'totwgt'),	
              addAttributes = TRUE, omittedLevels = FALSE)	
############################################### Slide 54	
# Note: head returns the first 6 rows of a data frame	
head(dtN)	
############################################### Slide 55	
# Note: head returns the first 6 rows of a data frame	
head(dtT)	
############################################### Slide 56	
subsetSDF <- subset(sdf, dsex %in% c("Male"))	
dim(sdf)	
dim(subsetSDF)	
############################################### Slide 57	
t19_females <- subset(t19, itsex %in% c("FEMALE"))	
dim(t19)	
dim(t19_females)	
############################################### Slide 58	
sdf2 <- recode.sdf(sdf, recode =	
                     list(b017451 = list(from = c("Never or hardly ever", "Once every few weeks"),	
                                         to = c("Infrequently")),	
                          b017451 = list(from = c("Every day"),	
                                        to = c("Frequently")))	
                   )	
searchSDF("b017451", sdf2, levels = TRUE)	
############################################### Slide 59	
t19$books <- ifelse(t19$bsbg04 %in% c("NONE OR VERY FEW (0-10 BOOKS)", "ENOUGH TO FILL ONE SHELF (11-25 BOOKS)"), "0-25", as.character(t19$bsbg04))	
table(t19$books, t19$bsbg04)	
############################################### Slide 60	
res <- searchSDF(c("visits", "screen"), t19)	
t19$visits <- apply(t19[,res$variableName], 1, sum, na.rm=TRUE)	
############################################### Slide 61	
############################################### Slide 62
# Running this code will require the packages ggplot2 and dplyr. 
# If you don't have these installed and want to run this code, 
# uncomment run this line:
# install.packages(c("dplyr", "ggplot2"))`

dtT <- getData(t19, varnames = c('bsbg01','mmat','visits','bsbg03','totwgt'),	
               omittedLevels = TRUE)	
	
dtT %>% filter(visits <= 250) %>%	
  ggplot(.,aes(x=visits, fill=bsbg03)) +	
  geom_histogram(aes(y=10*..density..),binwidth=10) + facet_wrap(~bsbg03) +	
  theme_minimal() + ylab("") + xlab("Visits")	
	
dtT <- rebindAttributes(dtT, t19)	
dtT.lm <- lm.sdf(formula = mmat ~ bsbg01 + bsbg03 + visits, data = dtT)	
	
############################################### Slide 63	
