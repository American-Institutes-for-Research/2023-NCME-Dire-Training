# 2023-NCME-Dire-Training

Greetings! 

Thank you for registering the NCME virtual training, **Tools For Analyzing NAEP and TIMSS Data in R Using Latent Regression**, which will be held from 1:00 pm-5:00 pm on Tuesday, March 28th, 2023. We are very excited to welcome you all!  

Before the training, please install: 

- [R for your operating system](https://www.r-project.org/) 

- [RStudio Desktop](https://www.rstudio.com/products/rstudio/)  

To initialize your system, open RStudio and run the following script:
```
install.packages(c("Dire", "EdSurvey"))
library(EdSurvey)
```
To practice the EdSurvey sections with 2019 TIMSS datasets, download the datasets using the following script, once EdSurvey is successfully installed.
```
library(EdSurvey)
downloadTIMSS(years = 2019, root = "~/")
```
Note: you may need to change the `root` argument depending on your system. Downloading the data files may take several hours to complete.

Warm Regards, 

Ting, Paul, Sinan, Blue and Emmanuel





