
devtools::install_github("PeterVerboon/cyclicpkg/cyclic")

library(cyclic)

################ START TUTORIAL ##############################

require(ggplot2)

a0 <- 1; b1 <- 2; b2 <- 10; P <- 24;  T <- seq(0, 24, by =0.1); 
y <- a0 + b1*cos(2*(pi/P)*(T - b2))   
g <- ggplot() + geom_line(aes(x=T, y=y))  
g <- g + labs(x = "Time points", y = "Dependent variable ") 
g <- g + theme(axis.text = element_text(size = 10, colour="black")) 
g <- g + scale_x_continuous(breaks = seq(0,24,2))  
g <- g + geom_line(aes(x=b2, y=seq(1,3,0.1)), lty="dashed")  
g <- g + geom_line(aes(x=seq(0,24,0.1), y=a0))  
g <- g + annotate("text", x = 1, y = 1.2, label = "b0") 
g <- g + annotate("text", x = 10.8, y = 2, label = "b1") 
g <- g + annotate("text", x = 10, y = 0.8, label = "b2") 
g

#load("data/smokedat.rda")
data("smokedat")
names(smokedat)


dat1 <- smokedat

nall <- length(unique(dat1$subjnr))

## Count number of record per subject

dat1$count <- 1
dat2 <- aggregate(dat1[,c("count")],by=list(dat1$subjnr), FUN=sum, na.rm=T);
dat2$subjnr <- dat2$Group.1
dat1$count <- NULL;
dat2$count <- dat2[,2]

dat3 <- merge(dat1,dat2[,c("subjnr","count")], by.x = "subjnr")

rm(dat2)

## remove subjects with less than 50 records 

dat3 <- subset(dat3, dat3$count >= 50)
nfew <- nall - length(unique(dat3$subjnr))

## compute variability of DV per subject

dat2 <- aggregate(dat3$intention,by=list(dat3$subjnr), FUN=sd, na.rm=F)
dat2$subjnr <- dat2$Group.1
dat2$Group.1 <- NULL
dat2 <- merge(dat3, dat2,  by.x = "subjnr")

# remove subjects with very small variation in DV

dat3 <- subset(dat2, dat2$V1 > .10)  

rm(dat2)
nunique <- length(unique(dat3$subjnr))
nsmall <- (nall - nfew) - nunique

cat(" Number of subjects in data set:              ", nall," \n",
    "Number of subjects with less than 50 records: ", nfew ," \n" ,
    "Number of subjects with SD smaller than .10:  ", nsmall," \n" ,
    "Number of subjects used in analysis:         ", nunique, "\n")


## aggregate over subjects

dat4 <- aggregate(dat3[,c("positiveAffect","stress","intention")],
                  by=list(dat3$beepnr,dat3$daynr), FUN=mean, na.rm=F);    
dat4$daynr <- dat4$Group.2
dat4$beepnr <- dat4$Group.1
dat4$Group.1 <- NULL
dat4$Group.2 <- NULL

### plot raw data

pdat <- dat4                                      # averaged over all subjects
#save(pdat, file= "pdat.rda")

pdat <- subset(dat3, dat3$subjnr == 15)           # choose ppn 2, 15, 18 
                                                     
npoints <- dim(pdat)[1]
x <- c(1:npoints)
pdat$day <- as.factor(pdat$daynr)


#### STEP 1
# The raw data for three subjects and average are plotted. Repeat thsi code 4 times with pdat chosen as above.
# We have to repeat this step for the aggregated data in dat4, and for subjects 2, 15 and 18.
# This makes Figure 2 from the tutorial.


g <- ggplot(pdat,aes(x=x, y=pdat$intention, colour=pdat$day)) + geom_point() +
     scale_x_discrete(name ="Time points (beeps within days)",  labels=pdat$beepnr, limits=c(1:npoints)) +
     theme(axis.text = element_text(size = 6, colour="black"),legend.position="none") 
g



#### STEP 2
# Now we analyze the cyclic model and plot the result, again for the average and for subjects 2, 15, and 18. 
# This is Figure 3 from the tutorial.

model_a <- fitCyclic(pdat,yvar = "intention",  xvar ="beepnr", grp = "daynr", 
                    ymin = -2.5, ymax = 1.5, step=.30)
print(model_a)
plot(model_a)



#### STEP 3  fit extra model with day as covariate 

model_b <- fitCyclic(dat4,  yvar = "intention", xvar ="beepnr",grp = "daynr", 
                     cov = "daynr", ymin=-0.5, ymax=0.5)
print(model_b)


#### STEP 4
# Apply the cylic model with daily period for the variables stress and positive affect for subject 15 
# to obtain a plot of the raw data with the fitted cycles and a plot of the aggregated data. 
# Together with intention (model_a) this makes Figure 4.

pdat <- subset(dat3, dat3$subjnr == 15)   

model_c <- fitCyclic(pdat,yvar = "intention", xvar ="beepnr", grp = "daynr", 
                     ymin = -1.0, ymax = 0.5, step= 0.25)
plot(model_c, type = c("raw","means"))
model_c <- fitCyclic(pdat,yvar = "stress", xvar ="beepnr", grp = "daynr", 
                     ymin = -1.0, ymax = 0.5, step= 0.25)
plot(model_c, type = c("raw","means"))
model_c <- fitCyclic(pdat,yvar = "positiveAffect", xvar ="beepnr", grp = "daynr", 
                     ymin = -1.0, ymax = 0.5, step= 0.25)
plot(model_c,type = c("raw","means"))



#### Step 5
## Apply, only for subject 15, the cylic model with a weekly period for intention, stress and positive affect, instead of a daily period. 
## Below the code is given which produces the six separate plots of figure 5.

pdat <- subset(dat3, dat3$subjnr == 15)   

model_d <- fitCyclic(pdat, yvar = "intention", xvar ="daynr", grp = "daynr", 
                     ymin = -2.0, ymax = 1.0, step= 0.25)
plot(model_d, type = c("raw","means"))
model_d <- fitCyclic(pdat, yvar = "stress", xvar ="daynr", grp = "daynr", 
                     ymin = -2.0, ymax = 1.0, step= 0.25)
plot(model_d, type = c("raw","means"))
model_d <- fitCyclic(pdat, yvar = "positiveAffect", xvar ="daynr", grp = "daynr", 
                     ymin = -2.0, ymax = 1.0, step= 0.25)
plot(model_d, type = c("raw","means"))


#### STEP 6
# Analyze the data with the cyclic model using  MLA and model comparison. 
# Each consecutive model is more complex. The intraclass correlation is computed with the null model (model1).

model1 <- fitCyclicMLA(dat=dat3, yvar="intention", id = "subjnr")
print(model1)

## Intraclass correlation

model2 <- fitCyclicMLA(dat=dat3, yvar="intention", xvar1="beepnr",xvar2="daynr", id = "subjnr", 
                       ncycle = 1, random = "intercept" )
print(model2)

model3 <- fitCyclicMLA(dat=dat3, yvar="intention", xvar1="beepnr",xvar2="daynr", id = "subjnr", 
                      ncycle = 1, random = "first")
print(model3)

## Fitting two cyclic process: within day and within week

model4 <- fitCyclicMLA(dat=dat3, yvar="intention", xvar1="beepnr", xvar2="daynr",id = "subjnr", 
                        ncycle = 2, random = "first")
print(model4)

model5 <- fitCyclicMLA(dat=dat3, yvar="intention", xvar1="beepnr", xvar2="daynr",id = "subjnr",
                       ncycle = 2, random = "all" )
print(model5)
plot(model5)      # This gives figure 6

model6 <- fitCyclicMLA(dat=dat3, yvar="intention", xvar1="beepnr", xvar2="daynr",id = "subjnr", cov = "stress",
                        ncycle = 2, random = "all" )
print(model6)


# compare the models

anova(model6$fit, model5$fit, model4$fit,model3$fit, model2$fit, model1$fit)  


