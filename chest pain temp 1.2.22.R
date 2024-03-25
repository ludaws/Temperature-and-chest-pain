library(readxl); library(splines); library(dlnm); library(MASS); library(tsModel)

#FIGURE 1 OVERALL RELATIONSHIP 21D LAG
obs$date <- as.Date(obs$date, format="%d/%m/%Y");
argvar <- list(fun="ns", knots = quantile(obs$tmean,c(35,75)/100, na.rm=T),Bound=range(obs$tmean,na.rm=T));
maxlag21 <- 21;
arglag21 <- list(fun="ns",knots=logknots(maxlag21,nk=3));
cb21 <- crossbasis(obs$tmean,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=21, by=1)  ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2)

#SUPPLEMENTAL FIGURE I

plot(pred21,"3d",ltheta=210,xlab="Temperature (C)",zlim=c(0.85,1.1),ylab="Lag",zlab="RR",col=gray(0.9), main="Overall exposure-lag-response for chest pain")

//FOR SUPPLEMENTAL FIGURE II ///
Repeat Figure 1 analysis setting maxlag to 24-, 28-
Repeat figure 1 analysis changing df to 8 and 9
Repeat Figure 1 analysis changing knots to (10,75,90) and (10,35,65,75,90)

#TO GET MINIMUM TEMP

cp <- crosspred(cb21,m21,cen=varcen,by=0.1);
cp$allRRfit

#FIGURE 2 INDIVIDUAL DIAGNOSES 21D LAG
mcardiovasc <- glm(cardiovasc ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mresp <- glm(resp ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mothermed <- glm(othermed ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mnonspec <- glm(nonspec ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mmi <- glm(mi  ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mccf <- glm(ccf ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mafsvt <- glm(afsvt ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mothercard <- glm(othercard ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
predcardiovasc <- crosspred(cb21, mcardiovasc, cen =25, by=1) ; 
predresp <- crosspred(cb21, mresp, cen =21, by=1) ; 
predothermed <- crosspred(cb21, mothermed, cen =21, by=1)  ;
prednonspec <- crosspred(cb21, mnonspec, cen =20, by=1)  ;
predmi <- crosspred(cb21, mmi, cen =31, by=1)  ;
predccf <- crosspred(cb21, mccf, cen =13, by=1)  ;
predafsvt <- crosspred(cb21, mafsvt, cen =22, by=1)  ;
predothercard <- crosspred(cb21, mothercard, cen =26, by=1)  

plot(predcardiovasc,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(predresp,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(predothermed,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(prednonspec,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(predmi,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(predccf,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(predafsvt,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)
plot(predothercard,"overall",col="red",ylim=c(0.5,2.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3)

#FIGURE 3 INTERACTION BY SUBGROUPS (AGE CATEGORY; SEX; SES)

mage18to49 <- glm(age18to49 ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mage50to69<- glm(age50to69 ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mage70over <- glm(age70over ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
msexf <- glm(sexf ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
msexm <- glm(sexm ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mses3low<- glm(ses3low ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mses3mid <- glm(ses3mid ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mses3high <- glm(ses3high ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);

predage18to49<- crosspred(cb21, mage18to49, cen =21, by=1)  ;
predage50to69 <- crosspred(cb21, mage50to69, cen =20, by=1)  ;
predage70over <- crosspred(cb21, mage70over, cen =24, by=1)  ;
predsexf <- crosspred(cb21, msexf, cen =20, by=1)  ;
predsexm<- crosspred(cb21, msexm, cen =21, by=1)  ;
predses3low <- crosspred(cb21, mses3low, cen =21, by=1)  ;
predses3mid <- crosspred(cb21, mses3mid, cen =21, by=1)  ;
predses3high <- crosspred(cb21, mses3high, cen =21, by=1)  ;


plot(predsexf,"overall",col="firebrick2",ci="n",ylim=c(0.9,1.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3);
lines(predsexm,col="royalblue2",lwd=3);
legend(10,1.45,c("Female","Male"),xpd = TRUE,col=c("firebrick2","royalblue2"),lwd=2,bg="white",cex=0.9,ncol=1,inset=0.1,bty="n")

plot(predage70over,"overall",col=" dodgerblue2",ci="n",ylim=c(0.9,1.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3);
lines(predage50to69,col="goldenrod1",lwd=3);
lines(predage18to49,col=" firebrick1",lwd=3);
legend(10,1.45,c("18-49 years","50-69 years","Over 70 years"),xpd = TRUE,col=c("firebrick1"," goldenrod1"," dodgerblue2"),lwd=2,bg="white",cex=0.9,ncol=1,inset=0.1,bty="n")

plot(predses3low,"overall",col="firebrick1",ci="n",ylim=c(0.9,1.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3);
lines(predses3mid,col="goldenrod1",lwd=3);
lines(predses3high,col="dodgerblue2",lwd=3);
legend(12,1.45,c("Low SES","Medium SES","High SES"),xpd = TRUE,col=c("firebrick1","goldenrod1","dodgerblue2"),lwd=2,bg="white",cex=0.9,ncol=1,inset=0.1,bty="n")

#HOME VS NOT HOME

mhome <- glm(home ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
mnothome <- glm(nothome ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);

predhome <- crosspred(cb21, mhome, cen =20, by=1)  ;
prednothome<- crosspred(cb21, mnothome, cen =21, by=1)  ;

plot(predhome,"overall",col="firebrick2",ci="n",ylim=c(0.9,1.5),axes=T,lab=c(6,5,7),xlab=xlab, ylab="RR",lwd=3);
lines(prednothome,col="royalblue2",lwd=3);
legend(10,1.45,c("At home","Not at home"),xpd = TRUE,col=c("firebrick2","royalblue2"),lwd=2,bg="white",cex=0.9,ncol=1,inset=0.1,bty="n")

#CALIBRATION

obs$date <- as.Date(obs$date, format="%d/%m/%Y")
rcp4p5$date<-as.Date(rcp4p5$date,format=c("%Y-%m-%d"))
rcp8p5$date<-as.Date(rcp8p5$date,format=c("%Y-%m-%d"))

rcp4p5cal <- fhempel(obs[c("date","tmean")],rcp4p5)
rcp8p5cal <- fhempel(obs[c("date","tmean")],rcp8p5)
rcp4p5<-rcp4p5cal
rcp8p5<-rcp8p5cal


#PROJECTIONS (FIGURE 4)

maxlag21 <- 21;
arglag21 <- list(fun="ns",knots=logknots(maxlag21,nk=3));
cb21 <- crossbasis(obs$tmean,maxlag21,argvar,arglag21) ;
 m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=20.8, by=.1)  ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2)

deathdoy <-tapply(obs$chestpain,as.numeric(format(obs$date,"%j")),mean,na.rm=T)[seq(365)];
while(any(isna <- is.na(deathdoy))) deathdoy[isna] <- rowMeans(Lag(deathdoy,c(-1,1)),na.rm=T)[isna];
deathproj <- rep(deathdoy,length=nrow(rcp4p5));
varcen<-20.8;
red <- crossreduce(cb21,m21,cen=varcen);

coef <- coef(red);
vcov <- vcov(red);
deathperiod <- sum(deathdoy)*10;
histperiod <- "2015-2024";
projperiod<-paste(202:209,5,"-",substr(203:210,3,3),4,sep="");
 histseqperiod <- factor(rep(histperiod,length.out=365*length(seq(2015,2024))));
 projseqperiod <- factor(rep(projperiod,each=365*10));
seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)));
 levels(seqperiod) <- c(histperiod,projperiod);
 temprange <- c("tot","cold","heat");
 absrel <- c("abs","rel");
 gcm <- c("GFDL-ESM2M"="tmean_gfdlesm","HadGEM2-ES"="tmean_hadgem", "ACCESS 1-0"="tmean_access","MIROC-ESM-CHEM"="tmean_miroc","NorESM1-M"="tmean_noresm","CNRM"="tmean_cnrm");
 rcp <- c(RCP4.5="rcp4p5",RCP8.5="rcp8p5");
 nsim <- 1000;
 ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange), length(absrel), length(gcm),length(rcp),nsim+1),dimnames=list(levels(seqperiod),temprange,absrel,names(gcm),names(rcp),c("est",paste0("sim",seq(nsim)))));

 for (i in seq(rcp)) {
 cat("\n\n",names(rcp)[i],"\n")
 tmeanproj <- get(rcp[[i]])
 for(j in seq(gcm)) {
 cat(gcm[j],"")
 bvar <- do.call(onebasis,c(list(x=tmeanproj[,j+1]),argvar))
 cenvec <- do.call(onebasis,c(list(x=varcen),argvar))
 bvarcen <- scale(bvar,center=cenvec,scale=F)
 indheat <- tmeanproj[,j+1]>varcen
 an <- (1-exp(-bvarcen%*%coef))*deathproj
 ansim[,"tot","abs",j,i,1] <- tapply(an,seqperiod,sum)
 ansim[,"cold","abs",j,i,1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
 ansim[,"heat","abs",j,i,1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
 set.seed(13041975+j)
 coefsim <- mvrnorm(nsim,coef,vcov)
 for(s in seq(nsim)) {
 an <- (1-exp(-bvarcen%*%coefsim[s,]))*deathproj
 ansim[,"tot","abs",j,i,s+1] <- tapply(an,seqperiod,sum)
 ansim[,"cold","abs",j,i,s+1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
 ansim[,"heat","abs",j,i,s+1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
 }
 }
 }

 ansim[,,"rel",,,] <- ansim[,,"abs",,,] - ansim[rep("2015-2024",length(levels(seqperiod))),,"abs",,,];
 anabs[,"est",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",1],1:2,mean);
 anabs[,"ci.l",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",-1],1:2,quantile,0.025);
 anabs[,"ci.u",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",-1],1:2,quantile,0.975);
 anabs[,"est",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",1],1:2,mean);
 anabs[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.025);
 anabs[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.975);
 anrel[,"est",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",1],1:2,mean);
 anrel[,"ci.l",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",-1],1:2,quantile,0.025);
 anrel[,"ci.u",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",-1],1:2,quantile,0.975);
 anrel[,"est",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",1],1:2,mean);
 anrel[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.025);
 anrel[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.975);
 afabs[,,,] <- anabs[,,,]/deathperiod*100;
 afrel[,,,] <- anrel[,,,]/deathperiod*100

#DIAGNOSES PROJECTIONS (SUPP FIGURE V) (REPEAT FOR EACH DIAGNOSIS)
#MIN NUMBERS cardiovasc 25.6C; resp 20.6C; other med 20.9; nonspec 19.9C; mi 31.9C; afsvt 22.5C; ccf 13.2C; othercard 26.5C)

deathdoy <-tapply(obs$othercard,as.numeric(format(obs$date,"%j")),mean,na.rm=T)[seq(365)];
 while(any(isna <- is.na(deathdoy))) deathdoy[isna] <- rowMeans(Lag(deathdoy,c(-1,1)),na.rm=T)[isna];
 deathproj <- rep(deathdoy,length=nrow(rcp4p5));
 varcen<-19.5;
 red <- crossreduce(cb21,mothercard,cen=varcen);
 coef <- coef(red);
 vcov <- vcov(red);
 deathperiod <- sum(deathdoy)*10;
 histperiod <- "2015-2024";
projperiod<-paste(202:209,0,"-",substr(202:209,3,3),9,sep="");
 histseqperiod <- factor(rep(histperiod,length.out=365*length(seq(2015,2024))));
 projseqperiod <- factor(rep(projperiod,each=365*10));
 seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)));
 levels(seqperiod) <- c(histperiod,projperiod);
 temprange <- c("tot","cold","heat");
 absrel <- c("abs","rel");
 gcm <- c("GFDL-ESM2M"="tmean_gfdlesm","HadGEM2-ES"="tmean_hadgem", "ACCESS 1-0"="tmean_access","MIROC-ESM-CHEM"="tmean_miroc","NorESM1-M"="tmean_noresm","CNRM"="tmean_cnrm")
 rcp <- c(RCP4.5="rcp4p5",RCP8.5="rcp8p5");
 nsim <- 1000;
 ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange), length(absrel), length(gcm),length(rcp),nsim+1),dimnames=list(levels(seqperiod),temprange,absrel,names(gcm),names(rcp),c("est",paste0("sim",seq(nsim)))));

 for (i in seq(rcp)) {
 cat("\n\n",names(rcp)[i],"\n")
 tmeanproj <- get(rcp[[i]])
 for(j in seq(gcm)) {
 cat(gcm[j],"")
 bvar <- do.call(onebasis,c(list(x=tmeanproj[,j+1]),argvar))
 cenvec <- do.call(onebasis,c(list(x=varcen),argvar))
 bvarcen <- scale(bvar,center=cenvec,scale=F)
 indheat <- tmeanproj[,j+1]>varcen
 an <- (1-exp(-bvarcen%*%coef))*deathproj
 ansim[,"tot","abs",j,i,1] <- tapply(an,seqperiod,sum)
 ansim[,"cold","abs",j,i,1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
 ansim[,"heat","abs",j,i,1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
 set.seed(13041975+j)
 coefsim <- mvrnorm(nsim,coef,vcov)
 for(s in seq(nsim)) {
 an <- (1-exp(-bvarcen%*%coefsim[s,]))*deathproj
 ansim[,"tot","abs",j,i,s+1] <- tapply(an,seqperiod,sum)
 ansim[,"cold","abs",j,i,s+1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
 ansim[,"heat","abs",j,i,s+1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
 }
 }
 }

ansim[,,"rel",,,] <- ansim[,,"abs",,,] - ansim[rep("2015-2024",length(levels(seqperiod))),,"abs",,,];
 anabs[,"est",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",1],1:2,mean);
 anabs[,"ci.l",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",-1],1:2,quantile,0.025);
 anabs[,"ci.u",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",-1],1:2,quantile,0.975);
 anabs[,"est",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",1],1:2,mean);
 anabs[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.025);
 anabs[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.975);
 anrel[,"est",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",1],1:2,mean);
 anrel[,"ci.l",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",-1],1:2,quantile,0.025);
 anrel[,"ci.u",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",-1],1:2,quantile,0.975);
 anrel[,"est",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",1],1:2,mean);
 anrel[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.025);
 anrel[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.975);
 afabs[,,,] <- anabs[,,,]/deathperiod*100;
 afrel[,,,] <- anrel[,,,]/deathperiod*100


#SUBGROUP PROJECTIONS (FIGURE 5 AND SUPP FIGURE VI AND VII)
#MIN NUMBERS age18to49: 20.6C; age50to69: 20.3C; age70over: 24.6C; sexf 20.5C; sexm 21.0C; ses3low 20.9C; ses3mid 20.7C; ses3high 21.2C) REPEAT FOR EACH SUBGROUP///

deathdoy <-tapply(obs$age18to49,as.numeric(format(obs$date,"%j")),mean,na.rm=T)[seq(365)];
 while(any(isna <- is.na(deathdoy))) deathdoy[isna] <- rowMeans(Lag(deathdoy,c(-1,1)),na.rm=T)[isna];
 deathproj <- rep(deathdoy,length=nrow(rcp4p5));
 varcen<-21.6
 red <- crossreduce(cb21,mage18to49,cen=varcen);
 coef <- coef(red);
 vcov <- vcov(red);
 deathperiod <- sum(deathdoy)*10;
 histperiod <- "2015-2024";
projperiod<-paste(202:209,0,"-",substr(202:209,3,3),9,sep="");
 histseqperiod <- factor(rep(histperiod,length.out=365*length(seq(2015,2024))));
 projseqperiod <- factor(rep(projperiod,each=365*10));
 seqperiod <- factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)));
 levels(seqperiod) <- c(histperiod,projperiod);
 temprange <- c("tot","cold","heat");
 absrel <- c("abs","rel");
 gcm <- c("GFDL-ESM2M"="tmean_gfdlesm","HadGEM2-ES"="tmean_hadgem", "ACCESS 1-0"="tmean_access","MIROC-ESM-CHEM"="tmean_miroc","NorESM1-M"="tmean_noresm","CNRM"="tmean_cnrm")
 rcp <- c(RCP4.5="rcp4p5",RCP8.5="rcp8p5");
 nsim <- 1000;
 ansim <- array(NA,dim=c(length(levels(seqperiod)),length(temprange), length(absrel), length(gcm),length(rcp),nsim+1),dimnames=list(levels(seqperiod),temprange,absrel,names(gcm),names(rcp),c("est",paste0("sim",seq(nsim)))));

 for (i in seq(rcp)) {
 cat("\n\n",names(rcp)[i],"\n")
 tmeanproj <- get(rcp[[i]])
 for(j in seq(gcm)) {
 cat(gcm[j],"")
 bvar <- do.call(onebasis,c(list(x=tmeanproj[,j+1]),argvar))
 cenvec <- do.call(onebasis,c(list(x=varcen),argvar))
 bvarcen <- scale(bvar,center=cenvec,scale=F)
 indheat <- tmeanproj[,j+1]>varcen
 an <- (1-exp(-bvarcen%*%coef))*deathproj
 ansim[,"tot","abs",j,i,1] <- tapply(an,seqperiod,sum)
 ansim[,"cold","abs",j,i,1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
 ansim[,"heat","abs",j,i,1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
 set.seed(13041975+j)
 coefsim <- mvrnorm(nsim,coef,vcov)
 for(s in seq(nsim)) {
 an <- (1-exp(-bvarcen%*%coefsim[s,]))*deathproj
 ansim[,"tot","abs",j,i,s+1] <- tapply(an,seqperiod,sum)
 ansim[,"cold","abs",j,i,s+1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
 ansim[,"heat","abs",j,i,s+1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
 }
 }
 }

ansim[,,"rel",,,] <- ansim[,,"abs",,,] - ansim[rep("2015-2024",length(levels(seqperiod))),,"abs",,,];
 anabs[,"est",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",1],1:2,mean);
 anabs[,"ci.l",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",-1],1:2,quantile,0.025);
 anabs[,"ci.u",,"RCP4.5"] <- apply(ansim[,,"abs",,"RCP4.5",-1],1:2,quantile,0.975);
 anabs[,"est",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",1],1:2,mean);
 anabs[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.025);
 anabs[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"abs",,"RCP8.5",-1],1:2,quantile,0.975);
 anrel[,"est",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",1],1:2,mean);
 anrel[,"ci.l",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",-1],1:2,quantile,0.025);
 anrel[,"ci.u",,"RCP4.5"] <- apply(ansim[,,"rel",,"RCP4.5",-1],1:2,quantile,0.975);
 anrel[,"est",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",1],1:2,mean);
 anrel[,"ci.l",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.025);
 anrel[,"ci.u",,"RCP8.5"] <- apply(ansim[,,"rel",,"RCP8.5",-1],1:2,quantile,0.975);
 afabs[,,,] <- anabs[,,,]/deathperiod*100;
 afrel[,,,] <- anrel[,,,]/deathperiod*100



#OTHER EXPOSURES (SUPP FIGURE III)

argvar <- list(fun="ns", knots = quantile(obs$tmax,c(25,75)/100, na.rm=T),Bound=range(obs$tmax,na.rm=T));
cb21 <- crossbasis(obs$tmax,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=26, by=1) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$tmin,c(25,75)/100, na.rm=T),Bound=range(obs$tmin,na.rm=T));
cb21 <- crossbasis(obs$tmin,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=15, by=1) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$tdiff,c(25,75)/100, na.rm=T),Bound=range(obs$tdiff,na.rm=T));
cb21 <- crossbasis(obs$tdiff,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=8, by=1) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);


argvar <- list(fun="ns", knots = quantile(obs$pm25,c(25,75)/100, na.rm=T),Bound=range(obs$pm25,na.rm=T));
cb21 <- crossbasis(obs$pm25,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=6, by=.1) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab="Particulate matter 2.5 (ug/m3)", ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$pm10,c(25,75)/100, na.rm=T),Bound=range(obs$pm10,na.rm=T));
cb21 <- crossbasis(obs$pm10,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=18, by=.1) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),xlim=c(0,70),axes=T,lab=c(6,5,7),xlab="Particulate matter 10 (ug/m3)", ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$api,c(25,75)/100, na.rm=T),Bound=range(obs$api,na.rm=T));
cb21 <- crossbasis(obs$api,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=.5, by=.01) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab="Airborn particle index (units)", ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$co,c(25,75)/100, na.rm=T),Bound=range(obs$co,na.rm=T));
cb21 <- crossbasis(obs$co,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=.1, by=.01) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),xlim=c(0,2),axes=T,lab=c(6,5,7),xlab="Carbon monoxide (ppm)", ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$o3,c(25,75)/100, na.rm=T),Bound=range(obs$o3,na.rm=T));
cb21 <- crossbasis(obs$o3,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=16, by=.1) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),xlim=c(0,65),axes=T,lab=c(6,5,7),xlab="Ozone (ug/m3)", ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$so2,c(25,75)/100, na.rm=T),Bound=range(obs$so2,na.rm=T));
cb21 <- crossbasis(obs$so2,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=.3, by=.01) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab="So2", ylab="Relative Risk",lwd=2);

argvar <- list(fun="ns", knots = quantile(obs$no2,c(25,75)/100, na.rm=T),Bound=range(obs$no2,na.rm=T));
cb21 <- crossbasis(obs$no2,maxlag21,argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=5, by=.11) ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab="Nitrogen dioxide (ppb)", ylab="Relative Risk",lwd=2)


#SUPP FIGURE HISTORICAL PROJECTED TEMPS RCP45 RCP85 (SUPP FIGURE IV)

tsyear <- as.numeric(format(rcp4p5$date,format="%Y"))
year <- unique(tsyear)
rcp4p5avg <- apply(rcp4p5[,-1],2,tapply,tsyear,mean)
>rcp8p5avg <- apply(rcp8p5[,-1],2,tapply,tsyear,mean)
rcp4p5avg <- rcp4p5avg-mean(rcp4p5avg[year%in%2010:2019,])
rcp8p5avg <- rcp8p5avg-mean(rcp8p5avg[year%in%2010:2019,])
> plot(year,rowMeans(rcp8p5avg),type="n",bty="l",ylab=ylab,xlab="Year", ylim=c(-1,6),xlim=c(2010,2099),main="") 
abline(h=0)
lines(year[year<2020],rowMeans(cbind(rcp4p5avg,rcp8p5avg)[year<2020,]),lwd=1.5)
polygon(c(year[year>=2020],rev(year[year>=2020])),c(apply(rcp4p5avg,1,max)[year>=2020],rev(apply(rcp4p5avg,1,min)[year>=2020])),col=col1,border=NA)
polygon(c(year[year>=2020],rev(year[year>=2020])),c(apply(rcp8p5avg,1,max)[year>=2020],rev(apply(rcp8p5avg,1,min)[year>=2020])),col=col2,border=NA)
lines(year[year>=2020],rowMeans(rcp4p5avg[year>=2020,]),lwd=1.5,col=4)
 lines(year[year>=2020],rowMeans(rcp8p5avg[year>=2020,]),lwd=1.5,col=2)
abline(v=2020,lty=3)
rect(2103-1,mean(apply(rcp4p5avg,1,min)[as.character(2090:2099)]),2103+1,mean(apply(rcp4p5avg,1,max)[as.character(2090:2099)]),col=4,border=NA)
rect(2106-1,mean(apply(rcp8p5avg,1,min)[as.character(2090:2099)]),2106+1,mean(apply(rcp8p5avg,1,max)[as.character(2090:2099)]),col=2,border=NA)
legend("top",c("Historical","RCP4.5","RCP8.5"),lwd=1.5,col=c(1,4,2),cex=0.8,bty="n",inset=0.05)


#SENSITIVITY ANALYSES ADJUST FOR POLLUTION

argvar <- list(fun="ns", knots = quantile(obs$tmean,c(25,75)/100, na.rm=T),Bound=range(obs$tmean,na.rm=T));
pm25_argvar <- list(fun="ns", knots = quantile(obs$pm25,c(25,75)/100, na.rm=T),Bound=range(obs$tmean,na.rm=T));
pm10_argvar <- list(fun="ns", knots = quantile(obs$pm10,c(25,75)/100, na.rm=T),Bound=range(obs$tmean,na.rm=T));
o3_argvar <- list(fun="ns", knots = quantile(obs$o3,c(25,75)/100, na.rm=T),Bound=range(obs$tmean,na.rm=T));
co_argvar <- list(fun="ns", knots = quantile(obs$co,c(25,75)/100, na.rm=T),Bound=range(obs$tmean,na.rm=T));
maxlag21 <- 21;
arglag21 <- list(fun="ns",knots=logknots(maxlag21,nk=3));
cb21 <- crossbasis(obs$tmean,maxlag21,argvar,arglag21)  ;
pm2521 <- crossbasis(obs$pm25,maxlag21,pm25_argvar,arglag21)  ;
pm1021 <- crossbasis(obs$pm10,maxlag21,pm10_argvar,arglag21)  ;
o321 <- crossbasis(obs$o3,maxlag21,o3_argvar,arglag21)  ;
co21 <- crossbasis(obs$co,maxlag21,co_argvar,arglag21)  ;
m21 <- glm(chestpain ~ cb21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pred21 <- crosspred(cb21, m21, cen=21, by=1)  ;
plot(pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2)

pm25m21 <- glm(chestpain ~ cb21 + pm2521 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pm25pred21 <- crosspred(cb21, pm25m21, cen=21, by=1)  ;
plot(pm25pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);

pm10m21 <- glm(chestpain ~ cb21 + pm1021 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pm10pred21 <- crosspred(cb21, pm10m21, cen=21, by=1)  ;
plot(pm10pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);

o3m21 <- glm(chestpain ~ cb21 + o321 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
o3pred21 <- crosspred(cb21, o3m21, cen=21, by=1)  ;
plot(o3pred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);

com21 <- glm(chestpain ~ cb21 + co21 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
copred21 <- crosspred(cb21, com21, cen=21, by=1)  ;
plot(copred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2);

pollm21 <- glm(chestpain ~ cb21 + pm2521 + pm1021 + co21 + o321 + dow + ns(date,df=round(8*length(date)/365.25)),data=obs, family=quasipoisson);
pollpred21 <- crosspred(cb21, pollm21, cen=22, by=1)  ;
plot(pollpred21,"overall",col="firebrick1",ylim=c(0.8,1.4),axes=T,lab=c(6,5,7),xlab=xlab, ylab="Relative Risk",lwd=2)



