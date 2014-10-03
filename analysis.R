rm(list = ls())

#install austin
#install.packages("austin", repos="http://r-forge.r-project.org", type="source")

#load packages
require(tm)
require(RWeka)
require(rJava)
require(Snowball)
require(austin)
require(ggplot2)
require(foreign)
require(plyr)
require(stargazer)

#set working dir here
setwd("C:\Users\fh\Documents\GitHub\ccv")

################################################################
# GERMANY
################################################################

### Documents Corpus

pol.text.corp <- Corpus(DirSource("Germany", encoding = "UTF-8"),
  readerControl = list(language = "de")) # Disregard error messageget

pol.text.corp <- tm_map(pol.text.corp, 
                        function(x) iconv(enc2utf8(x), sub = "byte"))  
pol.text.corp <- tm_map(pol.text.corp, stripWhitespace)
pol.text.corp <- tm_map(pol.text.corp, removePunctuation)
pol.text.corp <- tm_map(pol.text.corp, removeNumbers)
pol.text.corp <- tm_map(pol.text.corp, tolower)
pol.text.corp <- tm_map(pol.text.corp, function(x) removeWords(x,  stopwords("de")))

#stemming takes a while - log time to see how long exactly
t1<-proc.time()
pol.text.corp.stemmed <- tm_map(pol.text.corp, stemDocument,language="german") #this takes ages
t2<-proc.time()
t2-t1

#at this point we create a "laconic Germany" using 1/8 of the number of words in order to see how that affects validity
#create matrix that defines laconic Germany
#first col: text names
laconicger<-as.data.frame(matrix(NA,ncol=3,nrow=length(names(pol.text.corp))))
#first col: text names
laconicger$V1<-names(pol.text.corp)
laconicger
#second col: current number of words in manifesto
for (i in 1:nrow(laconicger)){
  laconicger$V2[i]<-as.numeric(length(unlist(strsplit(paste(pol.text.corp[[i]],collapse=""),split=" {1,}"))))
}
#third col: 1/8 of the words of each manifesto
laconicger$V3<-round(laconicger$V2/8)

#create list of laconic manifestos, sampling 1/8 of the words of each original
lacgerlist<-as.list(rep(NA,length(names(pol.text.corp))))
names(lacgerlist)<-names(pol.text.corp)
for (i in 1:length(lacgerlist)){
  lacgerlist[[i]]<-sample(unlist(strsplit(paste(pol.text.corp[[i]],collapse=""),split=" {1,}")),laconicger$V3[i],replace=F)
}

#create new corpus of laconic manifestos
pol.text.corp.stemmed.lac <- Corpus(VectorSource(lacgerlist, encoding = "UTF-8"),
                        readerControl = list(language = "de")) # Disregard error messageget

#create 3 word frequency matrices: 1 non-stemmed, 1 stemmed, 1 stemmed+laconic
germany_wfm<-as.wfm(TermDocumentMatrix(pol.text.corp))
germany_wfms<-as.wfm(TermDocumentMatrix(pol.text.corp.stemmed))
lgermany_wfms<-as.wfm(TermDocumentMatrix(pol.text.corp.stemmed.lac))

#summary stats for describing data
germansumstats<-as.data.frame(matrix(NA,nrow=1,ncol=5))
names(germansumstats)<-c("cty","eyrs","ppyr","lenmean","lensd")
germansumstats$cty<-"Germany"
germansumstats$eyrs<-9
germansumstats$ppyr<-ncol(germany_wfms)/germansumstats$eyrs
germansumstats$lenmean<-mean(as.numeric(colSums(germany_wfms)))
germansumstats$lensd<-sd(as.numeric(colSums(germany_wfms)))

#we only use the stemmed wfm from here on

### ESTIMATES DATA FRAME
germandat<-as.data.frame(matrix(NA,ncol=9,nrow=ncol(germany_wfms)))
names(germandat)<-c("party","yr","ws","ws.se","wf","wf.se","cmp","exp","vot")

### Wordfish
gwf <- wordfish(germany_wfms)
summary(gwf)
germandat$wf<-gwf$theta
germandat$wf.se<-gwf$se.theta
germandat$party<-substr(gwf$docs,1,3)
germandat$yr<-substr(gwf$docs,4,7)

### CMP
cmp<-read.csv("cmp.csv")
germancmp<-cmp[cmp$countryname=="Germany",]
germancmp<-data.frame(germancmp$date,germancmp$partyname,germancmp$rile)
names(germancmp)<-c("yr","party","cmp")
germancmp$yr<-as.factor(substr(germancmp$yr,1,4))
#rename party labels to merge in
germancmp$party<-substr(as.character(germancmp$party),1,3)
germancmp$party
germancmp$party[germancmp$party=="Gre"]<-"GRU"
germancmp$party[germancmp$party=="90/"]<-"GRU"
#merge in cmp
germandat$cmp<-merge(germandat,germancmp,by=c("party","yr"),all.x=T)$cmp.y

### VOTERS
germanvot<-read.csv("germanvot.csv",sep=";")
germandat$vot<-merge(germandat,germanvot,by=c("party","yr"),all.x=T)$vot.y

### EXPERTS
germanexp<-read.csv("germanexp.csv",sep=";")
germandat$exp<-merge(germandat,germanexp,by=c("party","yr"),all.x=T)$exp.y
  
### WORDSCORES

# Based on Debus 2008, ref scores are from 1998

## Select those columns from the full word score matrix that we want

ref <- subset(germany_wfms, select = which(substr(colnames(germany_wfm),4,7)==1998))
vir <- subset(germany_wfms, select = which(substr(colnames(germany_wfm),4,7)!=1998))

## get the reference text orders: colnames(ref)
colnames(ref)
# The order that you see here should be the one used for the scores in the classic.wordscores

## First reference text 10, second 12, etc as in here. colnames(ref) output guides you to see in what order do you find the texts:
ws <- classic.wordscores(ref, scores = c(14,17.5,8.8,3.9,9.4))

## EDIT AS NEEDED.

gws<-as.data.frame(predict(ws, newdata=vir))

gws$party<-as.factor(substr(rownames(gws),1,3))
gws$yr<-as.factor(substr(rownames(gws),4,7))

germandat$ws<-merge(germandat,gws,by=c("party","yr"),all.x=T)$Score

germandat$ws.se<-merge(germandat,gws,by=c("party","yr"),all.x=T)$"Std. Err."

#set reference year scores to 0 so ws will run
germandat$ws[germandat$yr==1998]<-0

### CORRELATIONS DATA FRAME
germancor<-as.data.frame(matrix(NA,nrow=length(unique(germandat$yr)),ncol=8))
names(germancor)<-c("yr","cty","wscmp","wsvot","wsexp","wfcmp","wfvot","wfexp")
germancor$yr<-unique(germandat$yr)
germancor$cty<-"GER"

#run correlations within each year
  for (i in 1:nrow(germancor)){
  
    germancor$wscmp[germancor$yr==germancor$yr[i]]<-cor.test(germandat$ws[germandat$yr==germancor$yr[i]],germandat$cmp[germandat$yr==germancor$yr[i]],method="s")$estimate
  
    germancor$wsvot[germancor$yr==germancor$yr[i]]<-cor.test(germandat$ws[germandat$yr==germancor$yr[i]],germandat$vot[germandat$yr==germancor$yr[i]],method="s")$estimate
  
    germancor$wsexp[germancor$yr==germancor$yr[i]]<-cor.test(germandat$ws[germandat$yr==germancor$yr[i]],germandat$exp[germandat$yr==germancor$yr[i]],method="s")$estimate
  
    germancor$wfcmp[germancor$yr==germancor$yr[i]]<-cor.test(germandat$wf[germandat$yr==germancor$yr[i]],germandat$cmp[germandat$yr==germancor$yr[i]],method="s")$estimate
  
    germancor$wfvot[germancor$yr==germancor$yr[i]]<-cor.test(germandat$wf[germandat$yr==germancor$yr[i]],germandat$vot[germandat$yr==germancor$yr[i]],method="s")$estimate
  
    germancor$wfexp[germancor$yr==germancor$yr[i]]<-cor.test(germandat$wf[germandat$yr==germancor$yr[i]],germandat$exp[germandat$yr==germancor$yr[i]],method="s")$estimate
  }

### CLEANUP
#rm(cmp,germancmp,germanexp,germanvot,gwf,i,gws,ref,vir,ws)

germandat[order(germandat$yr),]

### PLOTTING

#reshape data to long format first
require(reshape2)
germancor.plot<-melt(germancor,id.vars = c("yr", "cty"))
germancor.plot$method<-c(rep("Wordscores",nrow(germancor.plot)/2),rep("Wordfish",nrow(germancor.plot)/2))
germancor.plot$benchmark<-as.factor(substr(germancor.plot$variable,3,5))
require(car)
germancor.plot$benchmark<-recode(germancor.plot$benchmark,"'cmp'='CMP';'exp'='Experts';'vot'='Voters'")
table(germancor.plot$benchmark)

#the sign is flipped for Wordfish, so we need to fix that
germancor.plot$value[germancor.plot$method=="Wordfish"]<-germancor.plot$value[germancor.plot$method=="Wordfish"]*-1

#data frame for avg correlations
germancor.avgs<-as.data.frame(aggregate(germancor.plot$value,by=list(germancor.plot$method,germancor.plot$benchmark),FUN=mean,na.rm=T))

names(germancor.avgs)<-c("method","benchmark","avg")  

write.csv(germancor.avgs,file=paste("germanavgs.csv",sep=""))

#plot across years
pdf(file="germancors.pdf",height=9)
ggplot(germancor.plot,aes(x=value,y=yr)) +
  geom_point() +
  geom_vline(xintercept = 0,linetype=2) +
  geom_vline(aes(xintercept = avg), germancor.avgs) +
  theme_bw() +
  facet_grid(benchmark~method) +
  xlab("Rank order correlation") +
  ylab("Year")
dev.off()


################################################################
# LACONIC GERMANY
################################################################

#summary stats for describing data
lgermansumstats<-as.data.frame(matrix(NA,nrow=1,ncol=5))
names(lgermansumstats)<-c("cty","eyrs","ppyr","lenmean","lensd")
lgermansumstats$cty<-"Germany"
lgermansumstats$eyrs<-9
lgermansumstats$ppyr<-ncol(lgermany_wfms)/germansumstats$eyrs
lgermansumstats$lenmean<-mean(as.numeric(colSums(lgermany_wfms)))
lgermansumstats$lensd<-sd(as.numeric(colSums(lgermany_wfms)))

#we only use the stemmed wfm from here on

### ESTIMATES DATA FRAME
lgermandat<-as.data.frame(matrix(NA,ncol=9,nrow=ncol(lgermany_wfms)))
names(lgermandat)<-c("party","yr","ws","ws.se","wf","wf.se","cmp","exp","vot")

### Wordfish
lgwf <- wordfish(lgermany_wfms)
summary(lgwf)
lgermandat$wf<-lgwf$theta
lgermandat$wf.se<-lgwf$se.theta
lgermandat$party<-substr(lgwf$docs,1,3)
lgermandat$yr<-substr(lgwf$docs,4,7)

### CMP
cmp<-read.csv("cmp.csv")
lgermancmp<-cmp[cmp$countryname=="Germany",]
lgermancmp<-data.frame(lgermancmp$date,lgermancmp$partyname,lgermancmp$rile)
names(lgermancmp)<-c("yr","party","cmp")
lgermancmp$yr<-as.factor(substr(lgermancmp$yr,1,4))
#rename party labels to merge in
lgermancmp$party<-substr(as.character(lgermancmp$party),1,3)
lgermancmp$party
lgermancmp$party[lgermancmp$party=="Gre"]<-"GRU"
lgermancmp$party[lgermancmp$party=="90/"]<-"GRU"
#merge in cmp
lgermandat$cmp<-merge(lgermandat,lgermancmp,by=c("party","yr"),all.x=T)$cmp.y

### VOTERS
lgermanvot<-read.csv("germanvot.csv",sep=";")
lgermandat$vot<-merge(lgermandat,lgermanvot,by=c("party","yr"),all.x=T)$vot.y

### EXPERTS
lgermanexp<-read.csv("germanexp.csv",sep=";")
lgermandat$exp<-merge(lgermandat,lgermanexp,by=c("party","yr"),all.x=T)$exp.y

### WORDSCORES

# Based on Debus 2008, ref scores are from 1998

## Select those columns from the full word score matrix that we want

ref <- subset(lgermany_wfms, select = which(substr(colnames(lgermany_wfm),4,7)==1998))
vir <- subset(lgermany_wfms, select = which(substr(colnames(lgermany_wfm),4,7)!=1998))

## get the reference text orders: colnames(ref)
colnames(ref)
# The order that you see here should be the one used for the scores in the classic.wordscores

## First reference text 10, second 12, etc as in here. colnames(ref) output guides you to see in what order do you find the texts:
ws <- classic.wordscores(ref, scores = c(14,17.5,8.8,3.9,9.4))

## EDIT AS NEEDED.

lgws<-as.data.frame(predict(ws, newdata=vir))

lgws$party<-as.factor(substr(rownames(lgws),1,3))
lgws$yr<-as.factor(substr(rownames(lgws),4,7))

lgermandat$ws<-merge(lgermandat,lgws,by=c("party","yr"),all.x=T)$Score

lgermandat$ws.se<-merge(lgermandat,lgws,by=c("party","yr"),all.x=T)$"Std. Err."

#set reference year scores to 0 so ws will run
lgermandat$ws[germandat$yr==1998]<-0

### CORRELATIONS DATA FRAME
lgermancor<-as.data.frame(matrix(NA,nrow=length(unique(lgermandat$yr)),ncol=8))
names(lgermancor)<-c("yr","cty","wscmp","wsvot","wsexp","wfcmp","wfvot","wfexp")
lgermancor$yr<-unique(lgermandat$yr)
lgermancor$cty<-"GER"

#run correlations within each year
for (i in 1:nrow(lgermancor)){
  
  lgermancor$wscmp[lgermancor$yr==lgermancor$yr[i]]<-cor.test(lgermandat$ws[lgermandat$yr==lgermancor$yr[i]],lgermandat$cmp[lgermandat$yr==lgermancor$yr[i]],method="s")$estimate
  
  lgermancor$wsvot[lgermancor$yr==lgermancor$yr[i]]<-cor.test(lgermandat$ws[lgermandat$yr==lgermancor$yr[i]],lgermandat$vot[lgermandat$yr==lgermancor$yr[i]],method="s")$estimate
  
  lgermancor$wsexp[lgermancor$yr==lgermancor$yr[i]]<-cor.test(lgermandat$ws[lgermandat$yr==lgermancor$yr[i]],lgermandat$exp[lgermandat$yr==lgermancor$yr[i]],method="s")$estimate
  
  lgermancor$wfcmp[lgermancor$yr==lgermancor$yr[i]]<-cor.test(lgermandat$wf[lgermandat$yr==lgermancor$yr[i]],lgermandat$cmp[lgermandat$yr==lgermancor$yr[i]],method="s")$estimate
  
  lgermancor$wfvot[lgermancor$yr==lgermancor$yr[i]]<-cor.test(lgermandat$wf[lgermandat$yr==lgermancor$yr[i]],lgermandat$vot[lgermandat$yr==lgermancor$yr[i]],method="s")$estimate
  
  lgermancor$wfexp[lgermancor$yr==lgermancor$yr[i]]<-cor.test(lgermandat$wf[lgermandat$yr==lgermancor$yr[i]],lgermandat$exp[lgermandat$yr==lgermancor$yr[i]],method="s")$estimate
}

### CLEANUP
#rm(cmp,germancmp,germanexp,germanvot,gwf,i,gws,ref,vir,ws)

lgermandat[order(lgermandat$yr),]

### PLOTTING

#reshape data to long format first
require(reshape2)
lgermancor.plot<-melt(lgermancor,id.vars = c("yr", "cty"))
lgermancor.plot$method<-c(rep("Wordscores",nrow(lgermancor.plot)/2),rep("Wordfish",nrow(lgermancor.plot)/2))
lgermancor.plot$benchmark<-as.factor(substr(lgermancor.plot$variable,3,5))
require(car)
lgermancor.plot$benchmark<-recode(lgermancor.plot$benchmark,"'cmp'='CMP';'exp'='Experts';'vot'='Voters'")
table(lgermancor.plot$benchmark)

#data frame for avg correlations
lgermancor.avgs<-as.data.frame(aggregate(lgermancor.plot$value,by=list(lgermancor.plot$method,lgermancor.plot$benchmark),FUN=mean,na.rm=T))

names(lgermancor.avgs)<-c("method","benchmark","avg")  

write.csv(lgermancor.avgs,file=paste("lgermanavgs.csv",sep=""))

#plot across years
pdf(file="lgermancors.pdf",height=9)
ggplot(lgermancor.plot,aes(x=value,y=yr)) +
  geom_point() +
  geom_vline(xintercept = 0,linetype=2) +
  geom_vline(aes(xintercept = avg), lgermancor.avgs) +
  theme_bw() +
  facet_grid(benchmark~method) +
  xlab("Rank order correlation") +
  ylab("Year")
dev.off()




################################################################
# DENMARK
################################################################

### Documents Corpus

pol.text.corp <- Corpus(DirSource("Denmark", encoding = "UTF-8"),
                        readerControl = list(language = "da")) # Disregard error messageget

pol.text.corp <- tm_map(pol.text.corp, 
                        function(x) iconv(enc2utf8(x), sub = "byte"))  
pol.text.corp <- tm_map(pol.text.corp, stripWhitespace)
pol.text.corp <- tm_map(pol.text.corp, removePunctuation)
pol.text.corp <- tm_map(pol.text.corp, removeNumbers)

pol.text.corp <- tm_map(pol.text.corp, tolower)
pol.text.corp <- tm_map(pol.text.corp, function(x) removeWords(x,  stopwords("da")))

#stemming takes a while - log time to see how long exactly
t1<-proc.time()
pol.text.corp.stemmed <- tm_map(pol.text.corp, stemDocument,language="danish") #this takes ages
t2<-proc.time()
t2-t1

#create 4 word frequency matrices, 2x2 factorial for for stemmed x reduced corpora
denmark_wfm<-as.wfm(TermDocumentMatrix(pol.text.corp))
denmark_wfms<-as.wfm(TermDocumentMatrix(pol.text.corp.stemmed))

#summary stats for describing data
danishsumstats<-as.data.frame(matrix(NA,nrow=1,ncol=5))
names(danishsumstats)<-c("cty","eyrs","ppyr","lenmean","lensd")
danishsumstats$cty<-"Denmark"
danishsumstats$eyrs<-26
danishsumstats$ppyr<-ncol(denmark_wfm)/danishsumstats$eyrs
danishsumstats$lenmean<-mean(as.numeric(colSums(denmark_wfm)))
danishsumstats$lensd<-sd(as.numeric(colSums(denmark_wfm)))

### ESTIMATES DATA FRAME
danishdat<-as.data.frame(matrix(NA,ncol=9,nrow=ncol(denmark_wfms)))
names(danishdat)<-c("party","yr","ws","ws.se","wf","wf.se","cmp","exp","vot")

# First, find out how I need to rename the party names 
ests<-read.dta("danishests.dta")
names(table(toupper(ests$party)))

### Wordfish
dwf <- wordfish(denmark_wfms)
summary(dwf)
danishdat$wf<-dwf$theta
danishdat$wf.se<-dwf$se.theta
danishdat$yr<-as.numeric(gsub("[^0-9]","",dwf$docs))

#clean party names
danishdat$party<-gsub("[0-9]","",dwf$docs)
danishdat$party<-gsub("\\.txt","",danishdat$party)
danishdat$party<-toupper(danishdat$party)
danishdat$party<-gsub("KRA","SD",danishdat$party)
danishdat$party<-gsub("HIP","SF",danishdat$party)
danishdat$party<-gsub("ENH","EN",danishdat$party)

#clean up danish cmp, voters, experts estimates data
ests$yr<-as.numeric(substr(ests$year,1,4))
ests$party<-toupper(ests$party)
str(ests)

### Merge in Voters, Experts, CMP, Roll Call Estimates
mergedat<-merge(danishdat,ests,by=c("party","yr"))
danishdat<-as.data.frame(matrix(NA,ncol=9,nrow=nrow(mergedat)))
names(danishdat)<-c("party","yr","ws","ws.se","wf","wf.se","cmp","exp","vot")
danishdat$party<-mergedat$party
danishdat$yr<-mergedat$yr
danishdat$wf<-mergedat$wf
danishdat$wf.se<-mergedat$wf.se
danishdat$cmp<-mergedat$rilemean
danishdat$exp<-mergedat$dam
danishdat$vot<-mergedat$eurobar

### WORDSCORES

# Based on Klemmensen et al, ref scores are from 1947 and 1973

dktexts<-c(list.files("Denmark"))

intendRef<-dktexts[gsub("[^0-9]","",dktexts) %in% c(1947,1975)]

# Virgin texts are all other years

intendVir<-dktexts[!gsub("[^0-9]","",dktexts) %in% c(1947,1975)]

## Select those columns from the full word score matrix that we want

ref <- subset(denmark_wfms, select = which(colnames(denmark_wfms)  %in% intendRef))
vir <- subset(denmark_wfms, select = which(colnames(denmark_wfms)  %in% intendVir))

## get the reference text orders: colnames(ref)

colnames(ref)
# The order that you see here should be the one used for the scores in the classic.wordscores

## First reference text 10, second 12, etc as in here. colnames(ref) output guides you to see in what order do you find the texts:

ws <- classic.wordscores(ref, scores = c(6,0,0,7,7.5,2,8,9,3,3,7,5,5,10,8,1,10))

## EDIT AS NEEDED.

dws<-as.data.frame(predict(ws, newdata=vir))

dws$yr<-as.numeric(gsub("[^0-9]","",rownames(dws)))

#clean party names
dws$party<-gsub("[0-9]","",rownames(dws))
dws$party<-gsub("\\.txt","",dws$party)
dws$party<-toupper(dws$party)
dws$party<-gsub("KRA","SD",dws$party)
dws$party<-gsub("HIP","SF",dws$party)
dws$party<-gsub("ENH","EN",dws$party)

danishdat$ws<-merge(danishdat,dws,by=c("party","yr"),all.x=T)$Score

danishdat$ws.se<-merge(danishdat,dws,by=c("party","yr"),all.x=T)$"Std. Err."


### CORRELATIONS DATA FRAME

danishcor<-as.data.frame(matrix(NA,nrow=length(unique(danishdat$yr)),ncol=8))
names(danishcor)<-c("yr","cty","wscmp","wsvot","wsexp","wfcmp","wfvot","wfexp")
danishcor$yr<-unique(danishdat$yr)
danishcor$cty<-"DEN"
danishcor<-danishcor[order(danishcor$yr),]
danishdat<-danishdat[order(danishdat$yr),]

#ignore 1947 and 1975 NA's -- for now
danishdat$ws[danishdat$yr %in% c(1947,1975)]<-0

#run correlations within each year
  for (i in 1:nrow(danishcor)){
    if (sum(danishdat$cmp[danishdat$yr==danishcor$yr[i]],na.rm=T)!=0){
      danishcor$wfcmp[i]<-cor.test(danishdat$wf[danishdat$yr==danishcor$yr[i]],danishdat$cmp[danishdat$yr==danishcor$yr[i]],method="s",na.action=na.omit)$estimate
      danishcor$wscmp[i]<-cor.test(danishdat$ws[danishdat$yr==danishcor$yr[i]],danishdat$cmp[danishdat$yr==danishcor$yr[i]],method="s",na.action=na.omit)$estimate
    }

    if (sum(danishdat$vot[danishdat$yr==danishcor$yr[i]],na.rm=T)!=0){
      danishcor$wfvot[i]<-cor.test(danishdat$wf[danishdat$yr==danishcor$yr[i]],danishdat$vot[danishdat$yr==danishcor$yr[i]],method="s",na.action=na.omit)$estimate
      danishcor$wsvot[i]<-cor.test(danishdat$ws[danishdat$yr==danishcor$yr[i]],danishdat$vot[danishdat$yr==danishcor$yr[i]],method="s",na.action=na.omit)$estimate
    }
  
    if (sum(danishdat$exp[danishdat$yr==danishcor$yr[i]],na.rm=T)!=0){
      danishcor$wfexp[i]<-cor.test(danishdat$wf[danishdat$yr==danishcor$yr[i]],danishdat$exp[danishdat$yr==danishcor$yr[i]],method="s",na.action=na.omit)$estimate
      danishcor$wsexp[i]<-cor.test(danishdat$ws[danishdat$yr==danishcor$yr[i]],danishdat$exp[danishdat$yr==danishcor$yr[i]],method="s",na.action=na.omit)$estimate
    } 
  }

### CLEANUP
rm(ests,mergedat,i,dwf,pol.text.corp)

### PLOTTING

#reshape data to long format first
require(reshape2)
danishcor.plot<-melt(danishcor,id.vars = c("yr", "cty"))
danishcor.plot$method<-c(rep("Wordscores",nrow(danishcor.plot)/2),rep("Wordfish",nrow(danishcor.plot)/2))
danishcor.plot$benchmark<-as.factor(substr(danishcor.plot$variable,3,5))
require(car)
danishcor.plot$benchmark<-recode(danishcor.plot$benchmark,"'cmp'='CMP';'exp'='Experts';'vot'='Voters'")
table(danishcor.plot$benchmark)

#data frame for avg correlations
danishcor.avgs<-as.data.frame(aggregate(danishcor.plot$value,by=list(danishcor.plot$method,danishcor.plot$benchmark),FUN=mean,na.rm=T))
danishcor.avgs
names(danishcor.avgs)<-c("method","benchmark","avg")  

write.csv(danishcor.avgs,file=paste("danishavgs.csv",sep=""))

#plot across years
pdf(file=paste("danishcors.pdf",sep=""),height=9)
ggplot(danishcor.plot,aes(x=value,y=yr)) +
  geom_point() +
  geom_vline(xintercept = 0,linetype=2) +
  geom_vline(aes(xintercept = avg), danishcor.avgs) +
  theme_bw() +
  facet_grid(benchmark~method) +
  xlab("Rank order correlation") +
  ylab("Year")
dev.off()





################################################################
# DENMARK AND GERMANY AND LACONIC GERMANY COMBINED
################################################################

allcor<-as.data.frame(rbind(germancor,lgermancor,danishcor))

#fix reversed sign for germany - not sure why this happened
allcor[1:9,6:8]<-allcor[1:9,6:8]*-1

#country name for laconic germany
allcor$cty[10:18]<-"LGER"

#melt
allcor.plot<-melt(allcor,id.vars = c("yr", "cty"))

allcor.plot$method<-c(rep("Wordscores",nrow(allcor.plot)/2),rep("Wordfish",nrow(allcor.plot)/2))
allcor.plot$benchmark<-as.factor(substr(allcor.plot$variable,3,5))

allcor.avgs<-as.data.frame(aggregate(allcor.plot$value,by=list(allcor.plot$cty,allcor.plot$benchmark,allcor.plot$method),FUN=mean,na.rm=T))
names(allcor.avgs)<-c("cty","benchmark","method","avg")  

#plot across years
ggplot(allcor.plot,aes(x=value,y=yr)) +
  geom_point() +
  geom_vline(xintercept = 0,linetype=2) +
  geom_vline(aes(xintercept = avg), allcor.avgs) +
  theme_bw() +
  facet_grid(cty~method) +
  xlab("Rank order correlation") +
  ylab("Year")


#scatter plot of standardized estimates against expert placements
danishdat$ws[danishdat$yr %in% c(1947,1975)]<-NA
germandat$ws[germandat$yr==1998]<-NA

#add country names
danishdat$cty<-"Denmark"
germandat$cty<-"Germany"

alldat<-as.data.frame(rbind(germandat,danishdat))

#standardize exp, cmp,vot,wf,ws within each country
require(plyr)

alldat2<-ddply(alldat,c("cty","yr"),transform,zcmp=scale(cmp),zvot=scale(vot),zexp=scale(exp),zws=scale(ws))

#fix sign flipped for german WF
alldat2$wf[alldat2$cty=="Germany"]<-alldat2$wf[alldat2$cty=="Germany"]*-1

#get data ready for plotting
alldat.biplot<-data.frame(exp=rep(alldat2$zexp,4),cty=rep(alldat2$cty,4),corvar=c(alldat2$zcmp,alldat2$zvot,alldat2$wf,alldat2$zws),benchmark=c(rep("CMP",nrow(alldat)),rep("Voters",nrow(alldat)),rep("Wordfish",nrow(alldat)),rep("Wordscores",nrow(alldat))))


                                                  
pdf(file="bivarplot.pdf",width=10)
ggplot(alldat.biplot,aes(x=exp,y=corvar)) +
  geom_jitter(alpha=.5) +
  theme_bw() +
#  stat_smooth(method="lm",n=6) +
  facet_grid(cty~benchmark) +
  xlab("Standardized expert placement") +
  ylab("Standardized measures")
dev.off()

#summary stats table
allsumstats<-as.data.frame(rbind(germansumstats,lgermansumstats,danishsumstats))
allsumstats
?stargazer

#table with only germany and denmark
stargazer(data.frame(var=c("Election years","Avg manifestos per year","Avg menifesto length","Std.dev. manifesto lengths"),germany=unlist(allsumstats[1,2:5]),denmark=unlist(allsumstats[3,2:5])),summary=F,digits=1,align=F,covariate.labels=c("","Germany","Denmark"),title="Summary stats for German \& Danish manifesto data",label="sumstats")

#table with laconic germany included
stargazer(data.frame(var=c("Election years","Avg manifestos per year","Avg menifesto length","Std.dev. manifesto lengths"),germany=unlist(allsumstats[1,2:5]),lgermany=unlist(allsumstats[2,2:5]),denmark=unlist(allsumstats[3,2:5])),summary=F,digits=1,align=F,covariate.labels=c("","Germany","Lac. Germany","Denmark"),title="Summary stats for German & Danish manifesto data, Laconic Germany incluced",label="sumstats2")

#chances of correct ordering given number of parties
factorial(4)^-1
factorial(8)^-1

#how many words in vir and reftexts?
virrefsum<-data.frame(var=rep(NA,3),Denmark=rep(NA,3),Germany=rep(NA,3))
virrefsum$var<-c("Reference texts","Avg. word length, reference texts","Avg. word length, virgin texts")
virrefsum$Denmark[1]<-"1947, 1975 elections"
virrefsum$Germany[1]<-"1998 election"

danishwordcounts<-data.frame(text=colnames(denmark_wfm),length=colSums(denmark_wfm))
danishwordcounts$yr<-as.numeric(gsub("[^0-9]","",danishwordcounts$text))
virrefsum$Denmark[3]<-mean(danishwordcounts[!(danishwordcounts$yr %in% c(1947,1975)),]$length)
virrefsum$Denmark[2]<-mean(danishwordcounts[(danishwordcounts$yr %in% c(1947,1975)),]$length)

germanwordcounts<-data.frame(text=colnames(germany_wfm),length=colSums(germany_wfm))
germanwordcounts$yr<-as.numeric(gsub("[^0-9]","",germanwordcounts$text))
virrefsum$Germany[3]<-mean(germanwordcounts[!(germanwordcounts$yr %in% c(1998)),]$length)
virrefsum$Germany[2]<-mean(germanwordcounts[(germanwordcounts$yr %in% c(1998)),]$length,na.rm=T)

stargazer(virrefsum,summary=F,digits=1,title="Summary stats for reference and virgin texts",label="virrefsum")

#average correlations?
allcor.avgs<-allcor.avgs[order(allcor.avgs$cty,allcor.avgs$method),c(1,3,2,4)]
allcor.avgs$benchmark<-rep(c("CMP","Experts","Voters"),6)
#allcor.avgs$avg[7:9]<-allcor.avgs$avg[7:9]*-1
names(allcor.avgs)<-c("Country","Method","Benchmark","Avg. rho")

allcor

stargazer(allcor.avgs,summary=F,digits=1,title="Average rank order correlations with benchmark measures, Wordscores and Wordfish",label="allcoravgs")


