program <- read.csv("~/Downloads/smpPro.csv")
summary(program)
table(program$attendJ2,program$programJ2)
J2ProgramNotAttend = subset(subset(program, programJ2 == "Y"),attendJ2 == "-")
coreProgram = subset(subset(program, programJ2 == "Y"),attendJ2 == "Y")
sum(coreProgram$totalParticipant)
sum(coreProgram$totalClass)
table(coreProgram$devCode, coreProgram$totalClass)
table(coreProgram$devCode, coreProgram$totalParticipant)
coreProgram[coreProgram$totalParticipant =="4481",]
coreProgram[coreProgram$totalClass == "75",]
coreProgram$devCode = as.factor(coreProgram$devCode)
core1  = subset(coreProgram, devCode =="1")
core2  = subset(coreProgram, devCode =="2")
core3  = subset(coreProgram, devCode =="3")
core4  = subset(coreProgram, devCode =="4")
core5  = subset(coreProgram, devCode =="5")
core6  = subset(coreProgram, devCode =="6")
core8  = subset(coreProgram, devCode =="8")
core9  = subset(coreProgram, devCode =="9")



sum(coreProgram$totalClass)
sum(core1$totalClass)# 779
sum(core2$totalClass)#536
sum(core3$totalClass)#1029
sum(core4$totalClass)#650
779+536+1029+650 # 2994 #Core sessions
5461-2994 #2467 Interest Class

sum(core1$totalParticipant)# 7597
sum(core2$totalParticipant)# 11037
sum(core3$totalParticipant)# 16708
sum(core4$totalParticipant)# 6521
7597+11037+16708+6521 # 41863

table(core1$format,core1$totalClass)
core1[core1$totalClass == "29",]

table(core1$actCode,core1$totalClass)

#counting ASC sessions
subset(core1, grepl("^ASC",core1$actCode)) #17
subset(core2, grepl("^ASC",core2$actCode)) #23+25+24+24+24+20+24 +2+9= 175
23+25+24+24+24+20+24+2+9
subset(core3, grepl("^ASC",core3$actCode)) # 24+26 = 50
subset(core4, grepl("^ASC",core4$actCode)) # 0 

##Session analysis

table(core1$totalClass)
core1[core1$totalClass =="8",] #Core 1 all good
sum(core1$totalClass)#779 sessions


table(core2$totalClass)
core2[core2$totalClass >20,] 
# 75+44(S14ASV001), 25 ,(ASC140501) 24*4(ASC 0601,0901,1001,1201) ,23(ASC 0401),  20(ASC1101), 9(ASC0701) = 290
75+44+23+24*4+23+20+9
sum(core2$totalClass) #536-290 = 246
536-290

table(core3$totalClass)
core3[core3$totalClass >9,] ### 34,16,25 (INNO A14DCA101,A14DCH101, A14DCY101 ), 26,24 (ASC140301 )  = 125
sum(core3$totalClass) -(34+16+25+26+24) #904

table(core4$totalClass)
core4[core4$totalClass >7,] ###37 (A14DCZ021 INNO), 
sum(core2$totalClass) -37 #499


290+125+37 #452 Out of 2994
452/2994 #15%

###END session analysis

##Participants analysis

table(core1$totalClass ==1, core1$totalParticipant>50)
core1[core1$totalParticipant>50,]

table(core2$totalClass ==1, core2$totalParticipant>99)###380+380+400+100+380+380+380+400+80+55+60+80 = 3075
core2[core2$totalParticipant>40,]
380+380+400+100+380+380+380+400+80+55+60+80


table(core3$totalClass ==1, core3$totalParticipant>50) ## 80+91+74+60+400+115 = 820
core3[core3$totalParticipant>50,]
80+91+74+60+400+115


table(core4$totalClass ==1, core4$totalParticipant>50)
core4[core4$totalParticipant>50,] ##125 + 840 =965
125 + 840 


(965+820+3075)/41863 #4860    11.6%

core <- rbind(core1, core2,core3,core4)
core$participantPerClass = core$totalParticipant/core$totalClass
table(core$participantPerClass)
plot(core$participantPerClass )
library(ggplot2)
ggplot(data = core, aes(totalParticipant,y= totalClass)) +geom_dotplot()
quantile(core$totalParticipant,probs =c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type = 5) 
quantile(core$totalClass,probs =c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type =5 ) 

core[core$totalParticipant ==4481  ,]
help(quantile)
quantile(core$totalParticipant, type = 3)
quantile(core$totalClass, type = 9)

 quantile(core$totalParticipant,probs =c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type = 5)
# 0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
# 1.0    5.0    7.0    9.0   10.3   13.0   17.0   23.4   33.0   56.0 4481.0 
quantile(core$totalClass,probs =c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type =5 )
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 1    1    1    1    2    2    2    3    4    5   75 
quantile(core$participantPerClass,probs =seq(0, 1, length = 11),type =5)
# 0%        10%        20%        30%        40%        50%        60%        70%        80%        90%       100% 
# 1.000000   3.033333   4.000000   5.000000   6.000000   7.000000   8.000000  10.000000  12.000000  19.600000 840.000000 


quantile(core$participantPerClass,probs =c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type = 5)
quantile(core$totalParticipant,probs =c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type =5 )
quantile(core$totalParticipant,probs =seq(0.9, 1, length = 11),type =5)
quantile(core$totalParticipant,probs =seq(0, 1, length = 101),type =5)

table(core$totalParticipant)
table(core$totalClass)

################################################
library(lubridate)

program <- read.csv("~/Downloads/smpPro.csv")
summary(program)

program$act_core <- as.factor(program$act_core)
program$act_sdate <- as.Date(program$act_sdate, "%d/%m/%Y")
program$act_edate <- as.Date(program$act_edate, "%d/%m/%Y")
program$ppl_age <- as.integer(program$ppl_age)
program$ppl_wkstatus <- as.factor(program$ppl_wkstatus)
program$mem_id <- as.factor(program$mem_id)
program$act_mainpt <- as.factor(program$act_mainpt)
program$act_target <- as.factor(program$act_target)
program$ppl_martial <- as.factor(program$ppl_martial)
program$year <- year(program$act_sdate)
program$month <- month(program$act_sdate)
head(program)



str(program)
table(program$act_core)
programCore <- subset(program, 
                      act_core == 5001 | act_core == 5002 | act_core == 5003 |act_core == 5004 )
programNonCore <- subset(program, 
  act_core == 5005 | act_core == 5006 | act_core == 5007 | act_core == 5008 | act_core == 5009)

ggplot(program, aes(x=act_sdate,fill=act_core)) +
  geom_histogram( position="dodge") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m")


ggplot(programCore, aes(x=act_sdate,fill=act_core)) +
  geom_histogram(binwidth = 30) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m")

ggplot(programCore, aes(x=act_sdate)) +
  geom_histogram( position="dodge") +
  scale_x_date(date_breaks = "1 year", date_labels = "%m")

freqs <- aggregate(program$act_sdate, by=list(program$act_sdate), FUN=length)
freqs$names <- as.Date(freqs$Group.1,format="%Y-%m")
freqs$Month <- as.Date(cut(freqs$Group.1,
                         breaks = "month"))

ggplot(freqs, aes(x=Month)) + geom_histogram() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m")
  ylab("Frequency") + xlab("Year and Month") +
  theme_bw()

  
library(dplyr)
library(plotly)  

program %>%
  filter(act_core == "5001") %>%
  group_by(act_code) %>%
  summarise(
    Number = n()
  ) %>%
  arrange(desc(Number))

program %>%
  group_by(act_code) %>%
  summarise(
    Number = n()
    
  ) %>%
  arrange(desc(Number))

df <-  program %>%
  group_by(distance) %>%
   summarise(
     Num = n()
   )%>%
  mutate(dist = exp(-distance))
df <-  program %>%
  group_by(distance) %>%
  summarise(
    Num = n()
  )
df <- df[-c(64),]

plot_ly(df, y = ~Num, x= ~distance, type = "scatter") %>%
  add_lines( y= ~fitted(lm(Num ~ distance)))

plot_ly(df, y = ~Num, x= ~dist, type = "scatter") %>%
  add_lines( y= ~fitted(lm(Num ~ dist)))
library(broom)
m <- loess(Num ~ dist, data = df)

           
plot_ly(df, x = ~dist, color = I("black")) %>%
  add_markers(y = ~Num, showlegend = FALSE) %>%
  add_lines(y = ~fitted(loess(Num ~ dist)),
            line = list(color = '#07A4B5'))  %>%
  add_ribbons(data = augment(m),
                             ymin = ~.fitted - 1.96 * .se.fit,
                             ymax = ~.fitted + 1.96 * .se.fit,
                       line = list(color = 'rgba(7, 164, 181, 0.05)'),
                       fillcolor = 'rgba(7, 164, 181, 0.2)') %>%
                layout(xaxis = list(title = 'Distance from centre'),
                       yaxis = list(title = 'Number of members') )
         



df <-  program %>%
  group_by(mem_id) %>%
  summarise(
    Num = n()
  ) %>% 
  group_by(Num) %>%
  summarise(
    Freq  = n()
  )

plot_ly(df, y = ~Freq, x= ~Num, type = "bar")

plot_ly(program, y = ~mem_id, x = ~act_sdate, type = "scatter", color = act_core)

df17 <- program %>%
  filter(act_sdate > "2017-01-01" & act_sdate < "2018-01-01") %>%
  group_by(mem_id) %>%
  summarise(
    Num = n()
  )
df16 <- program %>%
  filter(act_sdate > "2016-01-01" & act_sdate < "2017-01-01") %>%
  group_by(mem_id) %>%
  summarise(
    Num = n()
  )
program %>%
  filter(act_sdate > "2016-01-01" & act_sdate < "2017-01-01") %>%
  group_by(act_core) %>%
  summarise(
    Num = n()
  )

program %>%
  filter(act_sdate > "2017-01-01" & act_sdate < "2018-01-01") %>%
  group_by(act_core) %>%
  summarise(
    Num = n()
  )

program %>%
  filter(act_sdate > "2016-01-01" & act_sdate < "2017-01-01") %>%
  group_by(act_code) %>%
  summarise(
    Num = n()
  )


program %>%
  filter(act_sdate > "2017-01-01" & act_sdate < "2018-01-01") %>%
  group_by(act_code) %>%
  summarise(
    Num = n()
  )












































#member as id  
#member merge with program for maker project.
library(reshape2)
<<<<<<< HEAD:program2014.R
head(member)
summary(program)
#member <- reshape(program, idvar="mem_id", timevar = "year", direction = "wide")
#member <-dcast(program, mem_id year + month ~ variable)
# member <- dcast(program, mem_id + joint_days + ppl_id + ppl_age + ppl_edu + ppl_wkstatus +
#         ppl_martial + center_id + mem_cssa + active_mem + active_mem + days_to_expiry +
#         distance ~ year + month  , fun.aggregate = sum, 
#       na.rm = TRUE)
program$yearMonth <- as.factor(paste(as.character(program$year),as.character(program$month)))

member <- dcast(program, mem_id ~ yearMonth + act_core )
unique(member$mem_id)
subset(program, mem_id == "333208800")
table(is.na(member$mem_id) == TRUE)

library(rpart)
frmla = mem_id ~ .
fit = rpart(frmla, method="class", data=member)


printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# tabulate some of the data
table(subset(raw, Koc>=190.5)$Metal)

library(tree)

tr = tree(frmla, data=member)
summary(tr)
plot(tr); text(tr)

###############
## OBLIQUE.TREE
library(oblique.tree)

aug.crabs.data = data.frame( g=factor(rep(1:4,each=50)),
                             predict(princomp(crabs[,4:8]))[,2:3])
plot(aug.crabs.data[,-1],type="n")
text( aug.crabs.data[,-1], col=as.numeric(aug.crabs.data[,1]), labels=as.numeric(aug.crabs.data[,1]))
ob.tree = oblique.tree(formula = g~.,
                       data = aug.crabs.data,
                       oblique.splits = "only")
plot(ob.tree);text(ob.tree)





##################
##longRPart
library(longRPart)

data(pbkphData)
pbkphData$Time=as.factor(pbkphData$Time)
long.tree = longRPart(pbkph~Time,~age+gender,~1|Subject,pbkphData,R=corExp(form=~time))
lrpTreePlot(long.tree, use.n=TRE, place="bottomright")













###############
# PARTY package
library(party)

(ct = ctree(frmla, data = member))
plot(ct, main="Conditional Inference Tree")

#Table of prediction errors
table(predict(ct), raw$Metal)

# Estimated class probabilities
tr.pred = predict(ct, newdata=raw, type="prob")

library(cluster) # Needed for silhouette function

kmeansDat <- member[1:410,]  # Extract only customer columns
kmeansDat <- member[1:410,-(1:1)]  # Extract only customer columns
is.na(member$mem_id)
kmeansDat.t <- t(kmeansDat)  # Get customers in rows and products in columns
head(kmeansDat.t)
head(member)
tail(member)
tail(kmeansDat)
# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 2      # Hypothesized minimum number of segments
maxClust <- 10      # Hypothesized maximum number of segments

# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(11) # For reproducibility
  km.out[i] <- list(kmeans(kmeansDat.t, centers = centr, nstart = 50))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(kmeansDat.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k                                                                                                                                                                                                                                                                                                                                                             30455
  y[i] = summary(sil.out[[i]])[[2]]  # Silhouette average width
}
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")
<<<<<<< HEAD

# Get customer names that are in each segment ----------------------------------

# Get attributes of optimal k-means output
maxSilRow <- which.max(y)          # Row number of max silhouette value
optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
      km.out.best$cluster[km.out.best$cluster == clustr]
    )
  )
}
names(clusterList) <- clusterNames

print(clusterList)

# Get attributes of optimal k-means output
#maxSilRow <- which.max(y)          # Row number of max silhouette value
maxSilRow <- 5

optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
      km.out.best$cluster[km.out.best$cluster == clustr]
    )
  )
}
names(clusterList) <- clusterNames

print(clusterList)


  
  


  
  
=======
=======
>>>>>>> e2cb2154da6f29c00045e3041d93b9a8c9f4e170
library(lubridate)
program$year <- year(program$act_sdate)
member$mem_id <- as.factor(member$mem_id)
programM <- subset(program, select = c("mem_id", "year"))
memberpro <- reshape(programM, idvar = "mem_id", timevar = "year", direction = "wide")
memberpro <- dcast(programM, mem_id ~ year)
head(member)
unique(member$mem_id)
table(is.na(memberpro$mem_id) == TRUE)
member <- read.csv("~/Downloads/member_statistic-20171115 - Sheet1.csv")
memberM <- left_join(member, memberpro)

write.csv(memberM,file = "~/Downloads/member_statistic-20171115 - Sheet2.csv")

#
<<<<<<< HEAD

=======
>>>>>>> 8368aad45387ef52970fdee071967b42c8a1e347:program2014.R
>>>>>>> e2cb2154da6f29c00045e3041d93b9a8c9f4e170

