# Airline-ticket-booking-cancellation
Analyzing the reasons for ticket booking and cancellations by passengers that occur in a specific airline.
#Install packages RJSONIO,Rcurl,rjson,stringr
#Converting data in to CSV
#data_lin = getURL("http://ist.gmu.edu/~hpurohit/courses/ait582-proj-data-spring16.json")
#data_j <- fromJSON(data_lin)
data_j <- fromJSON("/Users/jinusebastin/desktop/RZone/dataset.json")
data_p <- do.call(rbind,data_j)
data_p = data.frame(data_p,row.names=NULL)

#delete the first row which has the titles again
data_p = data_p[-c(1),]
fwrite(data_p,"Airlineproj.csv") #use package data.table
#Alternative method
#Loading data from CSV
#data_p = read.csv("Airlineproj.csv")

#rearrange to appear in order
data_p <- data_p[,c("CUSTOMERID","SUCCESS","DESCRIPTION","SEATCLASS","GUESTS","FARE")]

#Metadata extraction for Title, Gender, Age
data_p["TITLE"] <- NA
data_p$TITLE <- str_extract(string =gsub(".*, (.*.) .*;.*","\\1",data_p[,3]),pattern = 
                              "(Mr|Miss|Mrs|Master|Sir|Ms|Lady|Mlle|Mme|the Countess|Jonkheer|Don|Rev|Capt|Col|Major|Dr)\\.")

data_p["GENDER"] <- NA
data_p$GENDER <- ifelse(data_p$TITLE=="Mrs.","Female",ifelse(data_p$TITLE=="Mr.","Male",ifelse(data_p$TITLE=="Miss.","Female",ifelse(data_p$TITLE=="Master.","Male",ifelse(data_p$TITLE=="Don.","Male",ifelse(data_p$TITLE=="Lady.","Female",ifelse(data_p$TITLE=="Jonkheer.","Male",ifelse(data_p$TITLE=="Mme.","Female",ifelse(data_p$TITLE=="Mlle.","Female",ifelse(data_p$TITLE=="Ms.","Female",ifelse(data_p$TITLE=="Sir.","Male",ifelse(data_p$TITLE=="the Countess.","Female",ifelse(data_p$TITLE=="Major.","Male",ifelse(data_p$TITLE=="Capt.","Male",ifelse(data_p$TITLE=="Col.","Male",ifelse(data_p$TITLE=="Rev.","Male",ifelse(data_p$CUSTOMERID==797,"Female",ifelse(data_p$TITLE=="Dr.","Male","NA"))))))))))))))))))

data_p["AGE"] <- NA
data_p$AGE <- as.integer(gsub(".*;(.*)","\\1",data_p[,3]))

#Replace NA with 0 for AGE
data_p$AGE[is.na(data_p$AGE)] <- 0

#histogram of customer age with outliers
hist(data_p$AGE, breaks=45, xlim=c(0,90), col="blue", border="black", ylim=c(0,70),
     xlab="Age", ylab="Counts", main="Histogram of Customer Age")
box(which="plot",lty="solid",col="black")

#List the titles that have missing values
sqldf('select distinct TITLE from data_p where AGE=0')

#Imputing missing AGE with their median
#Title vs Age
sqldf(' select TITLE="Dr.",median(AGE) from data_p where TITLE="Dr." group by TITLE')
sqldf(' select TITLE,   Case when Title = "Dr." and Age = 0 Then (Select median(AGE) from data_p where TITLE="Dr." 
      group by TITLE) Else Age END as ReplacedAge From data_p where Title = "Dr."')
data_p <- within(data_p, AGE[TITLE== "Dr." & AGE == 0] <- 44)

sqldf(' select TITLE="Master.",median(AGE) from data_p where TITLE="Master." group by TITLE')
sqldf(' select TITLE,   Case when Title = "Master." and Age = 0 Then (Select median(AGE) from data_p where TITLE="Master." 
      group by TITLE) Else Age END as ReplacedAge From data_p where Title = "Master."')
data_p <- within(data_p, AGE[TITLE== "Master." & AGE == 0] <- 3)

sqldf(' select TITLE="Miss.",median(AGE) from data_p where TITLE="Miss." group by TITLE')
sqldf(' select TITLE,   Case when Title = "Miss." and Age = 0 Then (Select median(AGE) from data_p where TITLE="Miss." 
      group by TITLE) Else Age END as ReplacedAge From data_p where Title = "Miss."')
data_p <- within(data_p, AGE[TITLE== "Miss." & AGE == 0] <- 18)

sqldf(' select TITLE="Mr.",median(AGE) from data_p where TITLE="Mr." group by TITLE')
sqldf(' select TITLE,   Case when Title = "Mr." and Age = 0 Then (Select median(AGE) from data_p where TITLE="Mr." 
      group by TITLE) Else Age END as ReplacedAge From data_p where Title = "Mr."')
data_p <- within(data_p, AGE[TITLE== "Mr." & AGE == 0] <- 25)

sqldf(' select TITLE="Mrs.",median(AGE) from data_p where TITLE="Mrs." group by TITLE')
sqldf(' select TITLE,   Case when Title = "Mrs." and Age = 0 Then (Select median(AGE) from data_p where TITLE="Mrs." 
      group by TITLE) Else Age END as ReplacedAge From data_p where Title = "Mrs."')
data_p <- within(data_p, AGE[TITLE== "Mrs." & AGE == 0] <- 33)

#Seatclass vs Age
sqldf(' select SEATCLASS="1",median(AGE) from data_p where SEATCLASS="1" group by SEATCLASS')
sqldf(' select SEATCLASS="2",median(AGE) from data_p where SEATCLASS="2" group by SEATCLASS')
sqldf(' select SEATCLASS="3",median(AGE) from data_p where SEATCLASS="3" group by SEATCLASS')

#Imputing missing FARE with their mean
#Seatclass vs Fare
sqldf(' select SEATCLASS="1",avg(FARE) from data_p where SEATCLASS="1" group by SEATCLASS')
sqldf(' select SEATCLASS,   Case when SEATCLASS="1" and FARE = 0 Then (Select avg(FARE) from data_p where SEATCLASS="1" group by SEATCLASS) Else FARE END as ImputedFare From data_p where SEATCLASS="1"')
data_p <- within(data_p, FARE[SEATCLASS==1 & FARE == 0] <- 84.1546)

sqldf(' select SEATCLASS="2",avg(FARE) from data_p where SEATCLASS="2" group by SEATCLASS')
sqldf(' select SEATCLASS,   Case when SEATCLASS="2" and FARE = 0 Then (Select avg(FARE) from data_p where SEATCLASS="2" group by SEATCLASS) Else FARE END as ImputedFare From data_p where SEATCLASS="2"')
data_p <- within(data_p, FARE[SEATCLASS==2 & FARE == 0] <- 20.6621)

sqldf(' select SEATCLASS="3",avg(FARE) from data_p where SEATCLASS="3" group by SEATCLASS')
sqldf(' select SEATCLASS,   Case when SEATCLASS="3" and FARE = 0 Then (Select avg(FARE) from data_p where SEATCLASS="3" group by SEATCLASS) Else FARE END as ImputedFare From data_p where SEATCLASS="3"')
data_p <- within(data_p, FARE[SEATCLASS==3 & FARE == 0] <- 13.6755)

#Guests vs Fare
sqldf(' select GUESTS="0",avg(FARE) from data_p where GUESTS="0" group by GUESTS')
sqldf(' select GUESTS="1",avg(FARE) from data_p where GUESTS="1" group by GUESTS')
sqldf(' select GUESTS="2",avg(FARE) from data_p where GUESTS="2" group by GUESTS')
sqldf(' select GUESTS="3",avg(FARE) from data_p where GUESTS="3" group by GUESTS')

#After imputation
data_q = read.csv("Airprojectcopy.csv")

#Histogram of customer age after imputation
hist(data_q$AGE, breaks=45, xlim=c(0,90), col="blue", border="black", ylim=c(0,160),
     xlab="Age", ylab="Counts", main="Histogram of Customer Age")
box(which="plot",lty="solid",col="black")

#To find mean,median,SD of AGE,FARE
data_q.sum <-data_q %>%
  select(AGE,FARE) %>% # select variables to summarise
  summarise_all(funs(min = min, 
    q25 = quantile(., 0.25), 
    median = median, 
    q75 = quantile(., 0.75), 
    max = max,
    mean = mean, 
    sd = sd))
dim(data_q.sum)
data_q.statsum <- data_q.sum %>% gather(stat, val) %>%
 separate(stat, into = c("var", "stat"), sep = "_") %>%
 spread(stat, val) %>%
 select(var, min, q25, median, q75, max, mean, sd)
print(data_q.statsum)

#Scatterplot Fare Vs Age
#data_q = read.csv("Airprojectcopy.csv")
#plot(data_q$AGE,data_q$FARE,xlim=c(0,90),ylim=c(0,550),xlab="Age",ylab="Fare",main="Scatterplot of Fare by Age",type="p",pch=16,col="blue")+hw

#NORMALIZE THE SKEWNESS
#min-max normalization
mmnorm.AGE <- (data_q$AGE-min(data_q$AGE))/(max(data_q$AGE)-min(data_q$AGE))
mmnorm.AGE
m<-mean(mmnorm.AGE)
std<-sqrt(var(mmnorm.AGE))
hist(mmnorm.AGE,prob=TRUE,main="Min-Max Normalized Age")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)
summary(mmnorm.AGE)
#min-max does not improve the skewness of the data
#calculate skewness
minmaxAGE_skew <- (3*(mean(mmnorm.AGE) - median(mmnorm.AGE))) /sd(mmnorm.AGE)
minmaxAGE_skew

#Z-score
zscore.AGE<-(data_q$AGE-mean(data_q$AGE))/sd(data_q$AGE)
zscore.AGE
m<-mean(zscore.AGE)
std<-sqrt(var(zscore.AGE))
hist(zscore.AGE,prob=TRUE,main="Z-Score Normalized Age")
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)
summary(zscore.AGE)
#z-score does not improve the skewness
#calculate skewness
zscoreAGE_skew <- (3*(mean(zscore.AGE) - median(zscore.AGE))) /sd(zscore.AGE)
zscoreAGE_skew

#Passengers flying each seatclass
seat_customer <- read.csv("seatclass.csv")
View(seat_customer)
my_seat <- melt(seat_customer,id.vars = 'CUSTOMERS')
my_seat
seat_plot <- ggplot(my_seat,aes(x=value,y=CUSTOMERS))+
geom_bar(stat = "identity",fill=rainbow(n=length(seat_customer$SEATCLASS)))+geom_text(aes(label=CUSTOMERS),vjust=-0.2)+labs(y= "TOTAL PASSENGERS",x="SEATCLASS", title="PASSENGERS FLYING IN EACH SEATCLASS")+theme(legend.position="top")+hw
seat_plot

#Gender in each seatclass
gender_seat <- read.csv("genderseatclass.csv")
gender_seat$SEATCLASS <- factor(gender_seat$SEATCLASS, levels = gender_seat$SEATCLASS) 
genderseat.m <- melt(gender_seat, id.vars='SEATCLASS')
genderseat.m
genderseat_plot <- ggplot(genderseat.m,aes(SEATCLASS,value,fill= variable))+ geom_bar(stat="identity", position = "dodge")+geom_text(aes(label=value),position = position_dodge(width = 1),vjust=-0.2)+labs(y= "NO. OF PASSENGERS BY GENDER",x= "SEATCLASS",title="MALE AND FEMALE PASSENGERS FLYING IN EACH CLASS")+theme(legend.position="top")+scale_fill_manual(values = c("darkturquoise","deeppink3"))+hw+theme(legend.title=element_blank())
genderseat_plot

#Guests accompanying each customer
guests_customer <- read.csv("guests.csv")
View(guests_customer)
my_guests <- melt(guests_customer,id.vars = 'CUSTOMERS')
my_guests
guests_plot <- ggplot(my_guests,aes(x=value,y=CUSTOMERS))+
geom_bar(stat = "identity",fill=rainbow(n=length(guests_customer$GUESTS)))+geom_text(aes(label=CUSTOMERS),vjust=-0.2)+labs(y= "TOTAL PASSENGERS",x="GUESTS", title="GUESTS ACCOMPANYING MAIN CUSTOMER")+theme(legend.position="top")+scale_fill_manual(values = c("olivedrab","orange","orchid"))+hw
guests_plot

#GUESTS VS AVG FARE
guest1 <- read.csv("guestsfare.csv")
plot1 <-  ggplot(guest1,
             aes(x = GUESTS, y = AVERAGE_FARE)) +
geom_smooth(method="loess",span=.90,method.args=list(degree=1),
           size=1.5,color="blue") +
geom_point(shape=20,size=3,color="black",fill="red") +
labs(x="NUMBER OF GUESTS",
        y="AVERAGE FARE PAID BY CUSTOMERS",
      title="AVERAGE FARE BY CUSTOMERS TRAVELING WITH GUESTS") + hw
plot1

#seatclass vs avg fare
seatavg <- read.csv("seat1.csv")
my_avg <- melt(seatavg,id.vars = 'SEATCLASS')
my_avg
avg_plot <- ggplot(my_avg,aes(x=SEATCLASS,y=value))+
geom_bar(stat = "identity",fill=c("olivedrab","orange","orchid"),width=0.8)+geom_text(aes(label=value),hjust=1.8)+labs(y= "AVERAGE FARE",x="SEATCLASS", title="AVERAGE FARE IN EACH CLASS")+theme(legend.position="top")+scale_fill_manual(values = c("olivedrab","orange","orchid"))+hw
avg_plot+coord_flip()

#boxplot
#runaway.csv or airways.csv or seatclassfare.csv
abc <- read.csv("Airprojectcopy.csv")
colors = c("blue","green","red")
boxplot(abc$FARE~abc$SEATCLASS, data=abc,main="Total fare",xlab="seatclass",ylab="fare",col=colors)+hw

#Density plot for average fare
def <- read.csv("Airprojectcopy.csv")
p8 <- ggplot(def, aes(x = def$FARE)) +
         geom_density(color="red", fill="orange")+geom_vline(xintercept = mean(def$FARE), size = 1, colour = "blue",linetype = "dashed")+hw+labs(x="Average Fare",y="Density", title="Average Fare for the Airline")
p8

#Success vs GENDER
success <-  read.csv("successsex.csv")
success$SUCCESS <- factor(success$SUCCESS, levels = success$SUCCESS) 
success.m <-  melt(success, id.vars = 'SUCCESS')
success.m
success_plot <- ggplot(success.m,aes(SUCCESS,value,fill= variable))+ geom_bar(stat="identity", position = "dodge")+geom_text(aes(label=value),position = position_dodge(width = 1),vjust=-0.2)+labs(y= "NO. OF PASSENGERS",x= "SUCCESS",title="SUCCESS RATE OF PASSENGERS FLYING THE AIRLINE")+theme(legend.position="top")+scale_fill_manual(values = c("darkturquoise","deeppink3","orange"))+hw+theme(legend.title=element_blank())
success_plot

#Principal component
#library(ggfortify)
pcadoc <- read.csv("pca1.csv")
pr.out=prcomp(pcadoc, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
pr.out$x
dim(pr.out$x)
#pr.out$rotation=-pr.out$rotation
#pr.out$x=-pr.out$x
#biplot(pr.out, scale=0,cex=.9)
#pr.out$sdev
#pr.var=pr.out$sdev^2
#pr.var
#pve=pr.var/sum(pr.var)
#pve
# qplot(x,pve, xlab="Principal Component", 
#ylab="Proportion of Variance Explained", 
#main="Airline project",ylim=c(0,1)) +
#geom_line()+geom_point(shape=21,fill="red",cex=3)+hw
myshow <- as.data.frame(pr.out$x)
autoplot(prcomp(pcadoc), data = myshow, colour = 'SUCCESS')
