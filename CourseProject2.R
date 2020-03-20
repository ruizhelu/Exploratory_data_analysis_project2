#Part1
#actually these two RDS files are downloaded in my working directory already with the link page.
#so I have not wirtten down the download code.
#unzip("exdatadataNEI_data.zip")
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

head(NEI)
head(SCC)

str(NEI)

sum1<-sum(subset(NEI, NEI$year==1999)$Emissions)
sum2<-sum(subset(NEI, NEI$year==2002)$Emissions)
sum3<-sum(subset(NEI, NEI$year==2005)$Emissions)
sum4<-sum(subset(NEI, NEI$year==2008)$Emissions)

Emissions<-c(sum1,sum2,sum3,sum4)
years<-c(1999,2002,2005,2008)
plot1<-data.frame(Emissions=Emissions,years=years)

par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(years,Emissions,type = "p", xlim=c(1998,2008),pch=19,col="dodgerblue",xlab="Years",ylab=expression(PM[2.5]~" Emissions in U.S"),main=expression("U.S "~PM[2.5]~" Emissions by year"))
plot(years,Emissions,type = "l", xlim=c(1998,2008),lwd=2,col="dodgerblue",xlab="Years",ylab=expression(PM[2.5]~" Emissions in U.S"),main=expression("U.S "~PM[2.5]~" Emissions by year"))
dev.copy(png,"question1.png", width=900, height=480)
dev.off()

#Part2
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

subbt<-subset(NEI,NEI$fips==24510)
sum1<-sum(subset(subbt,subbt$year==1999)$Emissions)
sum2<-sum(subset(subbt,subbt$year==2002)$Emissions)
sum3<-sum(subset(subbt,subbt$year==2005)$Emissions)
sum4<-sum(subset(subbt,subbt$year==2008)$Emissions)

Emissions<-c(sum1,sum2,sum3,sum4)
years<-c(1999,2002,2005,2008)
plot2<-data.frame(Emissions=Emissions,years=years)
plot(years,Emissions,type = "o", xlim=c(1998,2008),pch=19,lwd=2,col="dodgerblue",xlab="Years",ylab=expression(PM[2.5]~" Emissions in Baltimore, Maryland"),main=expression("Baltimore, Maryland "~PM[2.5]~" Emission by year"))
dev.copy(png,"question2.png", width=900, height=480)
dev.off()

#Part3
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

subbt<-subset(NEI,NEI$fips==24510)
subtype<-aggregate(Emissions~year+type,subbt,sum)

library("ggplot2")
qplot(year,Emissions,data=subtype,col=type,geom=c("point","line"),xlab="Years",ylab=expression(PM[2.5]~" Emissions in Baltimore"),main=expression("Baltimore, Maryland "~PM[2.5]~" Emission by year and type"))
dev.copy(png,"question3.png", width=900, height=480)
dev.off()

#Part4
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

coal<-grepl("coal",SCC$Short.Name,ignore.case=TRUE)
ccr<-SCC[coal,]
ccrdata<-subset(NEI,NEI$SCC %in% ccr$SCC)
sumccr<-aggregate(Emissions~year,ccrdata,sum)

library("ggplot2")
#qplot cannot print the color of my favorite dodgerblue here, so I use ggplot.
#qplot(year,Emissions,data=sumccr,geom=c("point","line"),xlab="Years",ylab=expression(PM[2.5]~" Emissions of ccr in US"),main=expression("Coal combustion-related "~PM[2.5]~" Emission by year"))
ggplot(sumccr,aes(year,Emissions))+geom_point(col="dodgerblue") +geom_line(col="dodgerblue")+xlab("Years")+ylab(expression(PM[2.5]~" Emissions of ccr in US"))+ggtitle(expression("Coal combustion-related "~PM[2.5]~" Emission by year"))
dev.copy(png,"question4.png", width=900, height=480)
dev.off()

#Part5
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

submvbt<-subset(NEI,NEI$fips==24510&NEI$type=="ON-ROAD")
summvbt<-aggregate(Emissions~year,submvbt,sum)

library("ggplot2")
ggplot(summvbt,aes(year,Emissions))+geom_point(col="dodgerblue") +geom_line(col="dodgerblue")+xlab("Years")+ylab(expression(PM[2.5]~" Emissions of mv in BT in US"))+ggtitle(expression("Motor Vehicle in Baltimore "~PM[2.5]~" Emission by year"))
dev.copy(png,"question5.png", width=900, height=480)
dev.off()

#Part6
NEI<-readRDS("summarySCC_PM25.rds")
SCC<-readRDS("Source_Classification_Code.rds")

subbl<-subset(NEI,NEI$fips%in%c("24510","06037")&NEI$type=="ON-ROAD")
sumbl<-aggregate(Emissions~year+fips,subbl,sum)

library("ggplot2")
ggplot(sumbl,aes(year,Emissions,col=fips))+geom_point()+geom_line()+xlab("Years")+ylab(expression(PM[2.5]~" Emissions in BT vs LA in US"))+ggtitle(expression("Baltimore compares to Los Angeles "~PM[2.5]~" Emission by year"))+scale_colour_discrete(name="Cities",labels=c("Baltimore","Los Angeles"))
dev.copy(png,"question6.png", width=900, height=480)
dev.off()
