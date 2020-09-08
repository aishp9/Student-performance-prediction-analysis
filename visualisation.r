library(ggplot2)
maths<-read.csv("stud_math.csv")
maths2 = maths
attach(maths)
#histogram
ggplot(maths,aes(age))+geom_histogram(breaks=seq(10,30),col="red",fill="green",alpha=0.2)
 #boxplot
ggplot(maths,aes(school,age, color = school))+geom_boxplot(outlier.color="red")
#Desc Stats
maths2$Medu = factor(maths2$Medu ,  levels = c(0,1,2,3,4), labels = c('(0)none','(1)primary','(2)middle','(3)secondary','(4)higher'))
maths2$Fedu = factor(maths2$Fedu ,  levels = c(0,1,2,3,4), labels = c('(0)none','(1)primary','(2)middle','(3)secondary','(4)higher'))
maths2$traveltime = factor(maths2$traveltime ,  levels = c(1,2,3,4), labels = c('(1)<15m','(2)15-30m','(3)30m-1h','(4)>1h'))
maths2$studytime = factor(maths2$studytime ,  levels = c(1,2,3,4), labels = c('(1)<2h','(1)2-5h','(1)5-10h','(1)>10h'))
maths2$famrel = factor(maths2$famrel ,  levels = c(1,2,3,4,5), labels = c('(1)VeryBad','(2)Bad','(3)Ok','(4)good','(5)Excellent'))
maths2$health = factor(maths2$health ,  levels = c(1,2,3,4,5), labels = c('(1)VeryBad','(2)Bad','(3)Ok','(4)good','(5)VeryGood'))
maths2$freetime = factor(maths2$freetime ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths2$goout = factor(maths2$goout ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths2$Dalc = factor(maths2$Dalc ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
maths2$Walc = factor(maths2$Walc ,  levels = c(1,2,3,4,5), labels = c('(1)VeryLow','(2)Low','(3)Normal','(4)High','(5)VeryHigh'))
				
par(mfrow=c(2,2))
barplot(table(maths2$Medu), main="Mother's edu", xlab="Education", col="pink")
barplot(table(maths2$Fedu), main="Father's edu", xlab="Education", col="blue")
barplot(table(Mjob), main="Mother's job", xlab="Profession", col="pink")
barplot(table(Fjob), main="Father's job", xlab="Profession", col="blue")

par(mfrow=c(2,2))
barplot(table(reason), main="Reason to Choose this School", xlab="", col="yellow")
barplot(table(guardian), main="Guardian", xlab="", col="lightgreen")
barplot(table(maths2$traveltime), main="Travel time", xlab="", col="orange")
barplot(table(maths2$studytime), main="Study time", xlab="", col="purple")				

par(mfrow=c(2,2))
barplot(table(failures), main="No. of Past Class Failures", xlab="", col="red")
barplot(table(maths2$famrel), main="Family Relationship Quality", xlab="", col="beige")
barplot(table(maths2$health), main="Student Health condition", xlab="", col="darkgreen")
res = hist(absences, main="Student Absences", xlab="Absent days", col="cadetblue1")

par(mfrow=c(2,2))
barplot(table(maths2$freetime), main="Free time after School", xlab="", col="darkseagreen")
barplot(table(maths2$goout), main="Going out with friends", xlab="", col="deepskyblue")
barplot(table(maths2$Dalc), main="Workday Alcoholism", xlab="", col="darkgoldenrod1")
barplot(table(maths2$Walc), main="Weekend Alcoholism", xlab="", col="darkgoldenrod4")
