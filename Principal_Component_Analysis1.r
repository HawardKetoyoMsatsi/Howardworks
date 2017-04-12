#PRINCIPAL COMPONENT ANALYSIS
# Work by Haward Ketoyo Msatsi
#My work while still learning R.
#
# Please ask me any question I am now an expert, I am willing to assist.
#
# hketoyo@gmail.com
#
#This work used data on Swiss bank notes that is free online. 
#please search and download the data then feel free to use this code.
#
#Importing the Data into R 
notes_data <- read.table("data_science/R/Assignments/ass1/swiss_banknote.txt",header = FALSE)
names(notes_data)=c("x1","x2","x3","x4","x5","x6")
attach(notes_data)
head(notes_data)
View(notes_data)
#let us save tis new data in our folder just in case.
write.csv(notes_data,"data_science/R/Assignments/swiss_banknote.csv", row.names=FALSE,fileEncoding = "utf8")

#To add an extra column we are told from the data description that,
# The first 100 observetions are genuine andde the remaining are counterfeit.
new_notes_data <- data.frame(Status=rep(c("GENUINE","FAKE"),c(100,100)),notes_data[,])
View(new_notes_data)
View(new_notes_data[,2:7])
swiss=scale(new_notes_data[,2:7])
eigen(cov(swiss))
#the principal component analysis implementation
swiss.pca<-prcomp(swiss)
summary(swiss.pca)

# Plotting the principal components
plot(swiss.pca)
screeplot(swiss.pca, type="line", main="Scree Plot")
## Kaisers criterion
(swiss.pca$sdev)^2  
# Loadings
swiss.pca$rotation[,1:2]
sum(swiss.pca$rotation[,1]^2)
## calculate the values of the first principal component for each observation
swiss.pca$x[,1] 
# calculate the values of the second principal component for each observation
swiss.pca$x[,2]  
# make a scatterplot
plot(swiss.pca$x[,1],swiss.pca$x[,2],main="Component2 against component1",xlab="component1",ylab="component2") 
text(swiss.pca$x[,1],swiss.pca$x[,2], new_notes_data$Status, cex=0.7, pos=1, col="red") # add labels
# make a scatterplot
plot(swiss.pca$x[,3],swiss.pca$x[,4],main="Component4 against component3",xlab="component3",ylab="component4") 
text(swiss.pca$x[,3],swiss.pca$x[,4], new_notes_data$Status, cex=0.7, pos=1, col="red") # add labels

