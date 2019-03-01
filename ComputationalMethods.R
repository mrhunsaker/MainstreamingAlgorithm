################################################################################
################################################################################
######      Analysis Pipeline for TransEnvironmental Programming          ######
################################################################################
################################################################################

################################################################################
################################################################################
# Preprocessing Steps:
#
# Student data needs to be entered into an Excel or Google Docs sheet with the
# following column names:
# Name - this can be a student identifier or name.
# Outcome - this is where the team envisions the student being placed
# FSIQ - This is the Full Scale IQ, General Performance Index, or PRI from an IQ Test
# Basic_Reading_Skills - The Basic Reading Skills Score on the Woodcock Johnson
# Reading_Comp - The Reading Comprehension Score on the Woodcock Johnson
# Math_Calc - The Math Calculation Score on the Woodcock Johnson
# Math_Reasoning - The Math Reasoning Score on the Woodcock Johnson
# Written_Lang - The Written Language Score on the Woodcock Johnson
# Adaptive - General Adaptive Composite from Vineland or ABAS.
# SocioEmotional - Anxiety T score, BSI T Score, or other relevant Score from BASC, CBCL
# CBM_Math - Performance on District benchmarks or classroom CFA (%ile)
# CBM_Reading - Performance on District benchmarks or classroom CFA (%ile)
#
# When all the data are input, the file needs to be saved or exported as a .csv
# file. This is available as an option under Save As or Export.
#
# An important note is the Algorithm will print as a student Name whatever is in
# The "Name" column on any heatmap. These names should be omitted per FERPA prior
# to sharing any information outside IEP/School teams.
#
#
#
################################################################################
################################################################################

############################# Load data folder #################################
setwd("") #Put the address to the folder that contains the student data in between the quotation marks

########################## Verify Working Directory ############################
WD<-getwd() #This should print to the screen the address of the folder that contains your data

########## Verify required data are present in the working directory ###########
list.files(WD) #You should see the file with the data printed on the screen along with all the other files in the folder

#################################################################################
#################################################################################

########### Install and load  all needed packages for the algorithms ###########
install.packages(c("gplots", "RColorBrewer", "rpart", "rpart.plot", "e1071"), dependencies=TRUE, repos="https://cloud.r-project.org/")
library(e1071)
library(gplots)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
################################################################################
################################################################################
######                    Individual School Data                         #######
################################################################################
################################################################################

## School X Data
SXData<-read.csv("SchoolX_data.csv", head=TRUE, sep=",")

SXData<-subset(Data,select=c(Name,Outcome, FSIQ,Basic_Reading_Skills,Reading_Comp,Math_Calc,Math_Reasoning,Written_Lang,Adaptive,SocioEmotional, CBM_Math, CBM_Reading, WJIII, CBM))

SXclusterdata<-subset(SXData, select=c(FSIQ,Basic_Reading_Skills,Reading_Comp,Math_Calc,Math_Reasoning,Written_Lang,Adaptive,SocioEmotional, CBM_Math, CBM_Reading))

SXClusteringata<-as.matrix(SXclusterdata)

## School X Data Output into Heatmap
mypallete<-colorRampPalette(c("#e66101","#fdb863","#b2abd2","#4e3c99"))(n=256)
pdf("Heatmap_SX.pdf", width=8.5, height=14, bg="transparent", colormodel="cmyk",  pagecentre=TRUE)

heatmap.2(SXClusteringdata, main="SX", col=mypallete, scale="none",rowsep=1:100, colsep=1:10, sepcol="white", sepwidth=c(.015,.025), trace="none", labRow=SXData$Name, margins=c(10,10), cexRow=.75, cexCol=1, keysize=2, lhei=c(3,10))
dev.off()

################################################################################
#
#	Repeat this for each school, replacing "SX" with desired school identifier
#            In this document I will just use S1Data, S2Data, ...
################################################################################


################################################################################
################################################################################
######                    Omnibus School Data                            #######
################################################################################
################################################################################

## Combine all data into a single entity
merged_data<-rbind(S1Data, S2Data, S3Data, ... ) #put the name for all the School Data

Overallclusterdata<-subset(merged_data, select=c(FSIQ,Basic_Reading_Skills,Reading_Comp,Math_Calc,Math_Reasoning,Written_Lang,Adaptive,SocioEmotional, CBM_Math, CBM_Reading))

OverallClusteringdata<-as.matrix(Overallclusterdata)

OverallClusteringdata<-na.omit(Overallmydata) #This gets rid of missing data

## Overall Output into Heatmap
mypallete<-colorRampPalette(c("#e66101","#fdb863","#b2abd2","#4e3c99"))(n=256)

pdf("Heatmap_Overall.pdf", ,width=8.5,height=14, bg="transparent",colormodel="cmyk", pagecentre=TRUE)

heatmap.2(OverallClusteringdata, main="Combined School Data", col=mypallete, scale="none", rowsep=1:200, colsep=1:10,  sepcol="white", sepwidth=c(.015,.025), trace="none", labRow=merged_data$Name, margins=c(10,10), cexRow=.75, cexCol=1, keysize=2, lhei=c(3,10))
dev.off()

################################################################################
################################################################################
######           Regression Tree Confirmation of Decision Tree           #######
######               Using the data files generated above                #######
################################################################################
################################################################################

##################### Confirmation of Decision Trees ###########################
## Academic Testing (WJ-IIINU Separated into Component Subtests)

fit1<-rpart(Outcome~Adaptive+FSIQ+CBM_Math+CBM_Reading+Basic_Reading_Skills+Reading_Comp+Math_Reasoning+Math_Calc+Written_Lang+SocioEmotional, data=na.omit(merged_data),  cost=c(3,1,2,2,2,2,2,2,2,1), parms=list(prior=c(.4,.6)), method="class",  control=rpart.control(minsplit=1,minbucket=1,cp=-1))

pdf("RegressionTreeAllAcademics.pdf", paper="letter", bg="transparent",colormodel="cmyk", pagecentre=TRUE)

rpart.plot(fit1,type=0,extra=100, branch.lty=1, shadow.col="gray", nn=TRUE,  under=TRUE, tweak=.75, main="Decision Tree (Academic Testing Separated)")
dev.off()

## Academic Testing (WJ-IIINU Domains Binned into an Average)
fit2<-rpart(Outcome~Adaptive+FSIQ+WJIII+CBM+SocioEmotional, data=na.omit(OHdata), method="class",  parms=list(prior=c(.4,.6)), cost=c(3,1,2,2,1), control=rpart.control(minsplit=1, minbucket=1, cp=-1, mincriterion=.5))

pdf("RegressionTreeAcademicsAveraged.pdf", paper="letter",  bg="transparent", colormodel="cmyk", pagecentre=TRUE)

rpart.plot(fit2,type=0,extra=100, branch.lty=1, shadow.col="gray", nn=TRUE, under=TRUE,tweak=.75, main="Decision Tree (Academic Testing Present)")
dev.off()

## Academic Testing Absent
fit3<-rpart(Outcome~Adaptive+FSIQ+SocioEmotional, data=na.omit(merged_data),  method="class", parms=list(prior=c(.4,.6)), cost=c(3,1,1),  control=rpart.control(minsplit=1, minbucket=1,cp=-1))

pdf("RegressionTreeNoAcademics.pdf", paper="letter", bg="transparent",colormodel="cmyk", pagecentre=TRUE)

rpart.plot(fit3,type=0,extra=100, branch.lty=1, shadow.col="gray", nn=TRUE, under=TRUE, tweak=.75, main="Decision Tree (Academic Testing Absent)")
dev.off()

################################################################################
################################################################################
######   Support Vector Machines Modeling of Decision Tree Placements    #######
######       All AA Students in Granite School District Combined         #######
######           Final Decision Tree Placement Used as "Correct"         #######
################################################################################
################################################################################

# Set up text file for simulation results to be written to file named by the date
sink(paste(format(Sys.time(), "%Y-%m-%d %I-%p"), "txt", sep = "."), type="output", split=FALSE)

############# Iterative K means Confirmation of SVM Results ####################
print(Sys.time()) # Time Stamps the Output File

## Split the Learning and Test Set by Different values
print("K means cross validation - 30 v 31")
svm.fit<-svm(Outcome~Adaptive+FSIQ+WJIII+CBM+SocioEmotional, data=na.omit(merged_data), kernel="linear", cross=30, probability=TRUE)
print(svm.fit)
print(summary(svm.fit))
print(predict(svm.fit))

print("K means cross validation - 20 v 41")
svm.fit2<-svm(Outcome~Adaptive+FSIQ+WJIII+CBM+SocioEmotional, data=na.omit(merged_data), kernel="linear", cross=20, probability=TRUE)
print(svm.fit2)
print(summary(svm.fit2))
print(predict(svm.fit2))

print("K means cross validation - 10 v 51")
svm.fit3<-svm(Outcome~Adaptive+FSIQ+WJIII+CBM+SocioEmotional, data=na.omit(merged_data), kernel="linear", cross=10, probability=TRUE)
print(svm.fit3)
print(summary(svm.fit3))
print(predict(svm.fit3))

print("K means cross validation - 5 v 56")
svm.fit4<-svm(Outcome~Adaptive+FSIQ+WJIII+CBM+SocioEmotional, data=na.omit(merged_data), kernel="linear", cross=5, probability=TRUE)
print(svm.fit4)
print(summary(svm.fit4))
print(predict(svm.fit4))

print("K means cross validation - 3 v 58")
svm.fit5<-svm(Outcome~Adaptive+FSIQ+WJIII+CBM+SocioEmotional, data=na.omit(merged_data), kernel="linear", cross=3, probability=TRUE)
print(svm.fit5)
print(summary(svm.fit5))
print(predict(svm.fit5))
sink()

################################################################################
################################################################################
############################## End Document ####################################
################################################################################
################################################################################
