##########ANALYSIS OF FACTORIAL COMPLETELY RANDOMIZED DESIGN##################
#for 2 and 3 factors for multiple response variable
data <- read.csv("~/AFCRD.csv")
View(data)
library(doebioresearch)
str(data)

#change to factor by using as.factor
data$REPLICATION=as.factor(data$REPLICATION)
data$NITROGEN=as.factor(data$NITROGEN)
data$PHOSPHORUS=as.factor(data$PHOSPHORUS)
data$POTASSIUM=as.factor(data$POTASSIUM)
str(data)

#MODEL1
#fcrd2fact(data,fact.A,fact.B,Multiple.comparison.test)
#Multiple.comparison.test
    #0 for no(no comparison tets) test
    #1 for LSD(least significance difference) test
    #2 for Duncan test
    #3 for HSD(harness significance difference) test

#CRD (completely random design) - Duncan test for(one response variable)- yield 
#only
fcrd2fact(data[5],data$NITROGEN,data$PHOSPHORUS,2) #two factors(nitrogen and 
                                                                # phosphorus)

#CRD (completely random design) - Duncan test for(two response variable)- yield 
#                                                                 & plant_height
fcrd2fact(data[5:6],data$NITROGEN,data$PHOSPHORUS,2)

#CRD for three factors- (two response variables) yield & plant_height - Duncan 
#                                                                         test
##fcrd2fact(data,fact.A,fact.B,fact.C,Multiple.comparison.test)
fcrd3fact(data[5:6],data$NITROGEN,data$PHOSPHORUS,data$POTASSIUM,2)



##########ANALYSIS OF FACTORIAL RANDOMIZED BLOCK DESIGN FOR 2 & 3 FACTORS######
DATA <- read.csv("~/RBD.csv")
View(DATA)
attach(DATA)

library(ExpDes)

#Analysis of factorial randomized block design for 2 factors. 
#     --->CV(coefficient of variation)-the smaller it is the better the results
#fat2.rbd(fact.A,fact.B,response variable,qualitative,multiple.comparison,
#factors.names,significance.t-values,significance.F-values)
fat2.rbd(NITROGEN,PHOSPHORUS,REPLICATION,YIELD,quali=c(TRUE,TRUE),
         mcomp='lsd',fac.names=c("N","P"),sigT=0.05,sigF=0.05)

#Analysis of factorial randomized block design for 2 factors
#fat3.rbd(fact.A,fact.A,fact.B,fact.C,response variable,are.factors.qualitative?
# ,multiple.comparison,factors.names,significance.t-values,significance.F-values)
fat3.rbd(NITROGEN,PHOSPHORUS,POTASSIUM,REPLICATION,YIELD,quali=c(TRUE,TRUE,TRUE),
         mcomp='lsd',fac.names=c("N","P","K"),sigT=0.05,sigF=0.05)


###############COMPLETELY RANDOMIZED DESIGN IN DOUBLE FACTORIAL SCHEME#########
RBD <- read.csv("~/RBD.csv")
attach(RBD)
View(RBD)

fat2.crd(GENERAL_TYPE,NITROGEN,PLANT_HEIGHT,quali = c(TRUE,TRUE),mcomp="tukey",
         fac.names =c('GE','N'),sigT = 0.05,sigF = 0.05)

####################COMPLETELY RANDOMIZED DESIGN############################
#crd(treatment,response variable,qualitative=TRUE/FALSE,multiple comparison,
#linear model=TRUE/FALSE,homogeneity/homocedasticity of variables,sigT,sigF)

#using tukey test
#crd(PLANT_TYPE,YIELD,quali = TRUE,mcomp = "tukey",nl=FALSE,
    #hvar = "oneillmathews",sigT = 0.05,sigF = 0.05)

#using LSD test
#crd(GENERAL_TYPE,PLANT_HEIGHT,quali = TRUE,mcomp = "lsd",
  #  hvar = "oneillmathews",sigT = 0.05,sigF = 0.05)

#for quantitative treatment
#crd(PLANT_HEIGHT,YIELD,quali = FALSE,nl=FALSE,mcomp = "lsd",hvar = "oneillmathews",sigT = 0.05,sigF = 0.05,unfold = 1)


####################LATIN SQUARE DESIGN###########################################
#latsd(treatment,row,column,response variable,qualitative status,multiple comparison,sigT,sigF)
LATSD <- read.csv("E:/LATSD.csv")
attach(LATSD)

latsd(TREATMENT,ROW,COL,GY,quali = TRUE,mcomp = "snk",sigT = 0.05,sigF = 0.05) #snk means STUDENT NEWMAN KEULS'S test, you can also use lsd,dancun or tukey test under mcomp



######################RANDOMIZE DIFFERENT EXPERIMENT DESIGNS############

#####################RANDOMIZATION FOR COMPLETE RANDOM DESIGN############
library(agricolae) #library for randomization

trt=c('A','B','C','D','E')
rep=c(4,4,3,4,3) #in CRD the replication is not necessarily equal
outdesign.crd=design.crd(trt,r=rep,seed=888,serie=0) #according to serie: 0 means 
#               start from 1, 1 means start from 11  and 2 means start from 101
book.crd=outdesign.crd$book #the field book for randomization of CRD
book.crd
write.csv(book.crd,file = "book.crd.csv") #save your randomization

####################RANDOMIZATION FOR RANDOMIZED BLOCK DESIGN#########
trt=c('A','B','C','D','E','F')
REP=4 #replication will be the same for all
outdesign.rcbd=design.rcbd(trt,r=REP,seed = 878,serie = 2)
book.rcbd=outdesign.rcbd$book
book.rcbd
#write.csv(book.rcbd,file = "book.rcbd.csv")

##################################RANDOMIZATION FOR LATIN SQUARE DESIGN########
trt.Lsd=c('A','B','C','D','E')
designLSD=design.lsd(trt.Lsd,serie = 0,seed = 23)
bookLSD=designLSD$book
designLSD$book
designLSD$sketch
#write.csv(bookLSD,file = "book.lsd.csv")

################################RANDOMIZATION FOR SPLIT-PLOT DESIGN###########
trt1=c('A','B','C','D') #this is the main plot
trt2=c(0,50,100) #this is a sub-plot
outdesign=design.split(trt1,trt2,r=3,serie = 0,seed = 600)
book=outdesign$book
book
#write.csv(book,file = "book.split-plot.csv",row.names = FALSE)

#####################RANDOMIZATION FOR INCOMPLETE BLOCK DESIGNS##########################################################
#lattice design

#simple lattice design, 5X5
library(agricolae)
lat=1:25
outdesign.lat=design.lattice(lat,r=2,serie = 0)
outdesign.lat$sketch

#tripple lattice
trt=LETTERS[1:9]
outdesign=design.lattice(trt,r=3,serie = 0)
outdesign$book
outdesign$sketch

#Alpha lattice design
trt=1:30 #number of treatments
t=length(trt)

#size block
k=5
#Blocks
s=t/k
#replications r
r=3

outdesign=design.alpha(trt,k,r,serie = 0)
outdesign$sketch

#Augumented block design
trt1=1:5 #checks
trt2=1:100 #unreplicated entries
rep=5 #number of replications of the checks

outdesign.aug=design.dau(trt1,trt2,r=rep,seed = 600,serie = 0)
outdesign.aug$book


##############################SPLIT PLOTS IN CRD and RBD#########################################################################
SP <- read.csv("~/RBD.csv")

library(ExpDes)

SP$REPLICATION=as.factor(SP$REPLICATION)
SP$GENERAL_TYPE=as.factor(SP$GENERAL_TYPE)
SP$NITROGEN=as.factor(SP$NITROGEN)
SP$POTASSIUM=as.factor(SP$POTASSIUM)
SP$PHOSPHORUS=as.factor(SP$PHOSPHORUS)
SP$PLANT_TYPE=as.factor(SP$PLANT_TYPE)
SP$MONTH=as.factor(SP$MONTH)
attach(SP)

#######split-plots in CRD
#split2.crd(main factor,support factor,replication,response variable,...)
split2.crd(MONTH,PLANT_TYPE,REPLICATION,YIELD,quali = c(TRUE,TRUE),mcomp="lsd",
           fac.names = c('month','plant type'),sigT =0.05,sigF = 0.05)

#######split-plots in RBD
#split2.rbd(MONTH,PLANT_TYPE,REPLICATION,YIELD,quali = c(TRUE,TRUE),mcomp = "lsd",fac.names = c("month","type"),sigT = 0.05,sigF = 0.05)


######################################THREE WAY ANOVA in COMPLETELY RANDOMIZED DESIGN############################
#fat3.crd(FACTOR1,FACTOR2,FACTOR3,REPONSE VARIABLE,quali = c(TRUE,TRUE,TRUE),mcomp="tukey",
         #fac.names =c('GE','N','FFGHJ'),sigT = 0.05,sigF = 0.05)