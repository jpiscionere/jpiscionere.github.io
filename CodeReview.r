
H0=100
DEG_TO_RAD=0.01745328888
RAD_TO_DEG=57.2957914331
speed_of_light=299792.458
PI=3.14159
#install.packages('ggplot2')
library('ggplot2')
#install.packages("astrolibR")
library('astrolibR')
#install.packages('gridExtra')
library("gridExtra")
#install.packages("scatterplot3d")
library("scatterplot3d")

getwd() #get working directory
setwd(".") #set working directory


data <- read.csv("http://jpiscionere.github.io/HOPCAT2005TabSep.csv")

#data=read.csv("../Data/HOPCAT2005TabSep.csv")
#data=read.table("../Data.txt",head=T)
df_HIPASS=data.frame(data)
df_HIPASS$M_HI=2.365*10^5*(df_HIPASS$vel_mom/H0)^2*df_HIPASS$Sint #calculate HI Mass
df_HIPASS$Distance=df_HIPASS$vel_mom/H0 #use hubble approx. for distance even though astroR has a cosmo calc
summary(df_HIPASS)



plot(df_HIPASS$HicatExtl,df_HIPASS$HicatExtb) #try the tab complete if using the GUI or notebook

coords=glactc(gl = df_HIPASS$HicatExtl,gb = df_HIPASS$HicatExtb,j=2,year = 2000,degree = TRUE) #using astroR package
plot(coords$ra,coords$dec)
df_HIPASS$RA=coords$ra
df_HIPASS$Dec=coords$dec

df_HIPASS$Morphology


frequency<-table(df_HIPASS$Morphology) #table is a very useful command to summarize factors
summary(frequency)
frequency=as.data.frame(frequency) #let's turn it into a data frame so we can sort
frequency=frequency[order(frequency$Freq,decreasing=TRUE),] #sort the whole data frame by the frequency of each morphology
frequency

frequency$Var1[c(2:11)] #Skipping "XXXXX'
morphs=frequency$Var1[c(2:11)] #Putting the morph types in a vector
morphs #checking
df_Short <- df_HIPASS[df_HIPASS$Morphology %in% morphs, ] #subsetting the data frame to only those galaxies whose morphs are in the vector 
length(df_Short$Morphology) #checking that the data frame is smaller
df_Short$Morphology[c(1:100)] #checking that there are only the morphs we want
qplot(df_Short$Sint,fill=df_Short$Morphology,xlim=c(0,100)) #qplot "quick plot" is a ggplot tool

#this is one approach-- cleaning up the data, but it requires significant prior knowledge 
df_HIPASS$Morphology_Filtered=df_HIPASS$Morphology
df_HIPASS$Morphology_Filtered="LTG"

E_index=which(grepl('E', df_HIPASS$Morphology)=="TRUE" & 
              grepl('LINER',df_HIPASS$Morphology)=="FALSE" & 
              grepl('2MASS',df_HIPASS$Morphology)=="FALSE" &
              grepl('NELG',df_HIPASS$Morphology)=="FALSE")

df_HIPASS$Morphology_Filtered[grepl('XXXXX', df_HIPASS$Morphology)=="TRUE"]="Unclassified"
df_HIPASS$Morphology_Filtered[E_index]="E"
df_HIPASS$Morphology_Filtered[grepl('S0', df_HIPASS$Morphology)=="TRUE"]="S0"

df_HIPASS$Morphology[which(df_HIPASS$Morphology=="E")]


ggplot(df_HIPASS,aes(M_HI)) + #set up the data
geom_histogram() #tell it what to plot

ggplot(df_HIPASS,aes(M_HI,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_histogram()  #change the transparency for the curves




ggplot(df_HIPASS,aes(M_HI,color=Morphology_Filtered,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_density(alpha=0.1) + #change the transparency for the curves
    xlab("HI MASS") + #label x=axis
    theme_bw() + #change the background theme 
     scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="b") #fancy tick marks



ggplot(df_HIPASS,aes(x=M_HI,y=GalSepArcMin,color=Morphology_Filtered,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_point() + #plot points and change the transparency for the curves
    xlab("HI MASS") + #label x=axis
    theme_bw() + #change the background theme 
     scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="b") #fancy tick marks

summary(df_HIPASS$GalSepArcMin)

df_HIPASS$GalSepArcMin=as.numeric(as.character(df_HIPASS$GalSepArcMin))

ggplot(df_HIPASS,aes(x=M_HI,y=GalSepArcMin,color=Morphology_Filtered,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_point() + #plot points and change the transparency for the curves
    xlab("HI MASS") + #label x=axis
    theme_bw() + #change the background theme 
     scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="b") #fancy tick marks

ggplot(data=subset(df_HIPASS,Morphology_Filtered != "Unclassified"),
                   aes(x=M_HI,y=GalSepArcMin,color=Morphology_Filtered,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_point(alpha=0.5) + #plot points and change the transparency for the curves
    xlab("HI MASS") + #label x=axis
    theme_bw() + #change the background theme 
     scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="b") #fancy tick marks

ggplot(data=subset(df_HIPASS,Morphology_Filtered != "Unclassified"),
                   aes(x=M_HI,y=GalSepArcMin,color=Morphology_Filtered,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_point(alpha=0.5) +#plot points and change the transparency for the curves
    xlab("HI MASS") + #label x=axis
    theme_bw() + #change the background theme 
     scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="b") + #fancy tick marks
      geom_smooth(method = "lm") #lm= linear method

ggplot(data=subset(df_HIPASS,Morphology_Filtered != "Unclassified"),
                   aes(x=M_HI,y=GalSepArcMin,color=Morphology_Filtered,fill=Morphology_Filtered)) + #setup the base plot and factoring
    geom_point(alpha=0.1) +#plot points and change the transparency for the curves
    xlab("HI MASS") + #label x=axis
    theme_bw() + #change the background theme 
     scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="b") + #fancy tick marks
      geom_density_2d() #lm= linear method

ggplot(data=subset(df_HIPASS,Morphology_Filtered != "Unclassified"),
                   aes(y=M_HI,x=Morphology_Filtered)) + #setup the base plot and factoring
    theme_bw() + #change the background theme 
         geom_boxplot( ) +
scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="l")

f <- function(x) {
  r <- quantile(x, probs = c(0.025, 0.1573, 0.5, 0.84, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(data=subset(df_HIPASS,Morphology_Filtered != "Unclassified"),
                   aes(factor(Morphology_Filtered),M_HI)) + #setup the base plot and factoring
    theme_bw() + #change the background theme 

stat_summary(fun.data = f, geom="errorbar",aes(fill=Morphology_Filtered,width=0.5)) +
stat_summary(fun.data = f, geom="boxplot",aes(fill=Morphology_Filtered,width=0.5),position="dodge") +
scale_y_log10( breaks = scales::trans_breaks("log10", function(x) 10^x), #fancy tick marks
   labels = scales::trans_format("log10", scales::math_format(10^.x))) + #fancy tick marks
    annotation_logticks(side="l") + theme(legend.position="none")

ggsave("boxplot.pdf")

df_HIPASS$X=df_HIPASS$Distance*sin((90-df_HIPASS$Dec) * DEG_TO_RAD)*cos(df_HIPASS$RA * DEG_TO_RAD) #put galaxies on unit sphere
df_HIPASS$Y=df_HIPASS$Distance*sin((90-df_HIPASS$Dec) * DEG_TO_RAD)*sin(df_HIPASS$RA * DEG_TO_RAD) 
df_HIPASS$Z=df_HIPASS$Distance*cos((90-df_HIPASS$Dec) * DEG_TO_RAD)

rp_distances<-df_HIPASS$Distances*0
xyz_distances<-df_HIPASS$Distances*0
rp_distances_id<-df_HIPASS$Distances*0

for(i in c(1:length(df_HIPASS$X))){
    for(j in c(2:length(df_HIPASS$X))) {
             xyz_distances[j]=(df_HIPASS$X[i] - df_HIPASS$X[j])^2 + 
                              (df_HIPASS$Y[i] - df_HIPASS$Y[j])^2 + 
                              (df_HIPASS$Z[i] - df_HIPASS$Z[j])^2
    }
     rp_sqr_sorted<-sort(xyz_distances, method = "shell", index.return = TRUE)
     rp_distances[i]<-rp_sqr_sorted$x[3]
     rp_distances_id[i]<-rp_sqr_sorted$ix[3]
}
df_HIPASS$ThirdNN_MPC=sqrt(rp_distances)



scatterplot3d(log10(df_HIPASS$M_HI),df_HIPASS$GalSepArcMin,df_HIPASS$Red_Mag_AUTO_Calibrated)

df_HIPASS=na.omit(df_HIPASS) #only galaxies with morphology classifcations
galaxy_df=data.frame( #setting up a data frame that's a subset of the HIPASS data frame
                  RA=df_HIPASS$RA,
                  Dec=df_HIPASS$Dec,
                  Distance=df_HIPASS$Distance,
                  Bj_mag=df_HIPASS$Bj_Mag_AUTO_Calibrated,
                  R_mag=df_HIPASS$Red_Mag_AUTO_Calibrated,
                  Morphology=df_HIPASS$Morphology_Filtered,
                  M_HI=df_HIPASS$M_HI)
write.table(galaxy_df,"HIPASS_clean.txt",sep="\t",row.names = FALSE,col.names = FALSE,quote = FALSE)

install.packages("GGally")

library('GGally')

ggpairs(galaxy_df) 

galaxy_df$Bj_mag=as.numeric(as.character(galaxy_df$Bj_mag))


ggpairs(galaxy_df)



  
