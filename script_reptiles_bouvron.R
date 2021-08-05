x=c("here","tidyverse","ggpubr", "gridExtra") #object that contains the packages that will be used in the script
lapply(x,library, character.only=T) #call the packages with the function "library"

################################################
#BARPLOT - NUMBER OF OBSERVATIONS
################################################

df=read_tsv("data_bouvron_1.txt", col_names=F) #read the data table
df #check what it looks like
df[,3]=round((df[,2]*100)/sum(df[,2]),0) #add a column with the percentage of the number of observation per species
colnames(df)=c("species","n","per") #rename the columns
df=mutate(df,species=as.factor(species)) #change the variable "species" to factor
summary(df) #check if everything is ok

#Plot with percentages
p1<-ggplot(data=df, aes(x=species, y=per))+ #define the data source and the aesthetics of the plot
	geom_bar(stat="identity", width=.5)+ #call barplot
	labs(x="Espèces", y="% des observations", title="(A)")+ #titles of the whole plot, and of both axes
	scale_x_discrete(labels=c("Anguis fragilis"=expression(italic("A. fragilis")), "Podarcis muralis"=expression(italic("P. muralis")), "Lacerta bilineata"=expression(italic("L. bilineata")),"Zamenis longissimus"=expression(italic("Z. longissimus")),"Natrix helvetica"=expression(italic("N. helvetica"))))+ #change the x-axis tick mark labels
	theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1,face="italic"))+ #rotate the x-axis tick mark labels of 45° and use an italic font
	theme(plot.title=element_text(size=10), axis.title=element_text(size=8), axis.text.x=element_text(size=7), axis.text.y=element_text(size=7)) #adjust the size of the titles and tick mark labels on both axes

#Plot with actual values
jpeg(file="barplot_count.jpg", width=17, height=17, units="cm", res=200) #save the figure as a jpg file
ggplot(data=df, aes(x=species, y=n))+ #define the data source and the aesthetics of the plot
	geom_bar(stat="identity")+ #barplot representation
	labs(x="Espèces", y="Nombre d'observations")+ #titles of the axes
	theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1,face="italic")) #rotate the text of the axis ticks to 45° and use an italic font
dev.off() #save the figure in the current directory


################################
#PLOT SPECIES RICHNESS BY PLATE
################################

df2=read_tsv("data_bouvron_2.txt") #read the data table
df2 #check what it looks like

#build a new table with species richness per plate
df2<-df2 %>%
	mutate(plaque_ID=as.factor(plaque_ID), milieu=as.factor(milieu)) %>% #transform the variable plaque_ID as factor
	mutate(Pomu=replace(Pomu, Pomu>0,1),Labi=replace(Labi, Labi>0,1),Anfr=replace(Anfr, Anfr>0,1),Nahe=replace(Nahe, Nahe>0,1),Zalo=replace(Zalo, Zalo>0,1)) %>% #replace all values that are not zeros by 1
	mutate(n_sp=rowSums(across(where(is.numeric)))) #adds a column "n_sp" that sums the "1" values for each row where the variables are as numeric
	
#Plot species richness by plate
p2<-ggplot(df2, aes(x=reorder(plaque_ID,-n_sp), y=n_sp, fill=milieu))+ #define the data source and the aesthetics of the plot, as well as the color fill of the bars according to the variable "milieu"
	geom_bar(stat="identity",color="black", size=0.2,)+ #call barplot
	scale_fill_grey(start=.7, end=.9,name="Habitat", labels=c("Lisière","Prairie"))+ #fill the bars with two shades of grey according to habitat type
	labs(x="Plaques", y="Richesse spécifique", title="(B)")+ #provide axes titles and main title
	theme(plot.title=element_text(size=10), axis.title=element_text(size=8), axis.text.x=element_text(size=6), axis.text.y=element_text(size=7))+ #set text size
	theme(legend.title=element_text(size=7), legend.text=element_text(size=6))+ #set legend size
	theme(legend.key.size=unit(0.2,"cm"))+ #set legend elements size
	theme(legend.position=c(0.9,0.9)) #set legent position
	
#some extra descriptive statistics on this data set
sd(df2$n_sp) #standard deviation of the number of species
range(df2$n_sp) #range of the number of species
quantile(df2$n_sp) #quantile of the number of species

chisq.test(df2$n_sp) #chisquare test to test whether the richness significantly varies among plates
#Chi-squared test for given probabilities
#data:  df2$n_sp
#X-squared = 19.25, df = 23, p-value = 0.6866

#beware of the interpretation of these results, because the richness values only vary from 1 to five with the following frequencies:
#1 : 7
#2 : 6
#3 : 3
#4 : 4
#5 : 4

################################################
#NUMBER OF INDIVIDUALS/SPECIES/PLATE
################################################

df3=read_tsv("data_bouvron_3.txt") #read the data table
df3 #check what it looks like
df3<-mutate(df3,plaque_ID=as.factor(plaque_ID), species=as.factor(species)) #change the class of the variables "plaque_ID" and "species" to factor.

#Plot the mean number of observations by plate for each species with a boxplot
p3<-ggplot(df3, aes(x=species, y=n))+ #define the data source and the aesthetics of the plot
	geom_boxplot()+ #call boxplot
	theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ #rotate the x-axis tick mark labels of 45°
	theme(plot.title=element_text(size=10), axis.title=element_text(size=8), axis.text.x=element_text(size=7), axis.text.y=element_text(size=7))+ #set text size
	scale_x_discrete(labels=c("Pomu"=expression(italic("P. muralis")), "Labi"=expression(italic("L. bilineata")),"Anfr"=expression(italic("A. fragilis")),"Zalo"=expression(italic("Z. longissimus")),"Nahe"=expression(italic("N. helvetica"))))+ #change the x-axis tick mark labels
	labs(x="Espèces", y="Nombre d'observations par plaque", title="(C)") #set axes titles and main title

#one-way ANOVA test to test if there is a significant difference between species
res.aov=aov(n~species, data=df3)
#plot to check the post-hoc conditions of the ANOVA
par(mfrow=c(2,2))
plot(res.aov) #there are some outliers and the residuals do not seem to have a normal distribution
shapiro.test(res.aov$res) #to test the normality of the residuals of the ANOVA
#	Shapiro-Wilk normality test
#
#data:  res.aov$res
#W = 0.52445, p-value < 2.2e-16

#The residuals of the ANOVA do not follow a normal distribution
#We apply a log transformation on the n values +1, we add 1 to avoid log(0) wich produces -Inf in R

df3$log_n<-log(df3$n+1)
#we apply the ANOVA on these new values
res.aov2=aov(log_n~species, data=df3)
par(mfrow=c(2,2))
plot(res.aov2) #looks better, no outliers and the distribution of residuals seem to fit a normal distribution
shapiro.test(res.aov2$res) 
#	Shapiro-Wilk normality test
#
#data:  res.aov2$res
#W = 0.94614, p-value = 0.0001137
#Actually the residuals do not fit a normal distribution, the conditions for the ANOVA are not met.

#Still, we perform a summary of the ANOVA to check if there is a significant difference in the numbers according to the species

summary(res.aov2)
#             Df Sum Sq Mean Sq F value  Pr(>F)    
#species       4  53.08  13.271   23.03 5.4e-14 ***
#Residuals   115  66.27   0.576                    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Ther is a significant effect of species on the number.
#We now perform a Tukey test for a pairwise comparison between groups on the fitted values of the ANOVA to check which pair or pairs of species differ from each other in terms of numbers

tukey=TukeyHSD(res.aov2)
tukey
#  Tukey multiple comparisons of means
#    95% family-wise confidence level
#
#Fit: aov(formula = log_n ~ species, data = df2)
#
#$species
#                 diff        lwr       upr     p adj
#Labi-Anfr -3.63633383 -4.8124108 -2.460257 0.0000000
#Nahe-Anfr -3.11790918 -4.2939861 -1.941832 0.0000000
#Pomu-Anfr -3.09983453 -4.2759115 -1.923758 0.0000000
#Zalo-Anfr -2.61636026 -3.7924372 -1.440283 0.0000001
#Nahe-Labi  0.51842465 -0.6576523  1.694502 0.7387804
#Pomu-Labi  0.53649930 -0.6395777  1.712576 0.7134791
#Zalo-Labi  1.01997357 -0.1561034  2.196051 0.1217057
#Pomu-Nahe  0.01807465 -1.1580023  1.194152 0.9999992
#Zalo-Nahe  0.50154892 -0.6745280  1.677626 0.7616722
#Zalo-Pomu  0.48347427 -0.6926027  1.659551 0.7852958


#All the pairwise comparison between Anguis fragilis and all the other species are significant

#We can perform the same analysis without Anguis fragilis now

df3.red<-filter(df3,!species=="Anfr") #remove the data for Anguis fragilis
df3.red$species=factor(df3.red$species) #remove the value Anguis fragilis from the list of values
summary(df3.red) #check what it looks like

#plot the data in a boxplot
jpeg(file="boxplot_plaques_sans_anguis.jpg", width=17, height=17, res=300, units="cm") #save the figure as a jpg file
ggplot(df3.red, aes(x=species, y=n))+ #define the data source and the aesthetics of the plot
	geom_boxplot()+ #call boxplot
	theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ #rotate the x-axis tick mark labels of 45°
	scale_x_discrete(labels=c("Pomu"=expression(italic("P. muralis")), "Labi"=expression(italic("L. bilineata")),"Zalo"=expression(italic("Z. longissimus")),"Nahe"=expression(italic("N. helvetica"))))+ #change manualy the labels of the x axes
	labs(x="Espèces", y="Nombre d'observations par plaque") #titles of the axes
#save the figure in the current directory


#Same procedure as before
#One-way ANOVA test
res.aov3=aov(n~species, data=df3.red)
par(mfrow=c(2,2))
plot(res.aov3)
shapiro.test(res.aov3$res) #The residuals do not fit a normal distribution

#ANOVA with log-transformed data
res.aov4=aov(log_n~species, data=df3.red)
par(mfrow=c(2,2))
plot(res.aov4) #no outliers this time
shapiro.test(res.aov4$res) #The residuals do not fit a normal distribution, still we perform an ANOVA
summary(res.aov4)
#            Df Sum Sq Mean Sq F value Pr(>F)  
#species      3   12.5   4.165   3.183 0.0276 *
#Residuals   92  120.4   1.309                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Once again, there is a significant effect of species on the number of individuals
#We now perform a Tukey test for a pairwise comparison between groups on the fitted values of the ANOVA to check which pair or pairs of species differ from each other in terms of numbers

tukey=TukeyHSD(res.aov4)
tukey
#  Tukey multiple comparisons of means
#    95% family-wise confidence level
#
#Fit: aov(formula = log_n ~ species, data = df3)
#
#$species
#                diff        lwr       upr     p adj
#Nahe-Labi 0.51842465 -0.3456704 1.3825197 0.4008131
#Pomu-Labi 0.53649930 -0.3275958 1.4005944 0.3700154
#Zalo-Labi 1.01997357  0.1558785 1.8840686 0.0139135
#Pomu-Nahe 0.01807465 -0.8460204 0.8821697 0.9999407
#Zalo-Nahe 0.50154892 -0.3625461 1.3656440 0.4305489
#Zalo-Pomu 0.48347427 -0.3806208 1.3475693 0.4633052

#There is a significant difference between Zamenis longissimus and Lacerta bilineata

########################
#TEST AGE CLASSES
########################

df4=read_tsv("data_bouvron_4.txt") #read the data table
df4 #check what it looks like
df4<-mutate(df4, age=as.factor(age), milieu=as.factor(milieu)) #set the variables "age" and "milieu" as factor

#represent the data through a nested-barplot
df4$perc=round(df4$n*100/sum(df4$n),2) #add a column with percentages

df4_temp<-group_by(df4,age, milieu) %>% #group by "age" and "milieu"
	summarise(sum=sum(perc)) %>% #aggregate data by calculating the sum of percentages in each category
	ungroup() #ungroup the data table

df4_temp<-add_row(df4_temp,age="cumulé",milieu="lisiere",sum=sum(df4_temp[df4_temp$milieu=="lisiere",]$sum)) %>% #we add two rows of cumulate percentages (both categories grouped together)
	add_row(age="cumulé",milieu="prairie",sum=sum(df4_temp[df4_temp$milieu=="prairie",]$sum)) #we add two rows of cumulate percentages (both categories grouped together)

#nested-barplot
p5=ggplot(data=df4_temp, aes(x=age, y=sum, fill=age))+ #define the data source and the aesthetics of the plot
	geom_bar(position="dodge", stat="identity", width=0.7,color="black", size=0.2,show.legend=F)+ #call barplot
	scale_fill_grey(start=.2, end=.95)+ #set the color fill for the bars
	ylim(0,90)+ #set the range of the y-axis
	geom_text(aes(label=paste(sum, "%")),size=2,position=position_dodge(width=0.9),vjust=-0.25)+ #set the text bloc that will provide the percentage value above each bar
	facet_grid(.~milieu, labeller=as_labeller(c(prairie="Prairies",lisiere="Lisières")))+ #set the two barplot per habitat type ("milieu") with manuel labels
	labs(x="Type de milieu", y="% du nombre d'occurrences", title="(E)", width=0.5)+ #set axes titles
	theme(legend.position="none")+ #do not display the legend on the right
	theme(plot.title=element_text(size=10), axis.title=element_text(size=8), axis.text.x=element_text(size=7), axis.text.y=element_text(size=7)) #set text size

#ANOVA for hedges
df4.lis<-filter(df4, milieu=="lisiere") %>% #subset of df4 that contains only the data for "lisiere"
	mutate(log_n=log(n)) #add a column with the log value of "n"
aov.lis=aov(data=df4.lis,log_n~age) 	#ANOVA
summary(aov.lis) #summary of the ANOVA
#            Df Sum Sq Mean Sq F value Pr(>F)
#age          1   0.06  0.0605   0.036  0.855
#Residuals    8  13.49  1.6864                

#No significant effect of age on numbers of individuals in hedges

shapiro.test(aov.lis$res) #The residuals fit a normal distribution


#ANOVA for meadows, same procedure as previously
df4.pr<-filter(df4, milieu=="prairie") %>%
	mutate(log_n=log(n+1))
aov.pr=aov(data=df4.pr,log_n~age)
summary(aov.pr)
#            Df Sum Sq Mean Sq F value Pr(>F)  
#age          1  4.164   4.164   5.111 0.0537 .
#Residuals    8  6.518   0.815                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#5% threshold, we can consider that the effect of age on number of individuals is significant in meadows

shapiro.test(aov.pr$res) #The residuals fit a normal distribution


####################################
#TEST THIGMOTHERMY VS. HELIOTHERMY
####################################

df5=read_tsv("data_bouvron_5.txt") #read the data table
df5 #Check what it looks like
df5<-mutate(df5, species=as.factor(species), mode=as.factor(mode)) #set the class of "species" and "mode" to factor

#We run an ANOVA
aov.thermo=aov(data=df5, log(n)~mode)
summary(aov.thermo)
#            Df Sum Sq Mean Sq F value Pr(>F)  
#mode         1  14.78  14.780   9.914 0.0136 *
#Residuals    8  11.93   1.491                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The effect of the mode of thermoregulation on the number of individuals is significant

shapiro.test(aov.thermo$res) #The residuals fit a normal distribution

########################
#DISTANCE TO THE HEDGE
########################
df6<-read_delim("distance.csv",delim=";") %>% #read the data table
	mutate(species=as.factor(species)) #set the variable "species" to factor
summary(df6) #check what it looks like

#boxplot representation of the data set
p4=ggplot(data=df6, aes(x=factor(df6$species,levels=c("labi", "pomu", "anfr","nahe","zalo")), y=dist))+ #define the data source and the aesthetics of the plot
	geom_boxplot()+ #call boxplot
	labs(x="Espèces", y="Distance à la lisière (m)", title="(D)")+ #set axes and main titles
	theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ #rotate the x-axis tick mark labels of 45°
	theme(plot.title=element_text(size=10), axis.title=element_text(size=8), axis.text.x=element_text(size=7), axis.text.y=element_text(size=7))+ #set text size
	scale_x_discrete(labels=c("anfr"=expression(paste(italic("A. fragilis"))), "labi"=expression(paste(italic("L. bilineata"))),"nahe"=expression(paste(italic("N. helvetica"))),"pomu"=expression(paste(italic("P. muralis"))),"zalo"=expression(paste(italic("Z. longissimus"))))) #change the x-axis tick mark labels

#We run a non-parametric test on several groups
kruskal.test(dist~species, data=df6)
#	Kruskal-Wallis rank sum test

#data:  dist by species
#Kruskal-Wallis chi-squared = 6.3166, df = 4, p-value = 0.1767

#No significant difference between groups.

#############################
#CUMULATIVE COVERED DISTANCE
#############################
df7<-read_delim("distance_parcourue.csv",delim=";") %>% #read the data table
	mutate(species=as.factor(SP)) %>% #transform the variable "species" into factor
	mutate(dist=`cumul parcouru`, capt=`nbre de capture`) %>% #Change the name of the variables
	select(species, Sexe, Statut, ID, dist, capt) #select the variables
	
#We run a non-parametric test on several groups
kruskal.test(dist~species, data=df7)
#	Kruskal-Wallis rank sum test
#
#data:  dist by species
#Kruskal-Wallis chi-squared = 6.6599, df = 2, p-value = 0.0358

#There is a significant difference between groups.
#We now perform a pairwise comparison between species to detect which ones differ from another
pairwise.wilcox.test(df7$dist, df7$species)
#	Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
#
#data:  df7$dist and df7$species 
#
#     anfr  nahe 
#nahe 0.272 -    
#zalo 0.039 0.820
#
#P value adjustment method: holm 

#We see that the distances covered by Anguis fragilis significantly differ from Zamenis longissimus

my_comp=list(c("anfr","nahe"),c("anfr","zalo"),c("nahe","zalo"))

#boxplot of the data
p6=ggplot(data=df7, aes(x=species, y=dist))+ #define the data source and the aesthetics of the plot
	geom_boxplot()+ #call boxplot
	labs(x="Espèces", y="Distance parcourue cumulée (m)", title="(F)")+ #set axes and main titles
	stat_compare_means(label="p.signif", size=3,label.y=250, method="wilcox.test", ref.group="anfr")+ #add significance marks on the plot with A. fragilis as reference point
	stat_compare_means(method="kruskal",label.y=350, size=3)+ #add the value of the Kruskal-Wallis test on top of the plot
	ylim(0,400)+ #set y-axis range
	theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ #rotate the x-axis tick mark labels of 45°
	theme(plot.title=element_text(size=10), axis.title=element_text(size=8), axis.text.x=element_text(size=7), axis.text.y=element_text(size=7))+ #set text size
	scale_x_discrete(labels=c("anfr"=expression(paste(italic("A. fragilis"))), "nahe"=expression(paste(italic("N. helvetica"))),"zalo"=expression(paste(italic("Z. longissimus"))))) #change the x-axis tick mark labels

##############
#FINAL FIGURE
##############

#We cluster all the figures in the same panel
jpeg(file="figure4.jpg", width=17, height=17, units="cm", res=300) #save the figure as a jpg file
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2,nrow=3) #group all the plots within a single figure
dev.off() #save the figure in the current directory
