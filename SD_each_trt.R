#I have a data set of real field experiment which has 4 different varieties of wheat and 
#5 different levels of nitrogen were used. I have taken data of plant height at 30 days

df<- read.csv('clean.csv')
head(df)

attach(df)

df$rep<- as.factor(df$rep)
df$var<- as.factor(df$var)
df$nlvl<- as.factor(df$nlvl)

anova(lm(ph_30~rep+nlvl+var))

#install.packages("lsmeans")
require(lsmeans)
#install.packages("emmeans")
require(emmeans)
library(dplyr)
#install.packages('multcompView')
require(multcompView)

anova.rr<- aov(data = df, ph_30~var+nlvl)

summary(anova.rr)

df_summ<- group_by(df, var) %>% 
  summarise(mean=mean(ph_30), sd=sd(ph_30)) %>% 
  arrange(desc(mean))
print(df_summ)


turkey.rr<- TukeyHSD(anova.rr)

print(turkey.rr)


cld.rr<- multcompLetters4(anova.rr, turkey.rr)
print(cld.rr)


cld<- as.data.frame.list(cld.rr$var)
df_summ$Tukey<- cld$Letters

print(df_summ)


#creating bar plots
require(ggplot2)
ggplot(df_summ, aes(var, mean))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)

#Change labels of x 
ggplot(df_summ, aes(var, mean))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8)+
  scale_x_discrete(labels=c("Banganga", "Brikuti", "BL43", "Zinc"))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)


#adding labs
ggplot(df_summ, aes(var, mean))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)+
  labs(title= "Plant height at 30 days", y = "Value", x="Variety")
#adding theme

ggplot(df_summ, aes(var, mean))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)+
  labs(title= "Plant height at 30 days",y = "Plant height at 30 days (cm)", x="Variety")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 14))

#adding letter

ggplot(df_summ, aes(var, mean))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8)+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)+
  labs(title= "Plant height at 30 days",y = "Plant height at 30 days (cm)", x="Variety")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(aes(label=Tukey), nudge_x = 0.2, nudge_y = 2, size=5)


#adding colors
ggplot(df_summ, aes(var, mean))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8, color='black', fill="green")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)+
  labs(y = "Plant height at 30 days (cm)", x="Variety")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(aes(label=Tukey), nudge_x = 0.2, nudge_y = 2, size=5, color ="black")

#adding different color to different bars

ggplot(df_summ, aes(var, mean,fill= Tukey))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8, color='black')+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)+
  labs(y = "Plant height at 30 days (cm)", x="Variety")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(aes(label=Tukey), nudge_x = 0.2, nudge_y = 2, size=5, color ="black")

#remove legend

ggplot(df_summ, aes(var, mean,fill= Tukey))+
  geom_bar(stat = "identity", width = 0.8, alpha=0.8, color='black', show.legend = FALSE)+
  scale_x_discrete(labels=c("Banganga", "Brikuti", "BL43", "Zinc"))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2)+
  labs(y = "Plant height at 30 days (cm)", x="Variety")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  text = element_text(size = 15, color = "black"),
  plot.title = element_text(hjust = 0.5, face="bold"),
  axis.ticks = element_line(color = "black", size = 0.9),
  axis.text = element_text(color = "black", size = 12,face="bold"))+
  
  #we can use grid line using panel.grid.major=element_line(color='blue' linetype='dashed')
  geom_text(aes(label=Tukey), nudge_x = 0.2, nudge_y = 2, size=5, color ="black")+
  ggtitle("Plant height of different variety at 30 days")

#save plot
ggsave('barplot3.png', dpi = 1000)

