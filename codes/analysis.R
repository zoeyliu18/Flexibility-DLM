library(ggplot2)
library(gridExtra)
library(wesanderson)


####### Plotting flexibility #############

data<-read.csv(file="variability.csv",header=T,sep=",")
data$Structure <- paste(data[,3],data[,2])

p<-ggplot(data[,], aes(x=Language, y=Mean, color=Structure)) +
  #  geom_point(aes(colour=factor(Structure)))+
  geom_pointrange(aes(ymin=CI25, ymax=CI975)) +
  geom_text(aes(label=paste(Language)), vjust=-1.8,position=position_dodge(.9),size=3) +
  labs(x="") +
  labs(y="Variability") +
  labs(fill="")+
  scale_y_continuous(limits=c(0,1.2)) +
  scale_color_manual(values=wes_palette(n=4, name="Moonrise2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  theme(legend.position="top")

p

ggsave("variability.pdf", p)




data<-read.csv(file="corr.csv",header=T,sep=",")

#names(data)<-c('Language','Flexibility','DLM','Order','Group')

#data$Group[data$Group %in% c('C')] <- B


######### Testing relationship between DLM and flexibility #######

mod <- brm(DLM ~ Flexibility + (1|Order) + (1|Group),
           data = try, #data, 
           warmup = 500,
           iter = 3000,
           chains = 2,
           inits="0",
           prior=prior,
           #                family = "Gamma",
           control = list(adapt_delta = 0.9999, max_treedepth = 15),
           cores = 2)

saveRDS(mod, 'all_BC.mod')


mod <- brm(DLM ~ Flexibility, # + (1|Order),
                  data = D, 
                  warmup = 500,
                  iter = 3000,
                  chains = 2,
                  inits="random",
                  prior=prior,
                  control = list(adapt_delta = 0.999999999999, max_treedepth = 15),
                  cores = 2)

saveRDS(mod, 'C.mod')

######### Plotting flexibility against DLM ########

A <- subset(data, Group %in% c('A'))

A$Language <- factor(A$Language,levels=c("Arabic", "Hebrew",
                                         "English", "Danish", "Norwegian", "Swedish",
                                         "French", "Galician",  "Italian", "Portuguese", "Romanian", "Serbian", "Urkainian", "Bulgarian",
                                         "Greek",
                                         "Indonesian",
                                         "Latvian",
                                         "Irish",
                                         "Finnish",
                                         "Wolof")) 


A_dlm <- ggplot(A, aes(Language, DLM)) + 
#  geom_text(aes(label=paste(Language)), vjust=-3,position=position_dodge(.9),size=10) +
  geom_point(color="steelblue", size=4) +
  geom_errorbar(aes(ymax = DLM_CI975, ymin = DLM_CI25), width=.1, position=position_dodge(.9), color="steelblue") +
  labs(x="") +
  labs(y="DLM") +
  scale_y_continuous(limits=c(-0.5,2.5)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('A_dlm.pdf', A_dlm)

A_flexibility <- ggplot(A, aes(Language, Flexibility)) + 
#  geom_text(aes(label=paste(Language)), hjust=-.5, vjust=-2.3,position=position_dodge(.9),size=8) +
  geom_point(color="#52854C", size=4) +
  geom_errorbar(aes(ymax = Flexibility_CI975, ymin = Flexibility_CI25), width=.1, position=position_dodge(.9), color="#52854C") +
  labs(x="") +
  labs(y="Flexibility") +
  scale_y_continuous(limits=c(0.,1.1)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('A_flexibility.pdf', A_flexibility)

#plot = grid.arrange(A_flexibility, A_dlm, ncol=1, nrow=2)


p <- ggplot(A, aes(x=DLM, y=Flexibility, color=Order, shape=Order)) +
  geom_point(size=3) + 
  geom_text(aes(label=paste(Language)), vjust=-1.5,position=position_dodge(.9),size=5) +
  labs(x="DLM") +
  labs(y="Flexibility") +
  labs(fill="Ordering")+
  scale_x_continuous(limits=c(-0.25, 2.25)) +
  scale_y_continuous(limits=c(0,1.08), breaks=seq(0,1.08,by=0.25)) +
  scale_color_manual(values=c("#293352")) + #, "#E69F00", "#56B4E9")) + 
  theme(legend.position="top",
        text = element_text(size=18)) 

p

ggsave("A-flexibility.pdf", p)


########################

BC <- subset(data, Group %in% c('B', 'C'))

BC_post <- subset(BC, Group %in% c('B'))
BC_pre <- subset(BC, Group %in% c('C'))

BC_post_dlm <- ggplot(BC_post, aes(Language, DLM)) + 
  #  geom_text(aes(label=paste(Language)), vjust=-3,position=position_dodge(.9),size=10) +
  geom_point(color="steelblue", size=4) +
  geom_errorbar(aes(ymax = DLM_CI975, ymin = DLM_CI25), width=.1, position=position_dodge(.9), color="steelblue") +
  labs(x="") +
  labs(y="DLM") +
  scale_y_continuous(limits=c(-0.5,2.5)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('BC_post_dlm.pdf', BC_post_dlm)

BC_post_flexibility <- ggplot(BC_post, aes(Language, Flexibility)) + 
  #  geom_text(aes(label=paste(Language)), hjust=-.5, vjust=-2.3,position=position_dodge(.9),size=8) +
  geom_point(color="#52854C", size=4) +
  geom_errorbar(aes(ymax = Flexibility_CI975, ymin = Flexibility_CI25), width=.1, position=position_dodge(.9), color="#52854C") +
  labs(x="") +
  labs(y="Flexibility") +
  scale_y_continuous(limits=c(0.,1.1)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('BC_post_flexibility.pdf', BC_post_flexibility)


BC_pre_dlm <- ggplot(BC_pre, aes(Language, DLM)) + 
  #  geom_text(aes(label=paste(Language)), vjust=-3,position=position_dodge(.9),size=10) +
  geom_point(color="#9999CC", size=4) +
  geom_errorbar(aes(ymax = DLM_CI975, ymin = DLM_CI25), width=.1, position=position_dodge(.9), color="#9999CC") +
  labs(x="") +
  labs(y="DLM") +
  scale_y_continuous(limits=c(-0.5,2.5)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('BC_pre_dlm.pdf', BC_pre_dlm)

BC_pre_flexibility <- ggplot(BC_pre, aes(Language, Flexibility)) + 
  #  geom_text(aes(label=paste(Language)), hjust=-.5, vjust=-2.3,position=position_dodge(.9),size=8) +
  geom_point(color="#00AFBB", size=4) +
  geom_errorbar(aes(ymax = Flexibility_CI975, ymin = Flexibility_CI25), width=.1, position=position_dodge(.9), color="#00AFBB") +
  labs(x="") +
  labs(y="Flexibility") + 
  scale_y_continuous(limits=c(0.,1.1)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('BC_pre_flexibility.pdf', BC_pre_flexibility)


p <- ggplot(BC, aes(x=DLM, y=Flexibility, color=Order, shape=Order)) +
  geom_point(size=3) + 
  geom_text(aes(label=paste(Language)), vjust=-1.5,position=position_dodge(.9),size=5) +
  labs(x="DLM") +
  labs(y="Flexibility") +
  labs(fill="Ordering")+
  scale_x_continuous(limits=c(-0.25, 2.25)) +
  scale_y_continuous(limits=c(0,1.08), breaks=seq(0,1.08,by=0.25)) +
  scale_color_manual(values=c("#293352", "#52854C")) + 
  theme(legend.position="top",
        text = element_text(size=18))

p


ggsave("BC-flexibility.pdf", p)


#########################3

D <- subset(data, Group %in% c('D'))

D$Language <- factor(D$Language,levels=c("Afrikaans", "Persian", "Urdu", "Hindi", "Japanese")) #, 'pronominality'))

D_dlm <- ggplot(D, aes(Language, DLM)) + 
  #  geom_text(aes(label=paste(Language)), vjust=-3,position=position_dodge(.9),size=10) +
  geom_point(color="#9999CC", size=4) +
  geom_errorbar(aes(ymax = DLM_CI975, ymin = DLM_CI25), width=.1, position=position_dodge(.9), color="#9999CC") +
  labs(x="") +
  labs(y="DLM") +
  scale_y_continuous(limits=c(-0.5,2.5)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('D_dlm.pdf', D_dlm)

D_flexibility <- ggplot(D, aes(Language, Flexibility)) + 
  #  geom_text(aes(label=paste(Language)), hjust=-.5, vjust=-2.3,position=position_dodge(.9),size=8) +
  geom_point(color="#00AFBB", size=4) +
  geom_errorbar(aes(ymax = Flexibility_CI975, ymin = Flexibility_CI25), width=.1, position=position_dodge(.9), color="#00AFBB") +
  labs(x="") +
  labs(y="Flexibility") + 
  scale_y_continuous(limits=c(0.,1.1)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=30),
        axis.title.y=element_text(size=35),
        axis.text.y=element_text(size=30),
        legend.position="top") +
  coord_flip()

ggsave('D_flexibility.pdf', D_flexibility)


p <- ggplot(D, aes(x=DLM, y=Flexibility, color=Order)) +
  geom_point(aes(shape=Order), size=3) +
  geom_text(aes(label=paste(Language)), vjust=-1.5,position=position_dodge(.9),size=5) +
  labs(x="DLM") +
  labs(y="Flexibility") +
  labs(fill="")+
  scale_x_continuous(limits=c(-0.25, 2.25)) +
  scale_y_continuous(limits=c(0,1.08), breaks=seq(0,1.08,by=0.25)) +
  scale_color_manual(values=c("#52854C")) + 
  scale_shape_manual(values=c(17))+
  theme(legend.position="top",
        text = element_text(size=18))

p

ggsave("D-flexibility.pdf", p)

######## Plotting model coefficient ######

data<-read.csv(file="preposition-postverbal.csv",header=T,sep=",")

data = subset(data, !(Parameters %in% c('Accuracy', 'NP_defiteness', 'PP_defiteness', 'NP_pronominality', 'PP_pronominality')))

data$Parameters<-factor(data$Parameters,levels=c("Len", "Arg_status"))

colnames(data)[which(names(data) == "Parameters")] <- "Factor"

levels(data$Factor)[1] <- 'Dependency length'
levels(data$Factor)[2] <- 'Argument status'

p<-ggplot(data, aes(x=Factor,y=Mean,color=Factor)) + 
  geom_errorbar(aes(ymax = CI975, ymin = CI25), width=.1, position=position_dodge(.9)) +
#  geom_line() +
  geom_point() +
#  geom_text(aes(label=Parameters), vjust=-1,position=position_dodge(.9),size=2.5)+ 
#  labs(x="Factors")+labs(fill='Factor')+
#  scale_y_continuous(limits=c(0.,1.27)) + 
  labs(y="Coefficient") +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1"))+
  facet_wrap(~Language,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  theme(legend.position="top")

p

ggsave("preposition-postverbal-coeff.pdf", p)


data<-read.csv(file="preposition-preverbal.csv",header=T,sep=",")

data = subset(data, !(Parameters %in% c('Accuracy', 'NP_defiteness', 'PP_defiteness', 'NP_pronominality', 'PP_pronominality')))

data$Parameters<-factor(data$Parameters,levels=c("Len", "Arg_status"))

colnames(data)[which(names(data) == "Parameters")] <- "Factor"

levels(data$Factor)[1] <- 'Dependency length'
levels(data$Factor)[2] <- 'Argument status'

p<-ggplot(data, aes(x=Factor,y=Mean,color=Factor)) + 
  geom_errorbar(aes(ymax = CI975, ymin = CI25), width=.1, position=position_dodge(.9)) +
  #  geom_line() +
  geom_point() +
  #  geom_text(aes(label=Parameters), vjust=-1,position=position_dodge(.9),size=2.5)+ 
  #  labs(x="Factors")+labs(fill='Factor')+
  #  scale_y_continuous(limits=c(0.,1.27)) + 
  labs(y="Coefficient") +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1"))+
  facet_wrap(~Language,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  theme(legend.position="top")

p

ggsave("preposition-preverbal-coeff.pdf", p, width = 3, height = 5)


data<-read.csv(file="postposition-preverbal.csv",header=T,sep=",")

data = subset(data, !(Parameters %in% c('Accuracy', 'NP_defiteness', 'PP_defiteness', 'NP_pronominality', 'PP_pronominality')))

data$Language<-factor(data$Language,levels=c("Japanese", "Hindi"))

data$Parameters<-factor(data$Parameters,levels=c("Len", "Arg_status"))

colnames(data)[which(names(data) == "Parameters")] <- "Factor"

levels(data$Factor)[1] <- 'Dependency length'
levels(data$Factor)[2] <- 'Argument status'

p<-ggplot(data, aes(x=Factor,y=Mean,color=Factor)) + 
  geom_errorbar(aes(ymax = CI975, ymin = CI25), width=.1, position=position_dodge(.9)) +
  #  geom_line() +
  geom_point() +
  #  geom_text(aes(label=Parameters), vjust=-1,position=position_dodge(.9),size=2.5)+ 
  #  labs(x="Factors")+labs(fill='Factor')+
  #  scale_y_continuous(limits=c(0.,1.27)) + 
  labs(y="Coefficient") +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1"))+
  facet_wrap(~Language,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  theme(legend.position="top")

p

ggsave("postposition-preverbal-coeff.pdf", p, width = 3, height = 5)


data<-read.csv(file="preposition-mix.csv",header=T,sep=",")

data = subset(data, !(Parameters %in% c('Accuracy', 'NP_defiteness', 'PP_defiteness', 'NP_pronominality', 'PP_pronominality')))

data$Parameters<-factor(data$Parameters,levels=c("Len", "Arg_status"))

colnames(data)[which(names(data) == "Parameters")] <- "Factor"

levels(data$Factor)[1] <- 'Dependency length'
levels(data$Factor)[2] <- 'Argument status'

data1 <- subset(data, Adposition == 'postverbal')
data2 <- subset(data, Adposition == 'preverbal')

p1<-ggplot(data1, aes(x=Factor,y=Mean,color=Factor)) + 
  geom_errorbar(aes(ymax = CI975, ymin = CI25), width=.1, position=position_dodge(.9)) +
  #  geom_line() +
  geom_point() +
  #  geom_text(aes(label=Parameters), vjust=-1,position=position_dodge(.9),size=2.5)+ 
  #  labs(x="Factors")+labs(fill='Factor')+
  #  scale_y_continuous(limits=c(0.,1.27)) + 
  labs(y="Coefficient") +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1"))+
  facet_wrap(~Language,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) + 
  theme(legend.position="top")

ggsave("preposition-mix-coeff1.pdf")

p2<-ggplot(data2, aes(x=Factor,y=Mean,color=Factor)) + 
  geom_errorbar(aes(ymax = CI975, ymin = CI25), width=.1, position=position_dodge(.9)) +
  #  geom_line() +
  geom_point() +
  #  geom_text(aes(label=Parameters), vjust=-1,position=position_dodge(.9),size=2.5)+ 
  #  labs(x="Factors")+labs(fill='Factor')+
  #  scale_y_continuous(limits=c(0.,1.27)) + 
  labs(y="Coefficient") +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1"))+
  facet_wrap(~Language,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  theme(legend.position="none")

ggsave("preposition-mix-coeff2.pdf")

data<-read.csv(file="all-model-output.csv",header=T,sep=",")

data<-read.csv(file="postposition-mix.csv",header=T,sep=",")

data = subset(data, !(Parameters %in% c('Accuracy', 'NP_defiteness', 'PP_defiteness', 'NP_pronominality', 'PP_pronominality')))

data$Parameters<-factor(data$Parameters,levels=c("Len", "Arg_status"))

colnames(data)[which(names(data) == "Parameters")] <- "Factor"

levels(data$Factor)[1] <- 'Dependency length'
levels(data$Factor)[2] <- 'Argument status'


data1 <- subset(data, Adposition == 'postverbal')
data2 <- subset(data, Adposition == 'preverbal')

p<-ggplot(data, aes(x=Factor,y=Mean,color=Factor)) + 
  geom_errorbar(aes(ymax = CI975, ymin = CI25), width=.1, position=position_dodge(.9)) +
  #  geom_line() +
  geom_point() +
  #  geom_text(aes(label=Parameters), vjust=-1,position=position_dodge(.9),size=2.5)+ 
  #  labs(x="Factors")+labs(fill='Factor')+
  #  scale_y_continuous(limits=c(0.,1.27)) + 
  labs(y="Coefficient") +
  scale_color_manual(values=wes_palette(n=2, name="IsleofDogs1"))+
  facet_wrap(~Position,ncol=6)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +
  theme(legend.position="top")

ggsave("postposition-mix-coeff.pdf", p, width = 3, height = 5)

##########################################################################

options (contrasts = rep("contr.sum", 2))

data <- read.csv(file="English-preposition-postverbal-data.csv",header=T,sep=",")
data$Order <- as.factor(data$Order)
data$Len <- as.numeric(data$Len)
data$Arg_status <- as.numeric(data$Arg_status)
data$Pronominality <- as.numeric(data$Pronominality)

m1 = glm(Order~ Len + Arg_status + Pronominality , 
           data=data, 
           family='binomial')

m2 = glmer(Order~ Len + Arg_status + Pronominality + (1 | Verb) , 
           data=data, 
           family='binomial', 
           control=glmerControl(optimizer="bobyqa"))

num1 = nrow(data)
num2 = nrow(data) / 2

se <- function(x){sd(x)/sqrt(length(x))}

test_num = round(num1/10, digit=0)
train_num = num1 - test_num

acc = rep(10000,0)
l = a = p = rep(10000,0)

for (i in 1:10000){
  use = sample(1:num1, test_num,replace=FALSE)
  
  train = data[-which(1:nrow(data) %in% use),]
  
  model = glmer(Order~ Len + Arg_status + Pronominality + (1 | Verb) , 
                data=train, 
                family='binomial', 
                control=glmerControl(optimizer="bobyqa"))
  
  l[i] = coef(model)[2]
  a[i] = coef(model)[3]
  p[i] = coef(model)[4]
}


round(mean(acc) * 100,2)
se(acc) * 100

round(mean(l),2)
se(nl)
round(mean(a),2)
se(pl)
round(mean(la),2)
se(as)

########## Cross-validating #########

##### For model including random intercept ###########

k = 20
sep = round(nrow(data) / k)
s = 1
e = sep

se <- function(x){sd(x)/sqrt(length(x))}

l = a = p = rep(20,0)

#prediction = rep(nrow(filter_data),0)
all_pred <- data.frame()

for (i in 1 : k) {
  test_index = seq(s, e, 1)
  train_index<-setdiff(1:nrow(data),test_index)
  
  test <- data[test_index, ]
  train <- data[train_index, ]
  
  mod <- glmer(Order~ Len + Arg_status + Pronominality + (1 | Verb), 
               data=train,
               family='binomial', 
               control=glmerControl(optimizer="bobyqa"))
  
  l[i] = summary(mod)$coefficient[2]
  a[i] = summary(mod)$coefficient[3]
  p[i] = summary(mod)$coefficient[4]
  
  s = s + sep
  e = e + sep
}

l_mean = round(mean(l), 2)
l_25 = l_mean - round(se(l), 2)
l_975 = l_mean + round(se(l), 2)

a_mean = round(mean(a), 2)
a_25 = a_mean - round(se(a), 2)
a_975 = a_mean + round(se(a), 2)

p_mean = round(mean(p), 2)
p_25 = p_mean - round(se(p), 2)
p_975 = p_mean + round(se(p), 2)

output <- file("English-preposition-postverbal-output.txt")

writeLines(c("Len", l_mean, l_25, l_975,  
             "Arg_status", a_mean, a_25, a_975,
           "Pronominality", p_mean, p_25, p_975), output)
close(output)
