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


##### pairwise comparison #####

### postverbal ###

Wolof <- read.csv('Wolof-preposition-postverbal-data.csv', header = T, sep = ',')
Wolof$domain <- rep('postverbal',nrow(Wolof))
Wolof$language <- rep('Wolof',nrow(Wolof))
Wolof$order <- paste(Wolof$domain, Wolof$language)

Finnish <- read.csv('Finnish-postposition-postverbal-data.csv', header = T, sep = ',')
Finnish$domain <- rep('postverbal',nrow(Finnish))
Finnish$language <- rep('Finnish',nrow(Finnish))
Finnish$order <- paste(Finnish$domain, Finnish$language)

Irish <- read.csv('Irish-preposition-postverbal-data.csv', header = T, sep = ',')
Irish$domain <- rep('postverbal',nrow(Irish))
Irish$language <- rep('Irish',nrow(Irish))
Irish$order <- paste(Irish$domain, Irish$language)

Latvian <- read.csv('Latvian-preposition-postverbal-data.csv', header = T, sep = ',')
Latvian$domain <- rep('postverbal',nrow(Latvian))
Latvian$language <- rep('Latvian',nrow(Latvian))
Latvian$order <- paste(Latvian$domain, Latvian$language)

Indonesian <- read.csv('Indonesian-preposition-postverbal-data.csv', header = T, sep = ',')
Indonesian$domain <- rep('postverbal',nrow(Indonesian))
Indonesian$language <- rep('Indonesian',nrow(Indonesian))
Indonesian$order <- paste(Indonesian$domain, Indonesian$language)

Greek <- read.csv('Greek-preposition-postverbal-data.csv', header = T, sep = ',')
Greek$domain <- rep('postverbal',nrow(Greek))
Greek$language <- rep('Greek',nrow(Greek))
Greek$order <- paste(Greek$domain, Greek$language)

Bulgarian <- read.csv('Bulgarian-preposition-postverbal-data.csv', header = T, sep = ',')
Bulgarian$domain <- rep('postverbal',nrow(Bulgarian))
Bulgarian$language <- rep('Bulgarian',nrow(Bulgarian))
Bulgarian$order <- paste(Bulgarian$domain, Bulgarian$language)

Ukrainian <- read.csv('Ukrainian-preposition-postverbal-data.csv', header = T, sep = ',')
Ukrainian$domain <- rep('postverbal',nrow(Ukrainian))
Ukrainian$language <- rep('Ukrainian',nrow(Ukrainian))
Ukrainian$order <- paste(Ukrainian$domain, Ukrainian$language)

Serbian <- read.csv('Serbian-preposition-postverbal-data.csv', header = T, sep = ',')
Serbian$domain <- rep('postverbal',nrow(Serbian))
Serbian$language <- rep('Serbian',nrow(Serbian))
Serbian$order <- paste(Serbian$domain, Serbian$language)

Romanian <- read.csv('Romanian-preposition-postverbal-data.csv', header = T, sep = ',')
Romanian$domain <- rep('postverbal',nrow(Romanian))
Romanian$language <- rep('Romanian',nrow(Romanian))
Romanian$order <- paste(Romanian$domain, Romanian$language)

Portuguese <- read.csv('Portuguese-preposition-postverbal-data.csv', header = T, sep = ',')
Portuguese$domain <- rep('postverbal',nrow(Portuguese))
Portuguese$language <- rep('Portuguese',nrow(Portuguese))
Portuguese$order <- paste(Portuguese$domain, Portuguese$language)

Italian <- read.csv('Italian-preposition-postverbal-data.csv', header = T, sep = ',')
Italian$domain <- rep('postverbal',nrow(Italian))
Italian$language <- rep('Italian',nrow(Italian))
Italian$order <- paste(Italian$domain, Italian$language)

Galician <- read.csv('Galician-preposition-postverbal-data.csv', header = T, sep = ',')
Galician$domain <- rep('postverbal',nrow(Galician))
Galician$language <- rep('Galician',nrow(Galician))
Galician$order <- paste(Galician$domain, Galician$language)

French <- read.csv('French-preposition-postverbal-data.csv', header = T, sep = ',')
French$domain <- rep('postverbal',nrow(French))
French$language <- rep('French',nrow(French))
French$order <- paste(French$domain, French$language)

Swedish <- read.csv('Swedish-preposition-postverbal-data.csv', header = T, sep = ',')
Swedish$domain <- rep('postverbal',nrow(Swedish))
Swedish$language <- rep('Swedish',nrow(Swedish))
Swedish$order <- paste(Swedish$domain, Swedish$language)

Norwegian <- read.csv('Norwegian-preposition-postverbal-data.csv', header = T, sep = ',')
Norwegian$domain <- rep('postverbal',nrow(Norwegian))
Norwegian$language <- rep('Norwegian',nrow(Norwegian))
Norwegian$order <- paste(Norwegian$domain, Norwegian$language)

Danish <- read.csv('Danish-preposition-postverbal-data.csv', header = T, sep = ',')
Danish$domain <- rep('postverbal',nrow(Danish))
Danish$language <- rep('Danish',nrow(Danish))
Danish$order <- paste(Danish$domain, Danish$language)

English <- read.csv('English-preposition-postverbal-data.csv', header = T, sep = ',')
English$domain <- rep('postverbal',nrow(English))
English$language <- rep('English',nrow(English))
English$order <- paste(English$domain, English$language)

Hebrew <- read.csv('Hebrew-preposition-postverbal-data.csv', header = T, sep = ',')
Hebrew$domain <- rep('postverbal',nrow(Hebrew))
Hebrew$language <- rep('Hebrew',nrow(Hebrew))
Hebrew$order <- paste(Hebrew$domain, Hebrew$language)

Arabic <- read.csv('Arabic-preposition-postverbal-data.csv', header = T, sep = ',')
Arabic$domain <- rep('postverbal',nrow(Arabic))
Arabic$language <- rep('Arabic',nrow(Arabic))
Arabic$order <- paste(Arabic$domain, Arabic$language)

###### both #########

Spanish <- read.csv('Spanish-preposition-postverbal-data.csv', header = T, sep = ',')
Spanish$domain <- rep('postverbal',nrow(Spanish))
Spanish$language <- rep('Spanish',nrow(Spanish))
Spanish$order <- paste(Spanish$domain, Spanish$language)

Slovenian <- read.csv('Slovenian-preposition-postverbal-data.csv', header = T, sep = ',')
Slovenian$domain <- rep('postverbal',nrow(Slovenian))
Slovenian$language <- rep('Slovenian',nrow(Slovenian))
Slovenian$order <- paste(Slovenian$domain, Slovenian$language)

Slovak <- read.csv('Slovak-preposition-postverbal-data.csv', header = T, sep = ',')
Slovak$domain <- rep('postverbal',nrow(Slovak))
Slovak$language <- rep('Slovak',nrow(Slovak))
Slovak$order <- paste(Slovak$domain, Slovak$language)

Russian <- read.csv('Russian-preposition-postverbal-data.csv', header = T, sep = ',')
Russian$domain <- rep('postverbal',nrow(Russian))
Russian$language <- rep('Russian',nrow(Russian))
Russian$order <- paste(Russian$domain, Russian$language)

Polish <- read.csv('Polish-preposition-postverbal-data.csv', header = T, sep = ',')
Polish$domain <- rep('postverbal',nrow(Polish))
Polish$language <- rep('Polish',nrow(Polish))
Polish$order <- paste(Polish$domain, Polish$language)

German <- read.csv('German-preposition-postverbal-data.csv', header = T, sep = ',')
German$domain <- rep('postverbal',nrow(German))
German$language <- rep('German',nrow(German))
German$order <- paste(German$domain, German$language)

Estonian <- read.csv('Estonian-postposition-postverbal-data.csv', header = T, sep = ',')
Estonian$domain <- rep('postverbal',nrow(Estonian))
Estonian$language <- rep('Estonian',nrow(Estonian))
Estonian$order <- paste(Estonian$domain, Estonian$language)

Dutch <- read.csv('Dutch-preposition-postverbal-data.csv', header = T, sep = ',')
Dutch$domain <- rep('postverbal',nrow(Dutch))
Dutch$language <- rep('Dutch',nrow(Dutch))
Dutch$order <- paste(Dutch$domain, Dutch$language)

Czech <- read.csv('Czech-preposition-postverbal-data.csv', header = T, sep = ',')
Czech$domain <- rep('postverbal',nrow(Czech))
Czech$language <- rep('Czech',nrow(Czech))
Czech$order <- paste(Czech$domain, Czech$language)

Croatian <- read.csv('Croatian-preposition-postverbal-data.csv', header = T, sep = ',')
Croatian$domain <- rep('postverbal',nrow(Croatian))
Croatian$language <- rep('Croatian',nrow(Croatian))
Croatian$order <- paste(Croatian$domain, Croatian$language)

Catalan <- read.csv('Catalan-preposition-postverbal-data.csv', header = T, sep = ',')
Catalan$domain <- rep('postverbal',nrow(Catalan))
Catalan$language <- rep('Catalan',nrow(Catalan))
Catalan$order <- paste(Catalan$domain, Catalan$language)

Spanish_pre <- read.csv('Spanish-preposition-preverbal-data.csv', header = T, sep = ',')
Spanish_pre$domain <- rep('preverbal',nrow(Spanish_pre))
Spanish_pre$language <- rep('Spanish',nrow(Spanish_pre))
Spanish_pre$order <- paste(Spanish_pre$domain, Spanish_pre$language)

Slovenian_pre <- read.csv('Slovenian-preposition-preverbal-data.csv', header = T, sep = ',')
Slovenian_pre$domain <- rep('preverbal',nrow(Slovenian_pre))
Slovenian_pre$language <- rep('Slovenian',nrow(Slovenian_pre))
Slovenian_pre$order <- paste(Slovenian_pre$domain, Slovenian_pre$language)

Slovak_pre <- read.csv('Slovak-preposition-preverbal-data.csv', header = T, sep = ',')
Slovak_pre$domain <- rep('preverbal',nrow(Slovak_pre))
Slovak_pre$language <- rep('Slovak',nrow(Slovak_pre))
Slovak_pre$order <- paste(Slovak_pre$domain, Slovak_pre$language)

Russian_pre <- read.csv('Russian-preposition-preverbal-data.csv', header = T, sep = ',')
Russian_pre$domain <- rep('preverbal',nrow(Russian_pre))
Russian_pre$language <- rep('Russian',nrow(Russian_pre))
Russian_pre$order <- paste(Russian_pre$domain, Russian_pre$language)

Polish_pre <- read.csv('Polish-preposition-preverbal-data.csv', header = T, sep = ',')
Polish_pre$domain <- rep('preverbal',nrow(Polish_pre))
Polish_pre$language <- rep('Polish',nrow(Polish_pre))
Polish_pre$order <- paste(Polish_pre$domain, Polish_pre$language)

German_pre <- read.csv('German-preposition-preverbal-data.csv', header = T, sep = ',')
German_pre$domain <- rep('preverbal',nrow(German_pre))
German_pre$language <- rep('German',nrow(German_pre))
German_pre$order <- paste(German_pre$domain, German_pre$language)

Estonian_pre <- read.csv('Estonian-postposition-preverbal-data.csv', header = T, sep = ',')
Estonian_pre$domain <- rep('preverbal',nrow(Estonian_pre))
Estonian_pre$language <- rep('Estonian',nrow(Estonian_pre))
Estonian_pre$order <- paste(Estonian_pre$domain, Estonian_pre$language)

Dutch_pre <- read.csv('Dutch-preposition-preverbal-data.csv', header = T, sep = ',')
Dutch_pre$domain <- rep('preverbal',nrow(Dutch_pre))
Dutch_pre$language <- rep('Dutch',nrow(Dutch_pre))
Dutch_pre$order <- paste(Dutch_pre$domain, Dutch_pre$language)

Czech_pre <- read.csv('Czech-preposition-preverbal-data.csv', header = T, sep = ',')
Czech_pre$domain <- rep('preverbal',nrow(Czech_pre))
Czech_pre$language <- rep('Czech',nrow(Czech_pre))
Czech_pre$order <- paste(Czech_pre$domain, Czech_pre$language)

Croatian_pre <- read.csv('Croatian-preposition-preverbal-data.csv', header = T, sep = ',')
Croatian_pre$domain <- rep('preverbal',nrow(Croatian_pre))
Croatian_pre$language <- rep('Croatian',nrow(Croatian_pre))
Croatian_pre$order <- paste(Croatian_pre$domain, Croatian_pre$language)

Catalan_pre <- read.csv('Catalan-preposition-preverbal-data.csv', header = T, sep = ',')
Catalan_pre$domain <- rep('preverbal',nrow(Catalan_pre))
Catalan_pre$language <- rep('Catalan',nrow(Catalan_pre))
Catalan_pre$order <- paste(Catalan_pre$domain, Catalan_pre$language)

### preverbal ###

Japanese <- read.csv('Japanese-postposition-preverbal-data.csv', header = T, sep = ',')
Japanese$domain <- rep('preverbal',nrow(Japanese))
Japanese$language <- rep('Japanese',nrow(Japanese))
Japanese$order <- paste(Japanese$domain, Japanese$language)

Hindi <- read.csv('Hindi-postposition-preverbal-data.csv', header = T, sep = ',')
Hindi$domain <- rep('preverbal',nrow(Hindi))
Hindi$language <- rep('Hindi',nrow(Hindi))
Hindi$order <- paste(Hindi$domain, Hindi$language)

Urdu <- read.csv('Urdu-postposition-preverbal-data.csv', header = T, sep = ',')
Urdu$domain <- rep('preverbal',nrow(Urdu))
Urdu$language <- rep('Urdu',nrow(Urdu))
Urdu$order <- paste(Urdu$domain, Urdu$language)

Persian <- read.csv('Persian-preposition-preverbal-data.csv', header = T, sep = ',')
Persian$domain <- rep('preverbal',nrow(Persian))
Persian$language <- rep('Persian',nrow(Persian))
Persian$order <- paste(Persian$domain, Persian$language)

Afrikaans <- read.csv('Afrikaans-preposition-preverbal-data.csv', header = T, sep = ',')
Afrikaans$domain <- rep('preverbal',nrow(Afrikaans))
Afrikaans$language <- rep('Afrikaans',nrow(Afrikaans))
Afrikaans$order <- paste(Afrikaans$domain, Afrikaans$language)

########### Combine #####

data <- rbind(Wolof, Finnish, Irish, Latvian, Indonesian, Greek, Bulgarian, Ukrainian, Serbian,
              Romanian, Portuguese, Italian, Galician, French, Swedish, Norwegian, Danish,
              English, Hebrew, Arabic, Spanish, Spanish_pre, Slovenian, Slovenian_pre, Slovak, Slovak_pre,
              Russian, Russian_pre, Polish, Polish_pre, German, German_pre, Estonian, Estonian_pre,
              Dutch, Dutch_pre, Czech, Czech_pre, Croatian, Croatian_pre, Catalan, Catalan_pre,
              Japanese, Hindi, Urdu, Persian, Afrikaans)


m1 <- glm(Order ~ Len + Arg_status + Pronominality, # + (1|Verb),
            data = English, family = 'binomial')
m2 <- glm(Order ~ Len + Arg_status + Pronominality, # + (1|Verb),
            data = Japanese, family = 'binomial')

sample <- rbind(Spanish, Spanish_pre)
m <- glm(Order ~ (Len + Arg_status + Pronominality) * order, # + (1|Verb),
          data = sample, family = 'binomial')
summary(m)

# m4 <- glmer(Order ~ Len + Arg_status + Pronominality +  (Len|domain), # + (1|Verb),
#          data = data3, family = 'binomial')


mod <- brm(Order ~ (Len * Pronominality + Arg_status) * domain * language ,
           data = sample, 
           warmup = 400,
           iter = 2000,
           chains = 2,
           inits="0",
           prior=prior,
           #                family = "Gamma",
           control = list(adapt_delta = 0.9999, max_treedepth = 15),
           cores = 2)

mod <- brm(Order ~ Len + Pronominality + Arg_status + (Len + Pronominality + Arg_status|language) + (Len + Pronominality + Arg_status|domain), 
           data = sample, 
           warmup = 400,
           iter = 2000,
           chains = 2,
           inits="0",
           prior=prior,
           #                family = "Gamma",
           control = list(adapt_delta = 0.9999, max_treedepth = 15),
           cores = 2)

saveRDS(mod, 'all.mod')

data<-read.csv(file="corr.csv",header=T,sep=",")

#names(data)<-c('Language','Flexibility','DLM','Order','Group')

#data$Group[data$Group %in% c('C')] <- B


######### Testing relationship between DLM and flexibility #######

prior <- prior(student_t(3,0,8))

mod <- brm(DLM ~ Flexibility * Order + (1|Group), #  + (1|Order) + (1|Group),
           data = data, 
           warmup = 500,
           iter = 3000,
           chains = 2,
           inits="0",
           prior=prior,
           #                family = "Gamma",
           control = list(adapt_delta = 0.9999, max_treedepth = 15),
           cores = 2)

saveRDS(mod, 'all.mod')


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
