
####SETUP####

#LIBRERIAS 

library(tidyverse) 
library(dplyr) 
library(readr) 
library(readxl)  
library(haven)  
library(ggplot2) 
library(stringr)
library("writexl")
library(lubridate)
library(data.table)
library(ggthemes)
library(knitr)
library(viridis)
library(MASS)
library(DHARMa)
library(ggpubr)
library(lme4)
library(gmodels)
library(openxlsx)
library(outreg)
library(Hmisc)
library("ggrepel")
library(ggrepel)

#DIRECTORIO 
#setwd("C:/M")

#SEMILLA 
set.seed(1234) 

#CLEAR ALL 

rm(list=ls())

#### Primera Parte: Limpieza y base de datos ####

bbdd <- read_csv("C:/Users/ADMIN/Desktop/shp_utt_pointing_dataOCT3.csv")

#bbdd$speaker.role[bbdd$speaker.name == "ALN"] = "OC"

# empty as NA
bbdd[bbdd == ""] = NA

# contar palabras
nwords = function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}

# aplicando la funcion contar palabras
bbdd$palabras = nwords(bbdd$utterance)

# distinción entre VOC y Utt
bbdd$utterance_lenght = NA
bbdd$utterance_lenght[bbdd$palabras == 1] = 1
bbdd$utterance_lenght[bbdd$palabras >= 2] = 2
bbdd$utterance_lenght[bbdd$utterance == "VOC"] = 0
bbdd$utterance_lenght[bbdd$utterance == "NO VOC"] = 0
bbdd$utterance_lenght[bbdd$comment == "VOC"] = 0
bbdd$utterance_lenght[bbdd$comment == "NO VOC"] = 0
bbdd$utterance_lenght[is.na(bbdd$utterance) == TRUE] = 0

unique(bbdd$utterance_lenght)


bbdd$palabras[bbdd$utterance == "VOC"] = 0
bbdd$palabras[bbdd$utterance == "NO VOC"] = 0
bbdd$palabras[bbdd$comment == "VOC"] = 0
bbdd$palabras[bbdd$comment == "NO VOC"] = 0
bbdd$palabras[is.na(bbdd$utterance) == TRUE] = 0

unique(bbdd$palabras)

# crear columna de deictics
spatial_deictics_spa_shp = c("arribita", "aquito", "aquita", "allacito", "aquí", "ahí", "ay", "allí", "allá", "acá", "aqui", "alla", "ahi", "alli", "aca", "abajo", "debajo", "arriba", "atrás", "atras", "debajo", "detrás", "detras", "cerca", "lejos","neri", "nino", "jain", "hain", "hao", "jao", "jainoa", "hainoa", "jainoan", "hainoan", "oli", "ono", "jainoxon", "jainohon", "jaino", "haino", "bajana", "nama", "naman", "tename", "chiponki", "manan", "reboki", "chinabi", "pekao", "chonbi", "bochiki", "inan", "ochoma", "ocho", "ori")


bbdd$deictic = rowSums(sapply(spatial_deictics_spa_shp, function(p) grepl(p, bbdd$utterance, ignore.case = TRUE))) > 0
# cuántos deictics hay
total = sum(bbdd$deictic, na.rm = TRUE)

#quitar variable palabras
#bbdd$palabras = NULL

#deictic como dummy
bbdd$deictic = as.numeric(bbdd$deictic)

## open age files
edades = read_excel("C:/Users/ADMIN/Desktop/edades por file.xlsx")

##convert all into months
edades = edades %>%
  separate(Age, into = c("ao", "meses"), sep = ";")


edades$meses = as.numeric(edades$meses)
edades$ao = as.numeric(edades$ao)
edades$months = edades$ao * 12 + edades$meses
edades$ao = NULL
edades$meses  = NULL


##change varnames

names(edades) = c("file_name", "months")

#####Limpieza de los filenames

bbdd$file_clean = sub("-[^-]*$", "", bbdd$file_name)

edades$file_clean = sub("-[^-]*$", "", edades$file_name)

unique(edades$file_clean)

edades = edades[-76,]

#####union de archivos de meses y de utterances
unique(edades$file_clean)

edades_unique = edades[!duplicated(edades$file_clean), ]

edades_unique$file_name = NULL

bbdd$file_name = NULL

data = left_join(bbdd, edades_unique, by = "file_clean")

data$utttype = "Utterance"
data$utttype[data$utterance == "VOC"] = "Voc"
data$utttype[data$utterance == "NO VOC"] = "No Voc"

# clarify  points (ya no right-left distinction)

data$pointt =  NA
data$pointt = data$pointing
data$pointt[data$pointing == "HaPR"] =  "HaP"
data$pointt[data$pointing == "HaPL"] =  "HaP"
data$pointt[data$pointing == "FPR"] =  "FP"
data$pointt[data$pointing == "FRP"] =  "FP"
data$pointt[data$pointing == "FPL"] =  "FP"
data$pointt[data$pointing == "OPL"] =  "OP"
data$pointt[data$pointing == "OPR"] =  "OP"

#write.csv(data, file = "temp/data.csv")
colnames(bbdd) <- gsub(" ", ".", colnames(bbdd))
data1=data %>% 
  group_by(file_clean, short_name, months, speaker.role) %>% 
  summarise(n=n()) %>% 
  as.data.frame

unique(data$pointing)

#subset -------------- solo pointing observations
subset = data[is.na(data$pointing) == FALSE,]

#View(subset)

#summarizing pointing data --> para hacer la regresión necesitamos el conteo de variables solamente
subset1= subset %>% 
  group_by(file_clean, short_name, months, speaker.role) %>% 
  summarise(npoints=n(), ndeictics = sum(deictic)) %>% 
  as.data.frame
total2 = sum(subset$deictic, na.rm = TRUE)

names(subset1)

# subset para la regresión incluyendo los 2 age groups y creando Tc vs SS groups
subsetreg = subset1 %>%
  mutate(agegroup = case_when(months  <= 17 ~ 1, months  >= 18 ~ 2)) %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "CC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  as.data.frame()


#rate de pointing por fecha
# por target child (speaker role == TC) ^ 
# por otros niños (speaker role == OC)
# por adultos (speaker role == OA)
# por padres (sprol == MOT | FAT)
# por todos en general ( -TC)

#diferentes cortes de edad
#presentar el esqueleto
#estadisticas descriptivas
#rate de pointing por edades
#rate por tipo declarativo
#por edades
#rate de palabras por minuto vs rate de pointing

color_pal3 = c("#FDE725FF", "#3a5e8cFF","#440154FF")
color_pal = c("#440154FF", "#35B779FF", "#3a5e8cFF", "#FDE725FF")
color_pal5 = c("#440154FF","#3a5e8cFF", "#35B779FF", "#7ad151", "#FDE725FF")
color_pal6 = c("#440154FF","#3a5e8cFF", "#35B779FF", "#7ad151", "#FDE725FF", "orange")
color_pal7 = c("#440154FF","#3a5e8cFF", "#35B779FF", "#7ad151", "#FDE725FF", "orange", "grey")




####Tablas####

#posibles age groups 12-17 y 19 -27

# Tabla 1
#determinar la cantidad de surrounding speakers

unique(data$file_clean)
unique(data$speaker.name)

qpeople = data %>%
  filter(utttype == "Utterance") %>%
  filter(!speaker.role == "UNCL", !speaker.role == "TC") %>% #decidimos no incluir UNCLEAR para saber quien esta hablando
  mutate(speaker.role = case_when(speaker.role == "OA" ~ "SA", speaker.role == "MOT" ~ "SA", speaker.role == "OC" ~ "SC")) %>%
  mutate(agegroup = case_when(months  <= 17 ~ 1, months  >= 18 ~ 2)) %>%
  group_by(agegroup, file_clean, speaker.role, speaker.name) %>%
  summarise(a = NULL) %>%
  as.data.frame()

qqpeople = qpeople %>%
  group_by(agegroup, file_clean, speaker.role) %>%
  summarise(n = n())%>%
  as.data.frame()

tab1 = qqpeople %>%
  group_by(agegroup, speaker.role) %>%
  summarise(min = min(n), max = max(n), mean =mean(n))%>%
  as.data.frame()




#Tabla 3

tab3 = subset %>% 
  mutate(finger = ifelse(pointt == "FP", "FP", "OT")) %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(speaker.role, finger) %>% 
  as.data.frame()

tabb3 =  table(tab3$speaker.role, tab3$finger )


row_p3 <- prop.table(tabb3, margin = 1) * 100

chisq.test(tabb3)


#Tabla 4

#statement DECL
#command IMP

tab4 = subset %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(speechact = case_when(speechact == "ExpDecl" ~ "Statement", speechact == "InfDecl" ~ "Statement", speechact == "Imp" ~ "Command")) %>%
  filter(!speechact %in% NA) %>% 
  filter(speechact == "Statement" | speechact == "Command") %>% 
  group_by(speaker.role, speechact) %>% 
  as.data.frame()

tabb4 = table(tab4$speaker.role, tab4$speechact )
row_p4 <- prop.table(tabb4, margin = 1) * 100

chisq.test(tabb4)



#Tabla 5
#Comments vs Identifications

tab5 = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  filter(!infotype %in% NA) %>% 
  filter(!infotype == "UNCL")%>%
  filter(!infotype == "UNC")%>%
  group_by(speaker.role, infotype) %>%
  as.data.frame()

tabb5 = table(tab5$speaker.role, tab5$infotype )

row_p5 <- prop.table(tabb5, margin = 1) * 100

chisq.test(tabb5)


#Tabla 6
#Comments vs Id in one word


tab6 = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  filter(utterance_lenght == 1)%>%
  filter(!infotype %in% NA) %>% 
  filter(!infotype == "UNCL")%>%
  filter(!infotype == "UNC")%>%
  group_by(speaker.role, infotype) %>%
  as.data.frame()

tabb6 = table(tab6$speaker.role, tab6$infotype )

row_p6 <- prop.table(tabb6, margin = 1) * 100

chisq.test(tabb6)


#Tab multi word vs one word

tabword = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  filter(!infotype %in% NA) %>% 
  filter(!infotype == "UNCL")%>%
  filter(!infotype == "UNC")%>%
  group_by(utterance_lenght, infotype) %>%
  as.data.frame()
tabbword = table(tabword$utterance_lenght, tabword$infotype )
row_pword <- prop.table(tabbword, margin = 1) * 100


chisq.test(tabbword)





#Table Figure 5
tabfig5 = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA",  speaker.role == "OC" ~ "CC")) %>%
  mutate(utterance_or_not = ifelse(utttype == "Utterance", "utterance", "no utterance")) %>% 
  group_by(utterance_or_not, speaker.role) %>%
  as.data.frame()


tabbfig5 = table(tabfig5$speaker.role, tabfig5$utterance_or_not )

row_pfig5 <- prop.table(tabbfig5, margin = 1) * 100

chisq.test(tabbfig5)



#Table Figure 6

tabfig6 = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA",  speaker.role == "OC" ~ "CC")) %>%
  group_by(utttype, speaker.role) %>%
  as.data.frame()


tabbfig6 = table(tabfig6$speaker.role, tabfig6$utttype )

row_pfig6 <- prop.table(tabbfig6, margin = 1) * 100

chisq.test(tabbfig6)


####Figuras####
subset$speechact[subset$speechact ==  "imp"] = "Imp"
pdf("output/Figuresfinal.pdf", width = 10, height = 10)


# plot FIGURE 1



subset1 %>% 
  ggplot(aes(x=as.factor(months), y=npoints,  fill = speaker.role)) + 
  geom_point(shape=15, size=0) + 
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  facet_wrap(~short_name)+
  theme(text=element_text(size=20), legend.position ="bottom")+
  labs(x="Age (months)",
       y="Number of points per session",
       fill="Speaker group") +
  scale_fill_manual(values = color_pal)


# plot number of utterances -- FIGURE 2A
# just utterances
numbers_of_pointing %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  ggplot(aes(x=as.factor(months), y=utterances, fill=speaker.role)) + 
  geom_point(shape=15, size=0) + 
  geom_bar(stat = "identity", position = "dodge")+
  theme_hc(base_size=20) +
  labs(x="Age (months)",
       y="Number of utterances per session",
       fill="Speaker role") +
  scale_fill_manual(values = color_pal3)

#2 proportion of points/ utterance
##Figure 2B


# number of points per session
nume_points = subset %>% 
  group_by(months, short_name, file_clean, speaker.role) %>% 
  summarise(num_points = n()) %>% 
  # create joining variable for join with utterance table
  mutate(joining_var = paste(file_clean, speaker.role, sep='_')) %>% 
  dplyr::select(-file_clean) %>%  
  as.data.frame


# number of utterances per session
nume_utterances = data %>% 
  group_by(file_clean, speaker.role) %>% 
  summarise(utterances = n()) %>% 
  mutate(joining_var = paste(file_clean, speaker.role, sep='_')) %>% 
  ungroup %>% 
  dplyr::select(-file_clean, speaker.role) %>% 
  as.data.frame

# combine the two para hacer el rate
numbers_of_pointing = nume_points %>% 
  left_join(., nume_utterances) %>% 
  dplyr::select(-joining_var)

# proportions of points to utterances  -- FIGURE 2B
numbers_of_pointing %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(prop_pointed_utterances = num_points/utterances) %>% 
  ggplot(aes(x=months, y=prop_pointed_utterances, color=speaker.role)) + 
  #facet_wrap(~short_name) +
  geom_point(size=4) + 
  geom_smooth() +
  theme_hc(base_size=20) +
  labs(x="Age (months)", 
       y="Proportion of points/utterances", 
       color="Speaker role") #+
#scale_color_manual(values = c("TC" = "#FDE725FF", "SS" = "#3a5e8cFF", "MOT" = "#440154FF"))



#3 proportion of finger point

subset %>% 
  mutate(finger = ifelse(pointt == "FP", "FP", "OT")) %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, short_name, speaker.role, finger) %>% 
  summarise(finger_vs_other_points=n()) %>% 
  ggplot(aes(x=as.factor(months), y=finger_vs_other_points, fill=finger))+
  geom_bar(position='fill', stat='identity') +
  facet_wrap(~short_name+speaker.role, scales="free_x", ncol = 3, nrow = 4) +  
  theme_hc(base_size=20) + 
  labs(x= 'Age', y = 'Proportion of finger points out of all points' , 
       fill='Point type') +
  scale_fill_manual(values = c("FP" = "coral", "OT" = "cadetblue"),
                    labels = c("FP" = "Finger point", "OT" = "Other point"))




#4pointing types

subset %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, short_name, speaker.role, pointt) %>% 
  summarise(point_type=n()) %>% 
  ggplot(aes(x=as.factor(months), y=point_type, fill=pointt)) +
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~short_name+speaker.role, ncol = 3, scales="free_x", nrow = 4) +  
  theme_hc(base_size=20) +
  scale_fill_manual(values = color_pal7) +
  labs(x= "Age (months)", y = "Proportion of types of points" , 
       fill="Point type") 


#5 proportion of points without an accompanying utterance (no utt=no voc, voc)

subset %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA",  speaker.role == "OC" ~ "CC")) %>%
  mutate(utterance_or_not = ifelse(utttype == "Utterance", "utterance", "no utterance")) %>% 
  group_by(months, short_name, speaker.role, utterance_or_not) %>% 
  summarise(utterance_accompanying_point=n()) %>% 
  ggplot(aes(x=as.factor(months), y=utterance_accompanying_point, fill=utterance_or_not)) +
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~short_name+speaker.role, scales="free_x", ncol = 3, nrow = 4) +  
  theme_hc(base_size=20) +
  labs(x = "Age (months)", 
       y= 'Proportion of points with/without an accompanying utterance', 
       fill = '')+
  scale_fill_manual(values = c("no utterance" = "coral", "utterance" = "cadetblue"))

#6 proportion of points accompanied by utterances

subset %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, short_name, speaker.role, utttype) %>% 
  summarise(num_utterances=n()) %>% 
  ggplot(aes(x=as.factor(months), y=num_utterances, fill=utttype)) +
  geom_bar(position='fill', stat='identity') +
  facet_wrap(~short_name+speaker.role, scales="free_x", ncol = 3, nrow = 4) +  
  theme_hc(base_size=17) + 
  scale_fill_manual(values=c(  "#FDE725FF", "#3a5e8cFF","#440154FF"),  labels = c("silent", "utterance", "voc")) +
  labs(y='Proportion of points accompanied by utterances', 
       x= 'Age (months)', 
       fill="Utterance")
#7 proportion of different intentions


subset %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "SS", speaker.role == "MOT" ~ "MOT", speaker.role == "OC" ~ "SS")) %>%
  filter(!speechact %in% NA) %>%  
  group_by(months, short_name, speaker.role, speechact)%>% 
  summarise(num_utterances=n()) %>% 
  ggplot(aes(x=as.factor(months), y=num_utterances, fill=speechact)) +
  geom_bar(position='fill', stat='identity') +
  facet_wrap(~short_name+speaker.role, 
             scales="free_x", nrow = 4, ncol = 3) +  
  theme_hc(base_size=17) + 
  scale_fill_manual(values = color_pal6) +
  labs(y='Proportion of different intentions', x='Age (months)', fill="Intention")



#8 proportion of information

subset %>% 
  filter(!is.na(infotype)) %>%  
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, short_name, speaker.role, infotype) %>% 
  summarise(num_utterances=n()) %>% 
  ggplot(aes(x=as.factor(months), y=num_utterances, fill=infotype)) +
  geom_bar(position='fill', stat='identity') +
  facet_wrap(~speaker.role, scales="free_x", nrow = 4, ncol = 3) +  
  theme_hc(base_size=17) + 
  scale_fill_manual(values = color_pal6) +
  labs(y='Proportion of types of information', x='Age (months)',
       fill='Information')


#9 Information conveyed in one-word utterances by adults and target children

subset %>% 
  filter(utterance_lenght == 1 & !is.na(infotype)) %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA",  speaker.role == "OC" ~ "CC")) %>%
  group_by(months, short_name, speaker.role, infotype) %>% 
  summarise(points = n()) %>% 
  ggplot(aes(x=as.factor(months), y=points, fill=infotype)) +
  geom_bar(position='fill', stat='identity') + 
  facet_wrap(~short_name+speaker.role, scales="free_x", ncol = 3, nrow = 4) +  
  theme_hc(base_size=17) +
  scale_fill_manual(values = color_pal5) +
  labs(x = 'Age (months)', y= 'Proportion of one-word utterances ', fill = 'Information in speech')

#10A Deictics in utterances WITH points

subset %>% 
  filter(utttype == "Utterance") %>% 
  filter(!speaker.role == "UNCL")%>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(Deictic =ifelse(deictic==0, 'no', 'yes')) %>%
  group_by(short_name, months, 
           speaker.role, Deictic) %>% 
  summarise(deictics_yes_no=n()) %>% 
  ggplot(aes(x=as.factor(months), y=deictics_yes_no, fill=Deictic)) + 
  geom_bar(position='fill', stat = 'identity') +
  facet_wrap(~ short_name+speaker.role, scales="free_x", ncol = 3, nrow = 4) +
  theme_hc(base_size = 18) +
  labs(y="Proportion of point accompanying utterances using deictics",
       x="Age in months") +
  scale_fill_manual(values=c(  "coral", "cadetblue"))

#10B
data %>% 
  filter(utttype == "Utterance") %>% 
  filter(!speaker.role == "UNCL")%>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(Deictic =ifelse(deictic==0, 'no', 'yes')) %>%
  group_by(short_name, months, 
           speaker.role, Deictic) %>% 
  summarise(deictics_yes_no=n()) %>% 
  ggplot(aes(x=as.factor(months), y=deictics_yes_no, fill=Deictic)) + 
  geom_bar(position='fill', stat = 'identity') +
  facet_wrap(~ short_name+speaker.role, scales="free_x", ncol = 3, nrow = 4) +
  theme_hc(base_size = 18) +
  labs(y="Proportion of utterances using deictics",
       x="Age in months") +
  scale_fill_manual(values=c(  "coral", "cadetblue"))


dev.off()



####Preguntas####

pdf("output/Questionsnov.pdf", width = 10, height = 10)

#Question 1
#How does the rate and distribution of poiinting gestures differ across speaker roles and age groups?
# number of points per session


#sc
points1 = subset1 %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  as.data.frame()




stat_info1 = points1 %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(npoints, months, use = "complete.obs"),
    lm_coef = coef(lm(npoints ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(points1, aes(x = months, y = npoints, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info1,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "1. How does the rate and distribution of poiinting gestures differ across speaker roles and age groups?", 
       x = "age(months)", y = "npoints") +
  theme(legend.position = "bottom")



########
#Question 2
#Does the proportion of pointing gestures to spoken utterances change across development? 

nume_points = subset %>% 
  group_by(months, short_name, file_clean, speaker.role) %>% 
  summarise(num_points = n()) %>% 
  # create joining variable for join with utterance table
  mutate(joining_var = paste(file_clean, speaker.role, sep='_')) %>% 
  dplyr::select(-file_clean) %>%  
  as.data.frame


# number of utterances per session
nume_utterances = data %>% 
  group_by(file_clean, speaker.role) %>% 
  summarise(utterances = n()) %>% 
  mutate(joining_var = paste(file_clean, speaker.role, sep='_')) %>% 
  ungroup %>% 
  dplyr::select(-file_clean, speaker.role) %>% 
  as.data.frame

# combine the two para hacer el rate
numbers_of_pointing = nume_points %>% 
  left_join(., nume_utterances) %>% 
  dplyr::select(-joining_var)

numbers_of_pointing=numbers_of_pointing %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(prop_pointed_utterances = num_points/utterances) %>%
  as.data.frame()






#sc  


stat_info2 = numbers_of_pointing %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop_pointed_utterances, months, use = "complete.obs"),
    lm_coef = coef(lm(prop_pointed_utterances ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(numbers_of_pointing, aes(x = months, y = prop_pointed_utterances, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info2,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "2. Does the proportion of pointing gestures to spoken utterances change across development? ", 
       x = "age(months)", y = "pointing") +
  theme(legend.position = "bottom")


#######
#Question 3
#Does the number of surrounding speakers influence the rate of pointing gestures produced by target children and surrounding speakers in the Shipibo cultural settings?

tempspeakers = data %>%
  filter(utttype == "Utterance") %>%
  filter(!speaker.role == "UNCL", !speaker.role == "TC") %>% #decidimos no incluir UNCLEAR para saber quien esta hablando
  group_by(file_clean, speaker.name) %>%
  summarise(a=NULL) %>%
  as.data.frame()

nsurr_speakers = tempspeakers %>%
  group_by(file_clean)%>%
  summarise(surr_speakers_per_session = n()) %>%
  as.data.frame()

##correlation speakers and points

points_against_speakers = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(file_clean, short_name, speaker.role) %>% 
  dplyr::summarise(points_per_session=n()) %>% 
  left_join(., nsurr_speakers)




#sc


stat_info3 = points_against_speakers %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(points_per_session, surr_speakers_per_session, use = "complete.obs"),
    lm_coef = coef(lm(points_per_session ~ surr_speakers_per_session))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(points_against_speakers, aes(x = surr_speakers_per_session, y = points_per_session, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info3,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "3. Does the number of surrounding speakers influence the rate of pointing gestures produced by target children and surrounding speakers in the Shipibo cultural settings?", 
       x = "surr_speakers_per_session", y = "points_per_session") +
  theme(legend.position = "bottom")

########
#Question 4
#Is there a (cross-cultural and) developmental difference in the distribution of index finger points versus other types of pointing gestures among young children and their surrounding adults?


finger = subset %>%
  mutate(finger = ifelse(pointt == "FP", "FP", "OT")) %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, speaker.role, short_name) %>%
  summarise(propfinger = mean(pointt == "FP"))%>%
  ungroup()

stat_info4 = finger %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(propfinger, months, use = "complete.obs"),
    lm_coef = coef(lm(propfinger ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.15 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(finger, aes(x = months, y = propfinger, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info4,
    aes(
      x = Inf , y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "4 Is there a (cross-cultural and) developmental difference in the distribution of index finger points versus other types of pointing gestures among young children and their surrounding adults?", 
       x = "age(months)", y = "prop fingerpoint") +
  theme(legend.position = "bottom")


head = subset %>%
  mutate(finger = ifelse(pointt == "HeP", "HeP", "OT")) %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, speaker.role, short_name) %>%
  summarise(proportion = mean(pointt == "HeP"))%>%
  ungroup()

stat_info4b = head %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(proportion, months, use = "complete.obs"),
    lm_coef = coef(lm(proportion ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.15 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(head, aes(x = months, y = proportion, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info4b,
    aes(
      x = Inf , y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "4 Is there a (cross-cultural and) developmental difference in the distribution of head points versus other types of pointing gestures among young children and their surrounding adults?", 
       x = "age(months)", y = "prop headpoint") +
  theme(legend.position = "bottom")



hand = subset %>%
  mutate(finger = ifelse(pointt == "HaP", "HaP", "OT")) %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  group_by(months, speaker.role, short_name) %>%
  summarise(proportion = mean(pointt == "HaP"))%>%
  ungroup()
stat_info4c = hand %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(proportion, months, use = "complete.obs"),
    lm_coef = coef(lm(proportion ~ months))[2],
    y_pos = 1 - ((group) - 1)*0.15  ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(hand, aes(x = months, y = proportion, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info4c,
    aes(
      x = Inf , y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "4 Is there a (cross-cultural and) developmental difference in the distribution of hand points versus other types of pointing gestures among young children and their surrounding adults?", 
       x = "age(months)", y = "prop handpoint") +
  theme(legend.position = "bottom")


#########
#Question 5
#How does the use of silent points (points without accompanying vocalizations) vary across cultures, age groups, and individual speakers (children vs. adults) in the Shipibo setting? 


novoc = subset %>% 
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA",  speaker.role == "OC" ~ "CC")) %>%
  mutate(utterance_or_not = ifelse(utttype == "Utterance", "utterance", "no utterance")) %>% 
  group_by(months, short_name, speaker.role, utterance_or_not) %>% 
  summarise(utterance_accompanying_point=n()) %>% 
  as.data.frame()

stat_info5 = novoc %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(utterance_accompanying_point, months, use = "complete.obs"),
    lm_coef = coef(lm(utterance_accompanying_point ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.15 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(novoc, aes(x = months, y = utterance_accompanying_point, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info5,
    aes(
      x = Inf , y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "5.  How does the use of silent points (points without accompanying vocalizations) vary across cultures, age groups, and individual speakers (children vs. adults) in the Shipibo setting? ", 
       x = "age(months)", y = "prop infdecl") +
  theme(legend.position = "bottom")


#######
#Question 6
#How do children in the Shipibo cultural settings use vocalizations (e.g., babbling, words) accompanying points, particularly at younger ages? 


pointsutt = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  filter(utterance == "VOC")%>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(num_pointsvoc=n())%>%
  as.data.frame()




stat_info6 = pointsutt %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(num_pointsvoc, months, use = "complete.obs"),
    lm_coef = coef(lm(num_pointsvoc ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.15 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(pointsutt, aes(x = months, y = num_pointsvoc, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info6,
    aes(
      x = Inf , y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "6. How do children in the Shipibo cultural settings use vocalizations (e.g., babbling, words) accompanying points, particularly at younger ages?  ", 
       x = "age(months)", y = "Vocalizations") +
  theme(legend.position = "bottom")







########
#Question 7
#How do speech acts accompanying points (e.g., commands vs. statements) vary across age groups and between Shipibo adults and children? 


Infdecl = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  filter(!speechact %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop_infdecl = mean(speechact == "InfDecl"))%>%
  as.data.frame()



stat_info7 = Infdecl %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop_infdecl, months, use = "complete.obs"),
    lm_coef = coef(lm(prop_infdecl ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(Infdecl, aes(x = months, y = prop_infdecl, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info7,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "7 How do speech acts accompanying points (e.g., commands vs. statements) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "prop infdecl") +
  theme(legend.position = "bottom")

ExpDecl = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  filter(!speechact %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop = mean(speechact == "ExpDecl"))%>%
  as.data.frame()



stat_info7b = ExpDecl %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop, months, use = "complete.obs"),
    lm_coef = coef(lm(prop ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(ExpDecl, aes(x = months, y = prop, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info7b,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "7 How do speech acts accompanying points (e.g., commands vs. statements) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "Proportion Exp Declarative") +
  theme(legend.position = "bottom")


Decl = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(speechact = case_when(speechact == "InfDecl" ~ "Decl", speechact == "ExpDecl " ~ "Decl", speechact == "Imp" ~ "OT", speechact == "UNC" ~ "OT", speechact == "UNCL" ~ "OT", speechact == "Question" ~ "OT", speechact == "imp" ~ "OT")) %>%
  filter(!speechact %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop = mean(speechact == "Decl"))%>%
  as.data.frame()



stat_info7c = Decl %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop, months, use = "complete.obs"),
    lm_coef = coef(lm(prop ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(Decl, aes(x = months, y = prop, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info7c,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "7 How do speech acts accompanying points (e.g., commands vs. statements) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "Proportion Declarative") +
  theme(legend.position = "bottom")

IMP = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(speechact = case_when(speechact == "InfDecl" ~ "OT", speechact == "ExpDecl " ~ "OT", speechact == "Imp" ~ "Imp", speechact == "UNC" ~ "OT", speechact == "UNCL" ~ "OT", speechact == "Question" ~ "OT", speechact == "imp" ~ "Imp")) %>%
  filter(!speechact %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop = mean(speechact == "Imp"))%>%
  as.data.frame()



stat_info7d = IMP %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop, months, use = "complete.obs"),
    lm_coef = coef(lm(prop ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(IMP, aes(x = months, y = prop, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info7d,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "7 How do speech acts accompanying points (e.g., commands vs. statements) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "Proportion Imperative") +
  theme(legend.position = "bottom")

#######
#Question 8

#How does the type of information conveyed in point-accompanying utterances (e.g., attention-directing, identification, comment) vary across age groups and between Shipibo adults and children? 


attention = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(infotype = case_when(infotype == "Elaboration" ~ "OT", infotype == "Directing Attention" ~ "Directing Attention", infotype == "Identification" ~ "OT", infotype == "UNC" ~ "OT", infotype == "UNCL" ~ "OT")) %>%
  filter(!infotype %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop = mean(infotype == "Directing Attention"))%>%
  as.data.frame()



stat_info8a = attention %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop, months, use = "complete.obs"),
    lm_coef = coef(lm(prop ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(attention, aes(x = months, y = prop, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info8a,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "8. How does the type of information conveyed in point-accompanying utterances (e.g., attention-directing, identification, comment) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "Directing Attention") +
  theme(legend.position = "bottom")



identification = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(infotype = case_when(infotype == "Elaboration" ~ "OT", infotype == "Directing Attention" ~ "OT", infotype == "Identification" ~ "Identification", infotype == "UNC" ~ "OT", infotype == "UNCL" ~ "OT")) %>%
  filter(!infotype %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop = mean(infotype == "Identification"))%>%
  as.data.frame()



stat_info8b = identification %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop, months, use = "complete.obs"),
    lm_coef = coef(lm(prop ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(identification, aes(x = months, y = prop, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info8b,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "8. How does the type of information conveyed in point-accompanying utterances (e.g., attention-directing, identification, comment) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "identification") +
  theme(legend.position = "bottom")

Elaboration = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  mutate(infotype = case_when(infotype == "Elaboration" ~ "Elaboration", infotype == "Directing Attention" ~ "OT", infotype == "Identification" ~ "OT", infotype == "UNC" ~ "OT", infotype == "UNCL" ~ "OT")) %>%
  filter(!infotype %in% NA) %>%
  group_by(file_clean, speaker.role, short_name, months) %>%
  summarise(prop = mean(infotype == "Elaboration"))%>%
  as.data.frame()



stat_info8c = Elaboration %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop, months, use = "complete.obs"),
    lm_coef = coef(lm(prop ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(Elaboration, aes(x = months, y = prop, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info8c,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "8. How does the type of information conveyed in point-accompanying utterances (e.g., attention-directing, identification, comment) vary across age groups and between Shipibo adults and children? ", 
       x = "age(months)", y = "Elaboration") +
  theme(legend.position = "bottom")


######
#Question 9
#How do one-word and multi-word utterances differ in the way they accompany pointing gestures, specifically in conveying comments or identifications across Shipibo adults and children? 



oneword = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  
  group_by(file_clean, speaker.role, short_name, months) %>%
  
  summarise(prop_oneword = mean(utterance_lenght == 1))%>%
  
  as.data.frame()



stat_info9a = oneword %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop_oneword, months, use = "complete.obs"),
    lm_coef = coef(lm(prop_oneword ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(oneword, aes(x = months, y = prop_oneword, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info9a,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "9 How do one-word and multi-word utterances differ in the way they accompany pointing gestures, specifically in conveying comments or identifications across Shipibo adults and children?  ", 
       x = "age(months)", y = "prop one word utterances") +
  theme(legend.position = "bottom")

twoword = subset %>%
  mutate(speaker.role = case_when(speaker.role == "TC" ~ "TC", speaker.role == "TC " ~ "TC", speaker.role == "OA" ~ "AA", speaker.role == "MOT" ~ "AA", speaker.role == "OC" ~ "CC")) %>%
  
  group_by(file_clean, speaker.role, short_name, months) %>%
  
  summarise(prop_twoword = mean(utterance_lenght == 2))%>%
  
  as.data.frame()



stat_info9b = twoword %>%
  group_by(speaker.role, short_name) %>%
  mutate(group = case_when(speaker.role == "TC" ~ 1, speaker.role == "CC" ~ 2, speaker.role == "AA" ~ 3)) %>%
  summarise(
    cor = cor(prop_twoword, months, use = "complete.obs"),
    lm_coef = coef(lm(prop_twoword ~ months))[2],
    y_pos = 1 - ((group) - 1) * 0.2 ,
    .groups = "drop"
  )%>%
  as.data.frame()


ggplot(twoword, aes(x = months, y = prop_twoword, color = factor(speaker.role))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ short_name) +
  geom_text(
    data = stat_info9b,
    aes(
      x = Inf, y = y_pos,
      label = paste("Group", speaker.role, "\nr =", round(cor, 2), "\nslope =", round(lm_coef, 2)),
      color = factor(speaker.role)
    ),
    hjust = 1.1, vjust = 1.1,
    size = 3,
    inherit.aes = FALSE
  ) +
  theme_minimal() +
  labs(title = "9 How do one-word and multi-word utterances differ in the way they accompany pointing gestures, specifically in conveying comments or identifications across Shipibo adults and children?  ", 
       x = "age(months)", y = "prop two+ word utterances") +
  theme(legend.position = "bottom")


######
#Question 10
#How does the use of deictic expressions in utterances vary depending on whether they accompany pointing gestures, and what differences are observed across age groups and between Shipibo adults and children?

dev.off()




####Modelos####

# M0 -- null model (intercept only)
nbm_nulo = glm.nb(npoints ~  1, data=subsetreg)
summary(nbm_nulo)

# M1 -- speaker role
nbm_rol = glm.nb(npoints ~  speaker.role, data=subsetreg)
summary(nbm_rol)
# calculamos pR2 --> se crea uno por cada modelo
1 - nbm_rol$twologlik/nbm_nulo$twologlik  # usuaaaaaalmente, a partir de 0.2 está bien --> mientras más se acerca a 1, más explica --- el pseudoR2 nunca llegará a 1, porque es una variable construida; data discreta nunca va a llegar 1

## 2 x log-likelihood = la probabilidad de que el modelo se ajuste a la data, transformado por un logaritmo

# no significativo

# M2 -- agegroup
nbm_age = glm.nb(npoints ~  agegroup, data=subsetreg)
summary(nbm_age)
# pseudo R cuadrado (normal) = pR2    ---> entonces, como no es ajustado, podría estar inflado
1 - nbm_age$twologlik/nbm_nulo$twologlik

# no significativo

### AIC - qué tanto se acomoda el modelo a los datos  --> esto podría esconder overfitting
### R2  - qué tanto explica el modelo los datos  = la proporción de la varianza explicada (se refiere a la varianza de los puntos de datos que pueden ser explicados por nuestras variables independientes)

### mientras más variables, más R2 --> cualquier variable lo aumenta --> por eso vemos el R2 ajustado


# M3 -- speaker role + age group
nbm_agerol <- glm.nb(npoints ~  agegroup + speaker.role, data=subsetreg)
summary(nbm_agerol)
# pR2
1 - nbm_agerol$twologlik/nbm_nulo$twologlik

# no significativo

# M4 -- speaker role : agegroup interaction
nbm_agerol.int <- glm.nb(npoints ~  speaker.role*agegroup, data=subsetreg)
summary(nbm_agerol.int)
# pR2
1 - nbm_agerol.int$twologlik/nbm_nulo$twologlik

# no significativo




modelresults3 <- data.frame(
  Model = c("Model 0", "Model 1", "Model 2", "Model 3", "Model 4"),
  Variables = c("npoints ~  1", "npoints ~  speaker.role", 
                "npoints ~  agegroup", 
                "npoints ~  agegroup + speaker.role", 
                "npoints ~  speaker.role*agegroup"),
  TwologLikelihood = c(2 * as.numeric(logLik(nbm_nulo)),
                       2 * as.numeric(logLik(nbm_rol)),
                       2 * as.numeric(logLik(nbm_age)),
                       2 * as.numeric(logLik(nbm_agerol)),
                       2 * as.numeric(logLik(nbm_agerol.int))),
  pR2 = c(
    "-",
    1 - nbm_rol$twologlik/nbm_nulo$twologlik,
    1 - nbm_age$twologlik/nbm_nulo$twologlik,
    1 - nbm_agerol$twologlik/nbm_nulo$twologlik,
    1 - nbm_agerol.int$twologlik/nbm_nulo$twologlik),
  AIC = c(AIC(nbm_nulo), AIC(nbm_rol), AIC(nbm_age), AIC(nbm_agerol), AIC(nbm_agerol.int)))




write_xlsx(modelresults3, path = "output/modelresults3.xlsx")


summary(nbm_agerol)




simures = simulateResiduals(fittedModel = nbm_agerol)

plot(simures)
testDispersion(simures)
testZeroInflation(simures)
testQuantiles(simures)








