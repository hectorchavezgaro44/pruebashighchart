rm(list=ls())
setwd("~")

library(pacman)
p_load(tidyverse, readxl, zoo, extrafont, ggrepel, RColorBrewer, extrafont, geofacet)


inp <- "/Users/hectorchavez/Municipal-Delitos-2015-2020_abr2020"
inp2 <- "/Users/hectorchavez/CNI"
fua <- read_excel(paste(inp, "2018.xlsx", sep="/"))
pob <- read.csv(paste(inp, "pob.csv", sep="/"))
pob <- pob %>%
      mutate(cve_ent=formatC(cve_ent, width=5, format="d", flag = "0")) 

pob$ent <- substr(pob$cve_ent,1,2)  

pob <- select(pob, cve_ent, ent, poblacion)
names(fua)
table(fua$`Subtipo de delito`)

fu <- filter(fua, `Subtipo de delito`=="Extorsión" | `Subtipo de delito`=="Feminicidio" 
             | `Subtipo de delito`=="Homicidio doloso" | `Subtipo de delito`=="Lesiones dolosas" 
             | `Subtipo de delito`=="Robo a negocio" | `Subtipo de delito`=="Robo a transeúnte en vía pública"
             | `Subtipo de delito`=="Robo de vehículo automotor")

nombres <- c("year", "cve_ent", "ent", "cve_mun", "mun", "bien_afectado", "tipo_delito", "subtipo_delito", "modalidad")
names(fu)[1:9] <- nombres
fu <- fu %>%
      mutate(cve_ent=formatC(cve_ent, width=2, format="d", flag = "0")) %>%
      mutate(cve_mun=formatC(cve_mun,width=5, format="d", flag = "0"))


haf <- fu %>%
       filter(subtipo_delito=="Homicidio doloso" & modalidad=="Con arma de fuego")

haf <- gather(haf, mes, total, Enero:Diciembre)
haf <- haf %>%
       group_by(year, ent, cve_ent, mes) %>%
       summarise(total=sum(total=sum(total, na.rm=T))) %>%
       ungroup()


pobent <- pob %>%
          group_by(ent) %>%
          summarise(total_pob=sum(poblacion, na.rm = T)) %>%
          ungroup()
haf <- left_join(haf, pobent, by=c("cve_ent"="ent"))
haf$thom <- round((haf$total/haf$total_pob)*100000, digits=2)

haf <- mutate(haf, month=if_else(mes=="Enero", "01",
                    if_else(mes=="Febrero", "02",
                    if_else(mes=="Marzo", "03",
                    if_else(mes=="Abril", "04",
                    if_else(mes=="Mayo", "05",
                    if_else(mes=="Junio", "06",
                    if_else(mes=="Julio", "07",
                    if_else(mes=="Agosto", "08",
                    if_else(mes=="Septiembre", "09",
                    if_else(mes=="Octubre", "10",
                    if_else(mes=="Noviembre", "11", "12"))))))))))))
                    


haf$dia <- "01"
haf$year <- as.character(haf$year)
haf$fecha <- paste(haf$month, haf$dia, haf$year, sep="/")
haf$fecha <- as.Date(haf$fecha,
                       format = "%m/%d/%Y")
haf$fecha <- read.zoo(text = haf$fecha, FUN = as.yearmon)
haf$puntito_final <- NA
haf$thom <- as.character(haf$thom)
haf <- mutate(haf, punto_final=if_else(mes=="Diciembre", thom, "NA"))
haf$punto_final <- as.numeric(haf$punto_final)
haf <- mutate(haf, etiquetas=if_else(mes=="Diciembre", ent, ""))



haf$thom <- as.numeric(haf$thom)
dosocho <- haf
rm(pob, pobent, fua, fu, haf)
#########################################################
fua <- read_excel(paste(inp, "2019.xlsx", sep="/"))
pob <- read.csv(paste(inp, "pob.csv", sep="/"))
pob <- pob %>%
  mutate(cve_ent=formatC(cve_ent, width=5, format="d", flag = "0")) 

pob$ent <- substr(pob$cve_ent,1,2)  

pob <- select(pob, cve_ent, ent, poblacion)
names(fua)
table(fua$`Subtipo de delito`)

fu <- filter(fua, `Subtipo de delito`=="Extorsión" | `Subtipo de delito`=="Feminicidio" 
             | `Subtipo de delito`=="Homicidio doloso" | `Subtipo de delito`=="Lesiones dolosas" 
             | `Subtipo de delito`=="Robo a negocio" | `Subtipo de delito`=="Robo a transeúnte en vía pública"
             | `Subtipo de delito`=="Robo de vehículo automotor")

nombres <- c("year", "cve_ent", "ent", "cve_mun", "mun", "bien_afectado", "tipo_delito", "subtipo_delito", "modalidad")
names(fu)[1:9] <- nombres
fu <- fu %>%
  mutate(cve_ent=formatC(cve_ent, width=2, format="d", flag = "0")) %>%
  mutate(cve_mun=formatC(cve_mun,width=5, format="d", flag = "0"))

write.csv(fu, paste(inp2, "delitos_todos.csv", sep="/"))
haf <- fu %>%
  filter(subtipo_delito=="Homicidio doloso" & modalidad=="Con arma de fuego")

haf <- gather(haf, mes, total, Enero:Diciembre)
haf <- haf %>%
  group_by(year, ent, cve_ent, mes) %>%
  summarise(total=sum(total=sum(total, na.rm=T))) %>%
  ungroup()


pobent <- pob %>%
  group_by(ent) %>%
  summarise(total_pob=sum(poblacion, na.rm = T)) %>%
  ungroup()
haf <- left_join(haf, pobent, by=c("cve_ent"="ent"))
haf$thom <- round((haf$total/haf$total_pob)*100000, digits=2)

haf <- mutate(haf, month=if_else(mes=="Enero", "01",
                          if_else(mes=="Febrero", "02",
                          if_else(mes=="Marzo", "03",
                          if_else(mes=="Abril", "04",
                          if_else(mes=="Mayo", "05",
                          if_else(mes=="Junio", "06",
                          if_else(mes=="Julio", "07",
                          if_else(mes=="Agosto", "08",
                          if_else(mes=="Septiembre", "09",
                          if_else(mes=="Octubre", "10",
                          if_else(mes=="Noviembre", "11", "12"))))))))))))



haf$dia <- "01"
haf$year <- as.character(haf$year)
haf$fecha <- paste(haf$month, haf$dia, haf$year, sep="/")
haf$fecha <- as.Date(haf$fecha,
                     format = "%m/%d/%Y")
haf$fecha <- read.zoo(text = haf$fecha, FUN = as.yearmon)
haf$puntito_final <- NA
haf$thom <- as.character(haf$thom)
haf <- mutate(haf, punto_final=if_else(mes=="Diciembre" & year==, thom, "NA"))
haf$punto_final <- as.numeric(haf$punto_final)
haf <- mutate(haf, etiquetas=if_else(mes=="Diciembre", ent, ""))

haf$thom <- as.numeric(haf$thom)

final <- bind_rows(dosocho, haf)
final$thom <- as.character(final$thom)
final <- mutate(final, punto_final=if_else(mes=="Diciembre" & year=="2019", thom, "NA"))
final$punto_final <- as.numeric(final$punto_final)
final$thom <- as.numeric(final$thom)

bc <- filter(final, ent=="Baja California")
other <- filter(final, ent!="Baja California")
col <- filter(final, ent=="Colima")
other <- filter(other, ent!="Colima")
mich <- filter(final, ent=="Michoacán de Ocampo")
other <- filter(other, ent!="Michoacán de Ocampo")
gua <- filter(final, ent=="Guanajuato")
other <- filter(other, ent!="Guanajuato")
chi <- filter(final, ent=="Chihuahua")
other <- filter(other, ent!="Chihuahua")


rm(pob, pobent, dosocho, fua, fu)


########################

fua <- read_excel(paste(inp, "2020.xlsx", sep="/"))
pob <- read.csv(paste(inp, "pob.csv", sep="/"))
pob <- pob %>%
  mutate(cve_ent=formatC(cve_ent, width=5, format="d", flag = "0")) 

pob$ent <- substr(pob$cve_ent,1,2)  

pob <- select(pob, cve_ent, ent, poblacion)
names(fua)
table(fua$`Subtipo de delito`)

fu <- filter(fua, `Subtipo de delito`=="Extorsión" | `Subtipo de delito`=="Feminicidio" 
             | `Subtipo de delito`=="Homicidio doloso" | `Subtipo de delito`=="Lesiones dolosas" 
             | `Subtipo de delito`=="Robo a negocio" | `Subtipo de delito`=="Robo a transeúnte en vía pública"
             | `Subtipo de delito`=="Robo de vehículo automotor")

nombres <- c("year", "cve_ent", "ent", "cve_mun", "mun", "bien_afectado", "tipo_delito", "subtipo_delito", "modalidad")
names(fu)[1:9] <- nombres
fu <- fu %>%
  mutate(cve_ent=formatC(cve_ent, width=2, format="d", flag = "0")) %>%
  mutate(cve_mun=formatC(cve_mun,width=5, format="d", flag = "0"))


haf <- fu %>%
  filter(subtipo_delito=="Homicidio doloso" & modalidad=="Con arma de fuego")

haf <- gather(haf, mes, total, Enero:Diciembre)
haf <- haf %>%
  group_by(year, ent, cve_ent, mes) %>%
  summarise(total=sum(total=sum(total, na.rm=T))) %>%
  ungroup()


pobent <- pob %>%
  group_by(ent) %>%
  summarise(total_pob=sum(poblacion, na.rm = T)) %>%
  ungroup()
haf <- left_join(haf, pobent, by=c("cve_ent"="ent"))
haf$thom <- round((haf$total/haf$total_pob)*100000, digits=2)

haf <- mutate(haf, month=if_else(mes=="Enero", "01",
                          if_else(mes=="Febrero", "02",
                          if_else(mes=="Marzo", "03",
                          if_else(mes=="Abril", "04",
                          if_else(mes=="Mayo", "05",
                          if_else(mes=="Junio", "06",
                          if_else(mes=="Julio", "07",
                          if_else(mes=="Agosto", "08",
                          if_else(mes=="Septiembre", "09",
                          if_else(mes=="Octubre", "10",
                          if_else(mes=="Noviembre", "11", "12"))))))))))))



haf$dia <- "01"
haf$year <- as.character(haf$year)
haf$fecha <- paste(haf$month, haf$dia, haf$year, sep="/")
haf$fecha <- as.Date(haf$fecha,
                     format = "%m/%d/%Y")
haf$fecha <- read.zoo(text = haf$fecha, FUN = as.yearmon)
haf$puntito_final <- NA
haf$thom <- as.character(haf$thom)
haf <- mutate(haf, punto_final=if_else(mes=="Abril" & year=="2020", thom, "NA"))
haf$punto_final <- as.numeric(haf$punto_final)
haf <- mutate(haf, etiquetas=if_else(mes=="Abril", ent, ""))

haf$thom <- as.numeric(haf$thom)
haf$month_num <- as.numeric(haf$month)

haf <- filter(haf, month_num<=4)
final <- bind_rows(final, haf)
final$thom <- as.character(final$thom)
final <- mutate(final, punto_final=if_else(mes=="Abril" & year=="2020", thom, "NA"))
final$punto_final <- as.numeric(final$punto_final)
final$thom <- as.numeric(final$thom)


rm(pob, pobent, dosocho, fua, fu)





bc <- filter(final, ent=="Baja California")
other <- filter(final, ent!="Baja California")
col <- filter(final, ent=="Colima")
other <- filter(other, ent!="Colima")
mich <- filter(final, ent=="Michoacán de Ocampo")
other <- filter(other, ent!="Michoacán de Ocampo")
gua <- filter(final, ent=="Guanajuato")
other <- filter(other, ent!="Guanajuato")
chi <- filter(final, ent=="Chihuahua")
other <- filter(other, ent!="Chihuahua")

haf$etiquetas <- gsub("Michoacán de Ocampo", "Michoachán", haf$etiquetas)

display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(7, "RdYlBu")
brewer.pal(7, "RdYlBu")

ggplot(final, aes(x = fecha, 
           y = thom, group=ent))+ 
  geom_line(data=other, aes(fecha, thom, group=ent), size = 1, 
           color = "grey", 
          alpha = 0.6)+
  geom_line(data=col, aes(fecha, thom, group=ent), size = 1, 
            color = "#D73027")+

  geom_line(data=bc, aes(fecha, thom, group=ent), size = 1, 
            color = "#FC8D59")+
  geom_line(data=mich, aes(fecha, thom, group=ent), size = 1, 
            color = "#FEE090")+
  geom_line(data=gua, aes(fecha, thom, group=ent), size = 1, 
            color = "#31a354")+
  geom_line(data=chi, aes(fecha, thom, group=ent), size = 1, 
            color = "#91BFDB")+
  geom_point(data= filter(final, thom>=2.81), aes(x = fecha, 
  y = punto_final),
            size = 2, 
    color = "#1E6847") +
  geom_text(data= filter(haf, thom>=2.8), aes(label = etiquetas), 
            check_overlap = F,
            force = 3,
            vjust = -0.7,
            color = "grey30",
            # bg.colour = 'white',
            fontface = "bold",
            size = 5) +
  labs(title = "Tendencias de las tasas de homicidio estatales",
       subtitle ="Del 1ro de enero de 2018 al 30 de abril de 2020",
       x = "Fecha",
       y = "Tasa de homicidios dolosos",
       caption = "Fuente: Delitos municipales - SNSP") +
  theme_minimal() +
  theme(text = element_text(family = "Montserrat", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold",  family = "Montserrat", color = "grey35"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666",  family = "Montserrat"),
        plot.caption = element_text(hjust = 0, size = 15),
        legend.title = element_text(size = 16, face = "bold", family = "Montserrat"),
        legend.text = element_text(size = 14, family = "Montserrat"),
        axis.title = element_text(size = 18, face = "bold",  family = "Montserrat"),
        axis.text = element_text(size = 16, face = "bold", family = "Montserrat"))

ggsave(paste(inp, "1_tendencias.jpg", sep="/"), width=12, height=12)

rm(bc, chi, col, gua, mich)
final$CV_ENT_ADMINISTRATIVA <- as.numeric(final$cve_ent)
ggplot(final, aes(x=factor(fecha), y=thom)) +
  geom_line() + 
  facet_geo(~ ENT_ADMNISTRATIVA, grid = "mx_state_grid2", label = "code_abbr") 

  labs(title ="Porcentaje de escuelas según el total de estudiantes por grupo", 
       subtitle="", x = "", y = "%", caption = "Fuente: Estadística 911 - Ciclo 2018-2019", fill="Alumnos por grupo") + 
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        text=element_text(face="bold", family= "Montserrat",size=14),
        title  = element_text(face="bold", family= "Montserrat",size=14))


