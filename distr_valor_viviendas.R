######################################################################
# Simulación de valores de viviendas por estados en México
#
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################



##############
#Configuración----
rm(list = ls())

library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,
       foreign,expss,data.table, srvyr, dineq, datapasta, scales)

final18 <-
  read_dta("~/Documents/Encuestas/MMIP/2018 Nuevo ACN/Bases/Final/final18.dta")
final18 <- final18 %>% 
  dplyr::select(-ict)

hog_mmip <-
  read_dta("~/Documents/Encuestas/MMIP/2018 Nuevo ACN/Bases/TMP/hogaresmmip.dta")
hog_mmip <- hog_mmip %>% 
  dplyr::select(renta, estim_pago, folio,tenencia)

ingresos_fuentes <-
  read_dta("~/Documents/Encuestas/MMIP/2018 Nuevo ACN/Bases/Ingresos/ing_cor_tot_fuentes_18.dta")
ingresos_fuentes <- ingresos_fuentes %>% 
  dplyr::select(ing_renta_casas_edif_t, ict, folio)

datitos <- final18 %>% 
  left_join(ingresos_fuentes, by = "folio") %>% 
  left_join(hog_mmip, by = "folio") 

#Los precios de SHF vienen de acá: https://www.gob.mx/cms/uploads/attachment/file/568297/2020_II_ZM_publicar.pdf
precios_shf <- tibble::tribble(
              ~entidad,    ~mean,     ~p25,     ~p50,     ~p75,
              1L,  917894L,  439000L,  593000L, 1116000L,
              2L, 1058691L,  467900L,  598138L, 1085000L,
              3L, 1154560L,  618615L,  799500L, 1238610L,
              4L,  979828L,  562184L,  739588L, 1201834L,
              5L,  772427L,  424826L,  520000L,  802128L,
              6L,  822708L,  464000L,  600000L,  907450L,
              7L,  966448L,  536760L,  720000L, 1110830L,
              8L,  929546L,  444944L,  602093L, 1119581L,
              9L, 2911548L, 1329875L, 2090458L, 3570000L,
              10L,  676619L,  411029L,  486000L,  673935L,
              11L,  931403L,  447000L,  598618L, 1038072L,
              12L, 1127645L,  566000L,  739463L, 1173705L,
              13L,  863427L,  503975L,  637259L, 1047000L,
              14L, 1298589L,  467858L,  682399L, 1473923L,
              15L, 1342186L,  565913L,  754077L, 1424865L,
              16L,  964062L,  464400L,  652093L, 1129860L,
              17L, 1461389L,  732000L, 1100461L, 1563000L,
              18L, 1280817L,  510000L,  669785L, 1358867L,
              19L, 1072328L,  464384L,  571001L, 1100000L,
              20L, 1104097L,  611999L,  807919L, 1229669L,
              21L, 1053901L,  469332L,  666000L, 1203240L,
              22L, 1572828L,  732113L, 1166855L, 1971755L,
              23L, 1081322L,  479667L,  629000L, 1139822L,
              24L, 1141913L,  530444L,  785067L, 1339020L,
              25L,  994373L,  465780L,  646912L, 1160000L,
              26L,  911222L,  430053L,  553045L, 1007500L,
              27L, 1056944L,  510603L,  701000L, 1086962L,
              28L,  662957L,  419057L,  473352L,  650009L,
              29L,  729129L,  421323L,  577200L,  835000L,
              30L,  876778L,  475000L,  651963L, 1004274L,
              31L, 1102267L,  464748L,  748000L, 1337998L,
              32L,  777594L,  434000L,  555270L,  835000L
            )
glimpse(precios_shf)

datitos_hog <- datitos %>% 
  filter(parentesco==101) %>% 
  mutate(decil_nac  = dineq::ntiles.wtd(ict, 10, weights = factor)) %>% 
  group_by(entidad) %>% 
  mutate(deciles  = dineq::ntiles.wtd(ict, 10, weights = factor),
         veintiles= dineq::ntiles.wtd(ict, 20, weights = factor),
         renta= replace_na(renta,0),
         estim_pago= replace_na(estim_pago,0),
         renta_mensual=renta+estim_pago,
         renta_anual= renta_mensual*12,
         valor_casa= renta_anual*20,
         n=1,
         total_hogares=n*factor,
         propietario= case_when((tenencia=="3"|tenencia=="4"|ing_renta_casas_edif_t>0) ~1,
                                TRUE ~ 0)
         )

glimpse(datitos_hog)
summary(datitos_hog)

precios_viv_enigh_nac <-
datitos_hog %>% 
  as_survey(weights = c(factor)) %>%
  summarize(media = survey_mean(valor_casa, na.rm = T),
            mediana = survey_median(valor_casa, na.rm = T),
            minimo = min(valor_casa, na.rm = T),
            maximo = max(valor_casa, na.rm = T))

precios_viv_enigh_estatal <-
  datitos_hog %>% 
  as_survey(weights = c(factor)) %>%
  group_by(entidad) %>% 
  summarize(mediana = survey_median(valor_casa, na.rm = T),
            cuantil = survey_quantile(valor_casa, c(0.25,0.75), na.rm = T))
glimpse(precios_viv_enigh_estatal)            



#Crear factores
precios_viv_enigh_estatal
precios_estatal <- precios_viv_enigh_estatal %>% 
  left_join(precios_shf, by = "entidad") 
glimpse(precios_estatal)


precios_estatal <- precios_estatal %>% 
  mutate(fct_p25= p25 / cuantil_q25 ,
         fct_p50= p50 / mediana ,
         fct_p75= p75 / cuantil_q75 )
glimpse(precios_estatal)
view(precios_estatal)

factores <- precios_estatal %>% 
  mutate(v_1= fct_p25,
         v_2= fct_p25,
         v_3= fct_p25,
         v_4= fct_p25,
         v_5= fct_p25,
         v_6= fct_p25,
         v_7= fct_p50,
         v_8= fct_p50,
         v_9= fct_p50,
         v_10= fct_p50,
         v_11= fct_p50,
         v_12= fct_p50,
         v_13= fct_p50,
         v_14= fct_p75,
         v_15= fct_p75,
         v_16= fct_p75,
         v_17= fct_p75,
         v_18= fct_p75,
         v_19= fct_p75,
         v_20= fct_p75
         ) %>% 
  dplyr::select(entidad,starts_with("v_")) %>% 
  pivot_longer(!entidad, names_to = "veintiles", values_to = "factor") %>% 
  separate(veintiles, c("v", "veintiles"),
           sep="_", remove = T) %>% 
  dplyr::select(-v) %>% 
  dplyr::mutate(veintiles=as.numeric(veintiles)) 


precio_veintiles_viv <-   datitos_hog %>% 
    as_survey(weights = c(factor)) %>%
  group_by(entidad,veintiles) %>% 
  summarize(media = survey_mean(valor_casa, na.rm = T),
            propietario= survey_mean(propietario, na.rm = T),
            total_hogares=survey_total(n, na.rm = T))
glimpse(precio_veintiles_viv)

precio_veintiles_viv_wider <- precio_veintiles_viv %>% 
  dplyr::select(-media_se,-propietario,-propietario_se,-total_hogares,-total_hogares_se) %>% 
  pivot_wider(names_from = entidad, values_from = media)

write_csv(precio_veintiles_viv_wider,"www/precio_veintiles_viv_wider.csv")


datitos_fin<- precio_veintiles_viv  %>% 
  left_join(factores, by = c("entidad","veintiles")) %>% 
  dplyr::mutate(
    valor_promedio = media*factor,
    hogares_propietarios = propietario*total_hogares,
    acumulacion_valor = valor_promedio*hogares_propietarios)

acum_estado <- datitos_fin %>% 
  dplyr::group_by(entidad)  %>% 
  dplyr::summarise(
      sum_valor = sum(acumulacion_valor),
  )
  
datitos_final<- datitos_fin  %>% 
  left_join(acum_estado, by = c("entidad")) %>% 
  dplyr::mutate(
    acum_porcen=acumulacion_valor/sum_valor
  )
  
facet_acum <- 
  datitos_final %>% 
  ggplot( aes( y=acum_porcen, x=veintiles))+
  geom_bar(fill="#201a99",position="stack", stat="identity")+
  facet_wrap(~entidad)
  

facet_precios <-
  datitos_final %>% 
  ggplot( aes( y=valor_promedio, x=veintiles))+
  geom_bar(fill="#201a99",position="stack", stat="identity")+
  facet_wrap(~entidad)+
  scale_y_continuous(labels = label_number(scale = 1/1000,
                                           suffix = "K")) 

acum_estado_grupos <-
datitos_final %>% 
  dplyr::mutate(grupos=factor((case_when(
                      veintiles<=6 ~ 1,
                      veintiles>6 & veintiles<=18 ~ 2,
                      veintiles>18 ~ 3,
                      )),
                      levels = c(1,2,3),
                      labels = c("40% más pobre (P1-P40)",
                                 "Percentil 41 al 90",
                                 "10% más rico")
                      ),
                entidad=as.factor(entidad),
                ) %>% 
  ggplot(aes(fill=grupos, y=acumulacion_valor, x=entidad)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  scale_y_reverse()


write_csv(datitos_final,"www/acumulacion_valores_viviendas.csv")

  
    