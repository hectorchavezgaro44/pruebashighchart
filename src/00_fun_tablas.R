preparar_info_censo <- function(inegi,
                                unidad_analisis,
                                id_unidad_analisis,
                                variable,
                                weight
){
  info <- inegi %>%
    clean_names() %>%
    select(unidad_analisis, variable, weight) %>%
    as_tibble() %>%
    select(-geometry) %>%
    group_by(!!sym(unidad_analisis)) %>%
    summarise(variable=sum(!!sym(variable), na.rm=T),
              weight=sum(!!sym(weight), na.rm=T)) %>%
    ungroup() %>%
    mutate(Porcentaje=round((as.numeric(variable)/as.numeric(weight)), 4)) %>%
    select(unidad_analisis, Porcentaje) %>%
    arrange(-Porcentaje) %>%
    transform(rank=match(Porcentaje, unique(Porcentaje))) %>%
    mutate(Variable=variable,
           numero=nrow(.),
           Ranking=paste("Ranking", rank, "de", numero)) %>%
    select(Variable, unidad_analisis, Porcentaje, Ranking) %>%
    filter(!!sym(unidad_analisis)==id_unidad_analisis)
  
  return(info)
  
}


graficando_tabla <- function(bd,
                             bd2,
                             bd3,
                             unidad_analisis
                             
){
  final <- bind_rows(bd, bd2, bd3) %>%
    left_join(inegi_traduccion) %>%
    select(Indicador, unidad_analisis, Porcentaje, Ranking) %>%
    mutate(loque=!!sym(unidad_analisis),
           obs = paste(unidad_analisis, loque)) %>%
    select(Indicador, obs, Porcentaje, Ranking)
  
  
  gt_tbl <- gt(data = final %>% select(-obs))%>%
    fmt_percent(
      columns = vars(Porcentaje),
      decimals = 2
    )
  
  tbl_bd <-
    gt_tbl %>%
    tab_header(
      title = paste("Características socioeconómicas", unique(final$obs), sep=" "),
      subtitle = "Fuente: Estadísticas Censales a Escalas Geoelectorales"
    )
  
  
  return(tbl_bd)
  
}


