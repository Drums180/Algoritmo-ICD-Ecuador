
##################### Clean Master Clientes #####################

# Selección de Columnas Necesarias
master_clientes_seleccionados <- master_clientes %>%
  select(
    codigo,
    tamano,
    agenciabi,
    region,
    actividad,
    subcanal,
    modeloservicio,
    zona,
    rutaventa,
    lattuddec,
    lngtuddec,
    tipo
  )

# Valores Unicos para Variables no Numericas
valores_unicos <- list(
  tamano = unique(master_clientes_seleccionados$tamano),
  agenciabi = unique(master_clientes_seleccionados$agenciabi),
  region = unique(master_clientes_seleccionados$region),
  actividad = unique(master_clientes_seleccionados$actividad),
  subcanal = unique(master_clientes_seleccionados$subcanal),
  modeloservicio = unique(master_clientes_seleccionados$modeloservicio),
  zona = unique(master_clientes_seleccionados$zona),
  tipo = unique(master_clientes_seleccionados$tipo)
)

print(valores_unicos)

# Limpieza de Valores

## Unificación de Tamaños

master_clientes_normalizado <- master_clientes_seleccionados %>%
  mutate(tamano = case_when(
    tamano %in% c("1 - MICRO", "MICRO") ~ "MICRO",
    tamano %in% c("2 - CHICO", "CHICO") ~ "CHICO",
    tamano %in% c("3 - MEDIANO", "MEDIANO") ~ "MEDIANO",
    tamano %in% c("4 - GRANDE", "GRANDE") ~ "GRANDE",
    tamano %in% c("5 - EXTRAGRANDE", "EXTRA GRANDE") ~ "EXTRAGRANDE",
    TRUE ~ tamano  # Esto deja los valores que no coinciden con los anteriores como están
  ))

## Unificación de AgenciaBI

master_clientes_normalizado <- master_clientes_normalizado %>%
  mutate(agenciabi = case_when(
    grepl("QUITO VALLE", agenciabi, ignore.case = TRUE) ~ "QUITO VALLE",
    grepl("QUITO NORTE", agenciabi, ignore.case = TRUE) ~ "QUITO NORTE",
    grepl("IBARRA", agenciabi, ignore.case = TRUE) ~ "IBARRA",
    grepl("LAGO AGRIO", agenciabi, ignore.case = TRUE) ~ "LAGO AGRIO",
    grepl("ESMERALDAS", agenciabi, ignore.case = TRUE) ~ "ESMERALDAS",
    grepl("SANTO DOMINGO", agenciabi, ignore.case = TRUE) ~ "SANTO DOMINGO",
    grepl("QUEVEDO", agenciabi, ignore.case = TRUE) ~ "QUEVEDO",
    grepl("CHONE", agenciabi, ignore.case = TRUE) ~ "CHONE",
    grepl("PORTOVIEJO", agenciabi, ignore.case = TRUE) ~ "PORTOVIEJO",
    grepl("AMBATO", agenciabi, ignore.case = TRUE) ~ "AMBATO",
    grepl("PUYO", agenciabi, ignore.case = TRUE) ~ "PUYO",
    grepl("RIOBAMBA", agenciabi, ignore.case = TRUE) ~ "RIOBAMBA",
    grepl("BABAHOYO", agenciabi, ignore.case = TRUE) ~ "BABAHOYO",
    grepl("GUAYAQUIL NORTE", agenciabi, ignore.case = TRUE) ~ "GUAYAQUIL NORTE",
    grepl("GUAYAQUIL SUR", agenciabi, ignore.case = TRUE) ~ "GUAYAQUIL SUR",
    grepl("GUAYAQUIL CENTRO", agenciabi, ignore.case = TRUE) ~ "GUAYAQUIL CENTRO",
    grepl("CUENCA", agenciabi, ignore.case = TRUE) ~ "CUENCA",
    grepl("MACHALA", agenciabi, ignore.case = TRUE) ~ "MACHALA",
    grepl("LOJA", agenciabi, ignore.case = TRUE) ~ "LOJA",
    grepl("DURAN", agenciabi, ignore.case = TRUE) ~ "DURAN",
    TRUE ~ agenciabi  # Esto deja los valores que no coinciden con los anteriores como están
  ))

## Unificación de Region

master_clientes_normalizado <- master_clientes_normalizado %>%
  mutate(region = case_when(
    grepl("QUITO", region, ignore.case = TRUE) ~ "ZONA QUITO",
    grepl("COSTA", region, ignore.case = TRUE) ~ "ZONA COSTA",
    grepl("AUSTRO", region, ignore.case = TRUE) ~ "ZONA AUSTRO",
    grepl("GUAYAQUIL", region, ignore.case = TRUE) ~ "ZONA GUAYAQUIL",
    grepl("CENTRO - ORIENTE", region, ignore.case = TRUE) ~ "CENTRO - ORIENTE",
    TRUE ~ region  # Mantiene cualquier otra región tal como está si no coincide con los patrones anteriores
  ))

## Unificación de Actividad

master_clientes_normalizado <- master_clientes_normalizado %>%
  mutate(actividad = case_when(
    actividad %in% c("A - NEGOCIOS DE COMESTIBLES", "A") ~ "NEGOCIOS DE COMESTIBLES",
    actividad %in% c("B - OTROS NEGOCIOS Y SERVICIOS", "B") ~ "OTROS NEGOCIOS Y SERVICIOS",
    actividad == "H - ENTIDADES INTERMEDIAS" ~ "ENTIDADES INTERMEDIAS",
    actividad == "C - COMIDAS Y BEBIDAS" ~ "COMIDAS Y BEBIDAS",
    actividad == "G - TRABAJO" ~ "TRABAJO",
    actividad == "D - ENTRETENIMIENTO RECREACION ESPARCIMIENTO" ~ "ENTRETENIMIENTO Y RECREACION",
    actividad == "E - VIAJES TRANSPORTE ALOJAMIENTO" ~ "VIAJES, TRANSPORTE Y ALOJAMIENTO",
    actividad == "F - EDUCACION" ~ "EDUCACION",
    actividad == "W" ~ "W", 
    TRUE ~ "OTRA ACTIVIDAD" 
  ))

## Verificación
valores_unicos <- list(
  tamano = unique(master_clientes_normalizado$tamano),
  agenciabi = unique(master_clientes_normalizado$agenciabi),
  region = unique(master_clientes_normalizado$region),
  actividad = unique(master_clientes_normalizado$actividad),
  subcanal = unique(master_clientes_normalizado$subcanal),
  modeloservicio = unique(master_clientes_normalizado$modeloservicio),
  zona = unique(master_clientes_normalizado$zona),
  tipo = unique(master_clientes_normalizado$tipo)
)

print(valores_unicos)

summary(master_clientes_normalizado)

# Asumiendo que 'master_clientes_normalizado' ya está creado y listo para ser guardado
write.csv(master_clientes_normalizado, "Maestro_Clientes.csv", row.names = FALSE)


