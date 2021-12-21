# Cargamos nuestras librerías
if(require(tidyverse)==FALSE){
  install.packages("tidyverse")
  library(tidyverse)
}else{
  library(tidyverse)
}

if(require(tidymodels)==FALSE){
  install.packages("tidymodels")
  library(tidymodels)
}else{
  library(tidymodels)
}

if(require(ggdist)==FALSE){
  install.packages("ggdist")
  library(ggdist)
}else{
  library(ggdist)
}

if(require(patchwork)==FALSE){
  install.packages("patchwork")
  library(patchwork)
}else{
  library(patchwork)
}

# Cargar el catálogo de datos
petroleo = read_csv('bases/estados_con_petróleo.csv') %>%
  # Construir un factor con texto
  mutate(est_petroleo = factor(
                          if_else(
                            est_petroleo == 1, 'Con Petróleo', 'Sin Petróleo'),
                          levels = c('Con Petróleo', 'Sin Petróleo')
                          )
         )

# Obtener la lista de archivos
archivos = list.files(path = 'bases',full.names = T)[str_detect(list.files(path = 'bases'),'id')]
# Cargar los archivos
estados = archivos %>%
  # Fijar nombres
  setNames(nm = .) %>% 
  map_df(~read_csv(.x, col_types = cols(), col_names = T), .id = "archivo")%>%
  # Crear la variable estado_id a partir del nombre del archivo
  mutate(estado_id = as.numeric(str_extract(archivo,'\\d+')), archivo = NULL) %>%
  # Concatenar con el catálogo
  right_join(petroleo, by = 'estado_id') 

# Pregunta 1: un gráfico de cajascon el promedio por estado y coloreado por tenencia de petróleo
cajas_principal= estados %>%
  # Calcular las medias de pib per capita por estado
  with_groups(
    .groups = c(estado_id, est_petroleo),
    summarise,
    pib_percap = mean(pib/pob, na.rm = T)
  ) %>%
  # Construir el gráfico de cajas
  ggplot(
    aes(
      y = pib_percap,
      x = est_petroleo,
      fill = est_petroleo,
      col = est_petroleo
    )
  ) +
  geom_boxplot() +
  # Modificar etiquetas
  labs(
    x = 'Estados'
    ) +
  # Añadir títulos
  ggtitle(
    label = 'Los estados con petróleo tienen un mayor nivel de riqueza',
    subtitle = 'PIB per cápita estatal'
  ) +
  # Modificar paleta de colores
  scale_color_manual(
    values = c('gray60','gray40')
  ) +
  scale_fill_manual(
    values = c('black','orange')
  ) +
  # Modificar el tema
  theme_ggdist() +
  theme (
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title.y  = element_blank(),
    axis.line.x = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
    )

# Crear un gráfico secundario
cajas_secundario = estados %>%
  ggplot(
    aes(
      y = pib/pob,
      x = reorder(factor(estado_id),desc(pib/pob)),
      fill = est_petroleo,
      col = est_petroleo
    )
  ) +
  geom_boxplot() +
  facet_wrap(~est_petroleo, scales = 'free_x') +
  # Modificar etiquetas
  labs(
    x = 'Estados') +
  # Modificar paleta de colores
  scale_color_manual(
    values = c('gray60','gray30')
  ) +
  scale_fill_manual(
    values = c('black','orange')
  ) +
  # Modificar temas
  theme_ggdist() +
  theme (
    legend.position = 'none',
    axis.title.y  = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt")
  ) 

# Ordenar gráficos
cajas_principal/cajas_secundario

# Guardar gráfico
ggsave('boxplot.png',
       # Máxima resolución 
       units="cm",  dpi = 300,
       # Dimensiones del gráfico
       width = 15, height = 15)

# Pregunta 2: Población total y PIB total en la misma gráfica
lineas_y_barras_principal = estados %>%
  # Calcular los totales
  with_groups(
    .groups = tiempo,
    summarise,
    pob = sum(pob),
    pib = sum(pib),
  ) %>%
  ggplot(
    aes(
      x = tiempo
    )
  ) +
  # Columnas para la Población
  geom_col(
    aes(
      y = pob/1000,
      fill = 'Población'
    )
  ) +
  scale_fill_manual(values = 'gray60') +
  # Lineas para el PIB
  geom_line(
    aes(
      y = pib/15000,
      col = 'PIB'
    )
  ) +
  geom_point(
    aes(
      y = pib/15000,
      col = 'PIB'
    )
  ) +
  scale_color_manual(values = 'gray20') +
  # Ajustamos nuestra leyenda
  guides(color = guide_legend(title = '', label.position = 'left'),
         fill = guide_legend(title = '')) +
  #Crear eje secundario
  scale_y_continuous(
    
    # Eje primario
    name = "Población\n(miles)",
    
    # Eje secundario
    sec.axis = sec_axis( trans=~./15, 
                         breaks = c(0,5,10,15),
                         labels = c(0,1,2,3),
                         name="PIB\n(millones)")
  ) +
  scale_x_continuous(
    breaks = seq(1990,2015, 5)
  ) +
  theme_ggdist() +
  theme (
    legend.position = 'top',
    legend.spacing = unit(9, "cm"),
    axis.title.x = element_blank(),
    axis.title.y.left = element_blank() ,
    axis.title.y.right = element_blank(),
  ) +
  ggtitle('El crecimiento del PIB va de la mano del crecimiento poblacional')

# Crear gráfico secundario
lineas_y_barras_secundario = estados %>%
  # Calculamos los totales por tipo de estado
  with_groups(
    .groups = c(tiempo,est_petroleo),
    summarise,
    pob = sum(pob),
    pib = sum(pib)
  ) %>%
  # Calcular el cambio anual porcentual 
  with_groups(
    .groups = est_petroleo,
    mutate,
    pob =  100 * log(pob/lag(pob,1)),
    pib =  100 * log(pib/lag(pib,1))
  ) %>%
  ggplot(
    aes(
      x = tiempo
    )
  ) +
  # Columnas para Población
  geom_col(
    aes(
      y = pob,
      fill = est_petroleo
    )
  ) + 
  # Lineas para el PIB
  geom_line(
    aes(
      y = pib,
      col = est_petroleo
    )
  ) + 
  geom_point(
    aes(
      y = pib,
      col = est_petroleo
    )
  ) +
  scale_fill_manual(values = c('black','orange')) +
  scale_color_manual(values = c('gray60','gray20')) +
  facet_wrap(~est_petroleo, ncol = 1) +
  scale_x_continuous(
    breaks = seq(1990,2015, 5)
  ) +
  theme_ggdist() + 
  theme (
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.title.y  = element_blank(),
  ) +
  ggtitle('Variación Anual (%) según tenencia de petróleo')

# Areglar nuestro gráfico  
lineas_y_barras_principal / lineas_y_barras_secundario + 
  plot_layout(heights = c(4.5,10))  
 
ggsave('barras_y_lineas.png',
       # Máxima resolución 
       units="cm",  dpi = 300,
       # Dimensiones del gráfico
       width = 15, height = 15)

# Pregunta 3: Una gráfica libre que añada valor al análisis
# Estimar un modelo OLS
# Crear gráfica de dispersión simple
dispersion = estados %>%
  ggplot(
    aes(
      y = pib/1000,
      x = pob,
      col = est_petroleo
    )
  ) +
  geom_point(alpha = 0.5) +
  scale_x_log10()+
  scale_y_log10()+
  # Añadir el modelo lineal
  stat_smooth(method = 'lm', formula = y~x) +
  scale_color_manual(
    values = c('black','orange')
  ) +
  labs(x = 'log(Población)', y = 'log(PIB)') +
  theme_ggdist() +
  theme (
    legend.position = 'none',
    axis.title.x = element_text(size = 9, color = 'gray30'),
    axis.title.y = element_text(size = 9, color = 'gray30', angle = 0,
                                margin = margin(t = 0, r = -30, b = 0, l = 0))
  )+
  ggtitle('Resultados del Modelo', subtitle = 'Regresión log~log')


# Ahora estimar el modelo
modelo =  estados %>% 
  # Hacer un modelo para cada año
  group_by(est_petroleo, tiempo) %>% 
  nest() %>%
  mutate(modelo = map(data, ~lm(formula = log(pib)~log(pob), data = .x))) 

# Aquí definimos las etiquetas para el grafico
term_labs = c('Intercepto','Sensibilidad al crecimiento poblacional')
names(term_labs) = c('(Intercept)','log(pob)') 

# Extraemos los coeficientes del modelo
resultados = modelo%>% 
  mutate(coeficiente = map(modelo, ~tidy(.x)))%>%
  unnest(coeficiente) %>% 
  ggplot(
    aes(
      x = estimate,
      y = est_petroleo,
      fill= est_petroleo,
      col = est_petroleo
    )
  ) +
  # Hacemos un gráfico de lluvia
  stat_halfeye() +
  stat_dotsinterval(side = 'left') +
  facet_wrap(
    ~term, 
    scales = 'free_x', 
    ncol= 2,
    labeller = labeller(term= term_labs )) +
  scale_fill_manual(values = c('black','orange')) +
  scale_color_manual(values = c('gray60','gray20')) +
  theme_ggdist() +
  theme (
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.title.y  = element_blank()
  ) +
  ggtitle(label ='', 
          subtitle = 'El crecimiento poblacional tiene un menor impacto en el PIB estados \ncon petróleo. Pero estos cuentan con una mayor riqueza.')

# También visualizaremos los residuales
residuales = modelo %>% 
  # Obtenemos un df aumentado de cada modelo
  mutate(augmented = map(modelo,~augment(.x))) %>% 
  # expandimos el df
  unnest(augmented) %>%
  # Calculamos los residuales
  ggplot(
    aes(
      x = .fitted - `log(pib)`
    )
  ) +
  stat_dotsinterval()+
  labs(x = 'Residuales') +
  theme_ggdist() +
  theme (
    legend.position = 'none',
    axis.title.y  = element_blank(),
    axis.title.x = element_text(size = 9, color = 'gray30')
  ) +
  ggtitle(label = '', subtitle = 'Residuales de OLS log-log')

# Construimos el gráfico final
(dispersion + residuales) / resultados

ggsave('modelo_ols.png',
       # Máxima resolución 
       units="cm",  dpi = 300,
       # Dimensiones del gráfico
       width = 15, height = 15)
