# Fijar directorio de trabajo
setwd("~/Desktop/DataViz/30daymapchallenge/2. Lines")

# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar sf                                                       
if(require(sf) == FALSE){                                                
  install.packages('sf')                                                 
  library(sf)                                                            
}else{                                                                          
  library(sf)                                                            
}

# Instalar - Cargar ggspatial                                                       
if(require(ggspatial) == FALSE){                                                
  install.packages('ggspatial')                                                 
  library(ggspatial)                                                            
}else{                                                                          
  library(ggspatial)                                                            
}

# Instalar - Cargar ggimage                                                       
if(require(ggimage) == FALSE){                                                
  install.packages('ggimage')                                                 
  library(ggimage)                                                            
}else{                                                                          
  library(ggimage)                                                            
}


# Data --------------------------------------------------------------------
alcaldias = st_read('data/poligonos', options = "ENCODING=latin1") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

ciclista = st_read('data/ciclista') %>% 
  st_zm() %>% 
  mutate(
    TIPO_IC = case_when(
      TIPO_IC == 'Carril bus bici' ~ 'Carril Bus-Bici',
      TIPO_IC == 'Carril de prioridad ciclista' ~ 'Carril con prioridad ciclista',
      TIPO_IC == 'Ciclocarril' ~ 'Ciclocarril',
      str_detect(TIPO_IC, 'Ciclovia|Sendero') ~ 'Ciclovía',
      TRUE ~ 'Emergente'
    ) 
  ) %>% 
  filter(TIPO_IC != 'Emergente')

g = ggplot() +
  layer_spatial(data = alcaldias, fill = '#BCBDC0', col = '#797A7C', alpha = 0.7) +
  geom_sf(
    data = ciclista,
    aes(col = TIPO_IC, linetype = ESTADO),
    linewidth = 0.5
  ) +
  # Agregar la leyenda (hecha en inkscape)
  geom_image(
    mapping = aes(
      image = "images/ciclovia.png",
      # Centrar la imagen
      x = -98.85, y = 19.51), 
    # Tamaño y ajuste por ancho de imagen
    size = 0.15, by = "width"
  )  +
  # Agregar la leyenda (hecha en inkscape)
  annotate(
    geom = 'text',
    label = 'CICLO VÍA', col = '#ffe700',
    family = 'Open Sans Condensed ExtraBold',
    x = -98.85, y = 19.575, 
    size = 4
  ) +
  # Agregar la leyenda (hecha en inkscape)
  geom_image(
    mapping = aes(
      image = "images/ciclocarril.png",
      # Centrar la imagen
      x = -98.85, y = 19.37), 
    # Tamaño y ajuste por ancho de imagen
    size = 0.15, by = "width"
  )  +
  # Agregar la leyenda (hecha en inkscape)
  annotate(
    geom = 'text',
    label = 'CICLOCARRIL', col = 'white',
    family = 'Open Sans Condensed ExtraBold',
    x = -98.85, y = 19.435, 
    size = 4
  ) +
  # Agregar la leyenda (hecha en inkscape)
  geom_image(
    mapping = aes(
      image = "images/compartido.png",
      # Centrar la imagen
      x = -98.85, y = 19.23), 
    # Tamaño y ajuste por ancho de imagen
    size = 0.15, by = "width"
  )  +
  # Agregar la leyenda (hecha en inkscape)
  annotate(
    geom = 'text',
    label = 'CARRIL BUS-BICI', col = '#F0563A',
    family = 'Open Sans Condensed ExtraBold',
    x = -98.85, y = 19.295, 
    size = 4
  ) +
  # Agregar la leyenda (hecha en inkscape)
  geom_image(
    mapping = aes(
      image = "images/prioridad_ciclista.png",
      # Centrar la imagen
      x = -98.85, y = 19.09), 
    # Tamaño y ajuste por ancho de imagen
    size = 0.15, by = "width",
  ) +
  # Agregar la leyenda (hecha en inkscape)
  annotate(
    geom = 'text',
    label = 'PRIORIDAD CICLISTA', col = '#57585A',
    family = 'Open Sans Condensed ExtraBold',
    x = -98.85, y = 19.155, 
    size = 4
  ) +
  # Agrega la escala geográfica
  annotation_scale(
    location = "bl",
    bar_cols = c('white','#BCBDC0'),
    line_width = 0,
    text_family = 'Open Sans Condensed SemiBold',
    text_col = '#57585A'
  ) +
  # Agrega la escala geográfica
  annotation_north_arrow(
    location = "tl",                  # Posición en la esquina inferior derecha
    which_north = "true",             # Norte verdadero
    style = north_arrow_fancy_orienteering(
      line_col = "#57585A",     
      fill = c("#57585A", '#BCBDC0'),
      line_width = 1.5,               # Ancho de las líneas de la flecha
      text_size = 8,       
      text_family = 'Open Sans Condensed SemiBold',
      text_col = '#57585A'
      ),
    pad_y = unit(0.065, "npc")
  ) + 
  labs(
    title = 'INFRAESTRUCTURA CICLISTA EN CIUDAD DE MÉXICO',
    caption = 'Fuente: elaboración propia con Datos CDMX    \nAutor:@renorosgon    '
  ) +
  scale_x_continuous(limits = c(-99.35,-98.8)) +
  scale_color_manual(values = c('#F0563A','#57585A','white','#ffe700')) +
  guides(color = "none", linetype = guide_legend(position = 'inside', title = NULL)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = '#BCBDC0'),
    # Atributos del texto del gráfico
    text = element_text(color = '#57585A', size = 14, family = 'Open Sans SemiCondensed SemiBold'),
    title = element_text(color = '#57585A', size = 16, family = 'Open Sans SemiCondensed Bold', hjust = 'center'),
    plot.caption = element_text(color = '#57585A', size = 10, family = 'Open Sans SemiCondensed SemiBold'),
    plot.subtitle = element_text(color = '#57585A', size = 10, family = 'Open Sans SemiCondensed SemiBold'),
    legend.position.inside = c(0.65,0.885),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) 


ggbackground(gg = g, 'images/background.jpg')

  
