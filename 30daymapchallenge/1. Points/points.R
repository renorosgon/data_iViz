# Fijar directorio de trabajo
setwd("~/Desktop/DataViz/30daymapchallenge/1. Points")

# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar ooklaOpenDataR                                                       
if(require(ooklaOpenDataR) == FALSE){                                                
  remotes::install_github("teamookla/ooklaOpenDataR")                                               
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
# Descaergar datos de banda ancha fija
internet_fijo = ooklaOpenDataR::get_performance_tiles(
  service = "fixed", 
  quarter = 3, year = 2024, 
  sf = FALSE
  ) %>% 
  # México bounding box
  filter(
    # X min , Y min
    tile_x > -117.12776, tile_y > 14.5388286402,
    # X max , Y max
    tile_x < -86.811982388, tile_y < 32.72083
  ) %>% 
  # Transformar a sf
  st_as_sf(
    coords = c('tile_x','tile_y'),
    # Proyección WGS84
    crs = st_crs(4326)
  )
  
# Estos son los polígonos de municipios del Marco Geoestadístico nacional
municipios = st_read('data/municipios') %>% 
  st_transform(crs = st_crs(internet_fijo))

# Hace un spatial join para juntar únicamente los que coinciden
data = st_join(x = municipios, y = internet_fijo, left = TRUE) %>% 
      # Elimninamos las geometrïas
      st_drop_geometry() %>% 
      with_groups(
        # Agrupamos por municipio
        .groups = c(CVEGEO, CVE_ENT, CVE_MUN, NOMGEO),
        # Agregamos las métricas ponderadas
        summarise,
        avg_download_kbps = sum(avg_d_kbps * tests) / sum(tests),
        avg_upload_kbps = sum(avg_u_kbps * tests) / sum(tests),
        avg_latency_ms = sum(avg_lat_ms * tests) / sum(tests),
        test = sum(tests),
        devices = sum(devices)
      )  

# Agregamos la información a los polígonos de municipios
data_sf = municipios %>% 
  left_join(y = data, by = join_by(CVEGEO, CVE_ENT, CVE_MUN, NOMGEO)) %>% 
  # Mutamos la geometría a centroides
  mutate(geometry = st_centroid(geometry))



# Gráfico -----------------------------------------------------------------
ggplot() +
  # Agregamos la capa espacial
  layer_spatial(
    # Aquí hago la transformación a megas para colorear
    data = mutate(data_sf, color = avg_upload_kbps/1000, color = ifelse(color > 100, 100, color)),
    # Agrega color y tamaños
    aes(color = color, size = test),
    # Quita la leyenda
    show.legend = FALSE, alpha = 0.8
  ) +
  # Agrega la escala geográfica
  annotation_scale(
    location = "bl",
    bar_cols = c('#535569','#64697b'),
    text_family = 'Montserrat',
    text_col = '#64697b'
  ) +
  # Agregar la leyenda (hecha en inkscape)
  geom_image(
    mapping = aes(
      image = "images/leyenda.png",
      # Centrar la imagen
      x = -113.75, y = 17.5), 
    # Tamaño y ajuste por ancho de imagen
    size = 0.25, by = "width"
  ) +
  # Agrega etiquetas
  labs(
    caption = 'Source:own elaboration with Ookla\nAuthor:@renorosgon'
  ) +
  # Agregar título a la leyenda (lo olvidé colocar en inkscape)
  annotate(
    geom = 'text',
    x =  -113.75, y = 20.5,
    # Agregar etiquetas
    label = "National Average",
    # Modificar detalles
    col = 'white', size = 3, family = 'Montserrat',
    hjust = 'center'
  )  +
  # Agregar título 
  annotate(
    geom = 'text',
    x = -102, y = 34,
    # Agregar etiquetas
    label = "Mexico's fixed broadband internet uplaod speed",
    # Modificar detalles
    col = 'white', size = 6, family = 'Montserrat',
    hjust = 'center'
  )  +
  # Agregar subtitulo
  annotate(
    geom = 'text',
    x = -102, y = 33.25,
    # Agregar etiquetas
    label = "Average speed by municipality in Megabytes per second. Bubble sizes show the total number of tests performed in each region.",
    # Modificar detalles
    col = 'white', size = 3, family = 'Montserrat',
    hjust = 'center'
  ) +
  # Modifica el gradiente de color
  scale_color_gradient2(
    low = '#A55EE1', mid = '#F472EF', high =  '#F371BA', midpoint = 20,
    breaks = c(0, 1, 5, 10, 20, 30, 50, 75, 100)
    ) +
  # Modifica el tema
  theme_void(base_family = 'Montserrat') +
  theme(
    # Cambia el fondo de la imagen
    plot.background = element_rect(fill = '#141526'),
    # Agrega el texto de los ejes
    axis.text = element_text(color = '#64697b', family = 'Montserrat', size = 10),
    # Atributos del texto del gráfico
    text = element_text(color = '#64697b', size = 12)
    )

