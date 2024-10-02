# Fijar directorio de trabajo
setwd("~/Downloads/presidenta_CSP")

# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar rvest                                                       
if(require(rvest) == FALSE){                                                
  install.packages('rvest')                                                 
  library(rvest)                                                            
}else{                                                                          
  library(rvest)                                                            
}         

# Instalar - Cargar tidytext                                                       
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}   

# Instalar - Cargar ggwordcloud                                                       
if(require(ggwordcloud) == FALSE){                                                
  install.packages('ggwordcloud')                                                 
  library(ggwordcloud)                                                            
}else{                                                                          
  library(ggwordcloud)                                                            
}

# Instalar - Cargar ggimage                                                       
if(require(ggimage) == FALSE){                                                
  install.packages('ggimage')                                                 
  library(ggimage)                                                            
}else{                                                                          
  library(ggimage)                                                            
}

# Datos -------------------------------------------------------------------
# Extraer la versión estenográfica del mensaje de la presidenta
discurso = httr::GET(
  # Definir la url
  url = 'https://www.gob.mx/',
  # Definir el path
  path = 'presidencia/articulos/version-estenografica-mensaje-de-la-presidenta-de-los-estados-unidos-mexicanos-claudia-sheinbaum-pardo?idiom=es'
) %>% 
  # Leer el html
  read_html() %>% 
  # Extraer los párrafos
  html_elements(xpath = '//div[@class="article-body"]/p') %>% 
  # Extraer el texto
  html_text() %>% 
  # Convertir en tibble
  as_tibble_col(column_name = 'texto') %>% 
  filter(!str_detect(texto,'VOCES')) %>% 
  slice_head(n = 166)

# Crear un diccionario para bigramas
bigramas = discurso %>% 
  # Generar bigramas
  unnest_tokens(
    input = texto,
    output = "bigrama", 
    token = "ngrams", 
    n = 2
  ) %>% 
  # Conteo de bigramas
  count(bigrama, sort = T) %>% 
  # Separar bigramas en palabras
  separate(bigrama, c('palabra_1', 'palabra_2'), sep = ' ') %>% 
  # Filtrar paralbras de paro
  filter(
    !palabra_1 %in% stopwords::stopwords('es',source = 'stopwords-iso'),
    !palabra_2 %in% stopwords::stopwords('es',source = 'stopwords-iso'),
    palabra_1 != palabra_2
  ) %>% 
  # Generar ngramas
  transmute(
    # Regresar al bigrama
    bigrama = paste(palabra_1, palabra_2, sep = ' '),
    # Crear el monograma
    monograma = paste(palabra_1, palabra_2, sep = '_'),
    n = n
  ) %>% 
  filter(
    n > 1,
    !str_detect(bigrama, '\\d')
  )

# Crear un diccionario de monogramas
monogramas = pull(bigramas, monograma)
names(monogramas) = pull(bigramas, bigrama)

# Remplazar bigramas
tokens = discurso %>% 
  # Remplazar bigramas por monogramas
  mutate(
    texto = tolower(texto),
    texto = str_remove_all(texto, "presidenta claudia sheinbaum pardo"),
    texto = str_replace_all(texto, monogramas)
    ) %>% 
  # Generar bigramas
  unnest_tokens(
    input = texto,
    output = "token", 
    token = "words"
  ) %>% 
  # Conteo de bigramas
  count(token, sort = T) %>% 
  # Filtrar paralbras de paro, dígitos y tokens poco mencionados
  filter(
    !token %in% stopwords::stopwords('es',source = 'stopwords-iso'),
    !str_detect(token, '\\d'),
    n > 1
  ) %>% 
  mutate(
    token = str_replace_all(token, '_',' '),
    token = toupper(token)
    )


# Visualización -----------------------------------------------------------
# Crear un lienzo en blanco
ggplot() +
  # Crear la nube de palabras
  geom_text_wordcloud_area(
    # Usando los tokens del discurso
    data = tokens,
    # Fijar el tamaño y color por el logaritmo del conteo
    mapping = aes(label = token, size = log(n), color = log(n)),
    # Fijar la posición al rededor de la mascara
    mask = png::readPNG("inputs/mascara_fondo.png"),
    # Quitar tokens sobrantes
    rm_outside = TRUE,
    # Modificar la familia
    family = "Open Sans SemiBold"
  ) +
  # Agregar emblema de la mujer mexicana
  geom_image(
    mapping = aes(
      image = "inputs/joven_mexicana.png",
      # Centrar la imagen
      x = 0, y = 0), 
    # Tamaño y ajuste por ancho de imagen
    size = 1, by = "width"
    )  +
  labs(
    title = str_wrap('"Soy madre, abuela, científica y mujer de fe, y a partir de hoy, por voluntad del pueblo de México, la presidenta constitucional de los Estados Unidos Mexicanos"', 60),
    subtitle = '- Claudia Sheinbaum Pardo, 1 de octubre de 2024',
    caption = 'Fuente: elaboración propia con datos de Presidencia de la República\nAutor:@renorosgon'
    ) +
  # Mantener las proporciones fijas
  coord_fixed(ratio = 1) +   
  # Ajustar límites para las coordenadas
  xlim(-1, 1) + ylim(-1, 1) +   
  # Modificar colores del texto
  scale_color_gradient(low = "#6c0d33", high = "#a67f2c") + 
  # Escalar el texto por radio
  scale_radius(range = c(2, 16), limits = c(0, NA)) +
  # Modificar por tema vacío
  theme_void(base_size = 16, base_family = "Open Sans SemiBold") +
  # Modificar el tema
  theme(
    # Ajuste de fondo
    plot.background = element_rect(fill = "#FAFAFA", colour = NA),
    # Ajuste de color
    text = element_text(family = "Open Sans SemiBold", color = '#6F6F71'),
    # Ajuste de títulos
    plot.title = element_text(
      family = "Open Sans SemiBold Italic", 
      hjust = 1, 
      margin = margin(0,0,10,0)
      ),
    plot.subtitle = element_text(
      family = 'Open Sans Medium', 
      hjust = 1, 
      margin = margin(0,0,10,0)
      )
  )

# Guardar
ggsave(
  filename = 'output/presidenta_mexicana.png', 
  width = 1600, height = 1600, units = "px", dpi = 300
  )
