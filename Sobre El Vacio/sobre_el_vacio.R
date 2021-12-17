# Load libraries
if(require(tidyverse)==FALSE){
    install.packages("tidyverse")
    library(tidyverse)
  }else{
    library(tidyverse)
  }

if(require(tidytext)==FALSE){
    install.packages("tidytext")
    library(tidytext)
  }else{
    library(tidytext)
  }

if(require(textdata)==FALSE){
    install.packages("textdata")
    library(textdata)
  }else{
    library(textdata)
  }

if(require(ggforce)==FALSE){
    install.packages("ggforce")
    library(ggforce)
  }else{
    library(tidytext)
  }

if(require(stopwords)==FALSE){
    install.packages("stopwords")
    library(stopwords)
  }else{
   library(tidytext)
  }


# Get stopwords
stopwords = tibble(palabra = stopwords::data_stopwords_nltk$es)
# Get AFINN lexicon
afinn = read.csv("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv")


# Load our text
sobre_el_vacio = read.table(
  file = "sobre_el_vacio.txt", header=FALSE,
  encoding="UTF-8", 
  sep="\n", 
  quote=''
  ) %>%
  # Split by sentence.
  str_split(
    '\\. ', simplify = T
  ) %>%
  # Transpose our matrix
  t() %>%
  # Make a tibble
  tibble(
    texto = .
  ) %>% 
  # Create and manipulate variables
  mutate(
    # Set line number
    linea = row_number(),
    # Trim text
    texto = str_trim(
      # Leave letters only
      str_remove_all(
        # Set everything to lower
        tolower(
          texto
        ), 
        '[^[:alpha:] ]'),
      'both')
  ) %>%
  # Tokenize sentences
  unnest_tokens(
    palabra, texto
  ) %>%
  # Count words by sentence
  with_groups(
    .groups = linea,
    mutate,
    palabras = n()
  ) %>%
  # Join the lexicon
  left_join(afinn, by = c('palabra'='Palabra')) %>%
  # Get average polarity ant total words by sentence
  with_groups(
    .groups = linea,
    summarise,
    polaridad = mean(Puntuacion, na.rm = T),
    palabras = unique(palabras)
  ) %>%
  # Replace NAs for 0
  mutate(
    polaridad = replace_na(polaridad, 0),
    palabras = replace_na(palabras, 0),
    # Set color by polarity
    color = if_else(polaridad >0, 'positivo', 'negativo')
  ) %>%
  # Order by line
  arrange(linea) %>%
  # Redifine the line number
  mutate(linea = row_number())

# GGplot this
sobre_el_vacio %>%
  ggplot(
    aes(
      # Our x axis is always the line
      x = linea
    )
  ) +
  # Columns will use the polarity score
  geom_col(
    aes(      
      y = polaridad,
      fill = color),
    width = 1) +
  # Ser fill color
  scale_fill_manual(values = c('#00694Bff','#213A7Dff'))+
  # Points will use the number of words as size
  geom_point(
    aes(
      y = min(polaridad) - 2,
      size = palabras
    ), col = 'gray85'
  ) +
  # Choose size range
  scale_size(range = c(4,18)) +
  # This will allow us to get 3/4 circle
  scale_x_continuous(
    breaks = c(1,51),
    limits = c(0.45,67.15)
    )+
  # Let's draw a thin line
  geom_line(
    aes(x = seq(1,51),
        y = 0),
    lwd=0.005,
    linetype = 'dotted',
    col = 'gray60'
  ) +
  # This allow you to make a donut chart
  ylim(-20,3) +
  coord_polar(
    clip = 'off',
    start = 4.66
  ) +
  # Just stay with the graph
  theme_void() +
  # Remove the legend
  theme(
    legend.position = 'none'
  ) 

# I had to save it like this since the typeface I used is open source so I finished on Adobe Illustrator.
# If you know a way to actually load .otf on R it would be verymuch apreciated to put it hew
ggsave('~/Desktop/vacio.svg',
       # Max resolution with transparent background
       units="cm",  dpi = 300, bg="transparent",
       #Set png dimensions (this one is a square)
       width = 60, height = 60)
