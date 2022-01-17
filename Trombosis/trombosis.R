# Cargamos nuestras librerías
if(require(tidyverse)==FALSE){
  install.packages("tidyverse")
  library(tidyverse)
}else{
  library(tidyverse)
}

# Cargamos los datos
biometria = readxl::read_excel("biometria_hematica.xlsx") %>%
  mutate(
    # Creamos nuestrosfactores
    grupo = ordered(grupo,
                   levels = c('formula roja', 'formula blanca', 'formula blanca pcts', 
                              'serie plaquetaria', 'ttpa', 'fibrogeno de claus', 
                              'quimica sanguinea','electrolitos'),
                    ),
    factor = fct_reorder(
                    factor(
                      paste0(factor,'(',unidades,')')
                      ), as.numeric(grupo)),
  resultado_normalizado = (resultado - minimo) / (maximo - minimo)
  )

# Graficamos 
biometria %>%
  ggplot(
    group = grupo
  ) +
  geom_point(
    aes(
      x = reorder(factor(factor), grupo),
      y = resultado_normalizado,
      col = grupo
    ),
    size = 5
  ) +
  scale_color_viridis_d(option = 'B') +
  geom_hline(yintercept = c(0,1)) +
  scale_y_continuous(limits = c(-0.5,1.25)) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank() 
  )

# Guardamos
ggsave('rangos.svg', dpi = 300, width =  67, units = 'cm')
