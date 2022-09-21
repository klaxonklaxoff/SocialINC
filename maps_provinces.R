library(plotly)
library(sf)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

fig <- ggplotly(
  ggplot(nc) +
    geom_sf(aes(fill = AREA))
)

fig
