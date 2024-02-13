
# Crear un conjunto de datos de ejemplo
set.seed(123)
datos <- data.frame(
  tmed = rnorm(13, 10, 2), # Datos de temperatura media
  prec = rnorm(13, 50, 10), # Datos de precipitación
  elev = runif(13, 0, 1000) # Datos de elevación
)

# Crear el gráfico
ggplot(datos, aes(x = prec, y = tmed)) +
  geom_point() +
  geom_contour(aes(z = elev)) # Agregar puntos para tmed vs prec
library(vegan)


?ordisurf


d <- ordisurf(aux_avg1[, c("mean_prec", "mean_tmed")], aux_avg1$elev)

?gg_ordisurf

aux_avg1$x <- scales::rescale(aux_avg1$mean_prec)
aux_avg1$y <- scales::rescale(aux_avg1$mean_tmed)

gg_ordisurf(aux_avg1[, c("x", "y")], env.var = aux_avg1$elev, binwidth = 100)


o <- vegan::ordisurf(aux_avg1[, c("mean_prec", "mean_tmed")] ~ aux_avg1$elev)




ordi.grid <- o$grid
ordi.data <- expand.grid(x = ordi.grid$x, y = ordi.grid$y)
ordi.data$z <- as.vector(ordi.grid$z)
df_surf <- data.frame(na.omit(ordi.data))
df_ord <- as.data.frame(scores(ord, display = "sites"))

o$grid


gg_ordisurf(o$grid[, c("x", "y")], o$grid$z)
str(o$grid)
df_surf

ggplot() +
stat_contour(data = df_surf, aes(x = x, y = y, z = z))
