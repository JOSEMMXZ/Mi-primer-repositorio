# Paquete necesario para graficar
install.packages("lpSolve")   # Para resolver LP
library(lpSolve)

# Definimos coeficientes de la función objetivo
func_obj <- c(40, 30)   # 40x + 30y

# Restricciones
matriz_restr <- matrix(c(2, 1,   # 2x + y <= 100
                         1, 2),  # x + 2y <= 80
                       nrow = 2, byrow = TRUE)

direcciones <- c("<=", "<=")
rhs <- c(100, 80)  # Lado derecho de las restricciones

# Resolver con lpSolve
sol <- lp("max", func_obj, matriz_restr, direcciones, rhs)
sol$solution   # Valores de x y y
sol$objval     # Valor máximo de Z

# ---------------------------
# GRAFICAR EL MÉTODO GRÁFICO
# ---------------------------
# Cargar librería gráfica
install.packages("ggplot2")
library(ggplot2)

# Restricciones como rectas
x <- seq(0, 60, length.out = 100)
y1 <- (100 - 2*x)     # de 2x + y <= 100
y2 <- (80 - x)/2      # de x + 2y <= 80

# DataFrame
df <- data.frame(x, y1, y2)

# Gráfico
ggplot(df, aes(x)) +
  geom_line(aes(y = y1), color="blue") +
  geom_line(aes(y = y2), color="red") +
  geom_abline(intercept=0, slope=0, color="black") +
  geom_abline(intercept=0, slope=Inf, color="black") +
  coord_cartesian(xlim = c(0,60), ylim = c(0,60)) +
  labs(title="Método Gráfico en RStudio",
       y="y (Producto B)", x="x (Producto A)") +
  geom_point(aes(x=sol$solution[1], y=sol$solution[2]), color="green", size=4)

