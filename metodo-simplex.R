# -------------------------------------------------------
# Minimización con 2 variables (Método Simplex)
# -------------------------------------------------------

# install.packages("lpSolve")
# install.packages("ggplot2")

library(lpSolve)
library(ggplot2)

# --------- MODELO ---------
f.obj <- c(5, 8) # Z = 5x1 + 8x2

f.con <- matrix(c(1, 1,
                  2, 1),
                nrow = 2,
                byrow = TRUE)

f.dir <- c(">=", ">=")
f.rhs <- c(10, 15)

# --------- RESOLVER ---------
resultado <- lp("min", f.obj, f.con, f.dir, f.rhs)

cat("Solución óptima:\n")
print(resultado$solution)
cat("Valor mínimo de Z =", resultado$objval, "\n")

# --------- GRÁFICA ---------
x_vals <- seq(0, 20, 1)
rest1 <- 10 - x_vals         # x1 + x2 = 10
rest2 <- 15 - 2*x_vals       # 2x1 + x2 = 15

# Filtrar valores válidos (x2 >= 0)
rest1[rest1 < 0] <- NA
rest2[rest2 < 0] <- NA

# Puntos factibles (intersección de restricciones + ejes)
p1 <- c(0, 15)   # (0,15)
p2 <- c(5, 5)    # (5,5)  <-- óptimo
p3 <- c(15, 0)   # (15,0)

# Polígono de la región factible
df <- data.frame(
  x1 = c(p1[1], p2[1], p3[1]),
  x2 = c(p1[2], p2[2], p3[2])
)

# Punto óptimo
opt <- data.frame(x1 = resultado$solution[1],
                  x2 = resultado$solution[2])

# Graficar
ggplot() +
  geom_line(aes(x = x_vals, y = rest1), color = "blue", size = 1) +
  geom_line(aes(x = x_vals, y = rest2), color = "red", size = 1) +
  geom_polygon(data = df, aes(x1, x2), fill = "lightgreen", alpha = 0.4) +
  geom_point(data = opt, aes(x1, x2), color = "black", size = 4) +
  annotate("text", x = opt$x1 + 0.5, y = opt$x2,
           label = paste("Óptimo (", opt$x1, ",", opt$x2, ")", sep=""),
           hjust = 0, color = "black") +
  labs(title = "Método Simplex - Minimización (Región factible)",
       x = "x1", y = "x2") +
  xlim(0, 20) + ylim(0, 20)