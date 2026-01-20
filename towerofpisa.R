# Normal distributions found in the Tower of Pisa
# www.overfitting.net
# https://www.overfitting.net/2026/01/distribuciones-normales-en-la-torre-de.html

library(Cairo)


# Function to plot N skewed normal distributions (each Area=1) and their sum
plot_skew_normals_and_sum <- function(
    xi = c(-0.2, 0.2),  # x axis locations
    omega = c(1, 1),  # widths (~stdev)
    alpha = c(-4, 4),  # skewness (asymmetry)
    n = 5000, labels = FALSE  # resolution and labelling
) {
    # Skewed normal distribution function of Area=1
    dskewnorm <- function(x, xi, omega, alpha) {
        z <- (x - xi) / omega
        2 / omega * dnorm(z) * pnorm(alpha * z)
    }
    
    # Calculations
    N = length(xi)  # number of curves
    from <- min(xi - 5*omega)
    to   <- max(xi + 5*omega)
    x <- seq(from, to, length.out = n)
    y=list()
    for (i in 1:N) y[[i]] <- dskewnorm(x, xi[i], omega[i], alpha[i])
    ysum <- Reduce(`+`, y)
    
    # Plotting
    plot(x, ysum, type = "l", lwd = 8,  # ysum always >= y[[i]]
         xlab = "", ylab = "", axes = labels, ann = labels)
    cols <- topo.colors(N)
    for (i in 1:N) lines(x, y[[i]], lwd = 3, col=cols[i])
}


# Adjustments to fit the staircase in the Tower of Pisa
CairoPNG("plot_skew_normals.png", width=1920, height=1080)
    plot_skew_normals_and_sum(
        xi = c(-3.7, 3.7),
        omega = c(5, 5),
        alpha = c(-0.8, 0.8)
    )
dev.off()


# Multiple distribution example
CairoPNG("plot_skew_normals_multi.png", width=1280, height=800)
    plot_skew_normals_and_sum(
        xi = c(-4, -2, 4, 9),
        omega = c(3, 1, 5, 4),
        alpha = c(0, 0, 0, 5),
        labels = TRUE
    )
dev.off()


