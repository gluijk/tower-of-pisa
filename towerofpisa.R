# Normal distributions found in the Tower of Pisa
# www.overfitting.net
# https://www.overfitting.net/2026/01/distribuciones-normales-en-la-torre-de.html

library(Cairo)


plot_two_skew_normals_and_sum <- function(
        xi1 = -0.2, xi2 =  0.2,  # location
        omega1 = 1, omega2 = 1,  # width (~stddev)
        alpha1 = -4, alpha2 = 4,  # skewness (asymmetry)
        n = 5000, labels = FALSE
) {
    
    # Skewed normal distribution function
    dskewnorm <- function(x, xi, omega, alpha) {
        z <- (x - xi) / omega
        2 / omega * dnorm(z) * pnorm(alpha * z)
    }
    
    
    # Calculations
    from <- min(xi1 - 5*omega1, xi2 - 5*omega2)
    to   <- max(xi1 + 5*omega1, xi2 + 5*omega2)
    x <- seq(from, to, length.out = n)
    
    y1 <- dskewnorm(x, xi1, omega1, alpha1)
    y2 <- dskewnorm(x, xi2, omega2, alpha2)
    ysum <- y1 + y2
    

    # Plotting
    plot(x, ysum, type = "l", lwd = 9,
         axes = labels, ann = labels)
    lines(x, y1, lwd = 4, col='red')
    lines(x, y2, lwd = 4, col='blue')
}


# Adjustments to fit the staircase in the Tower of Pisa
CairoPNG("plot_skew_normals.png", width=2000, height=1024)
plot_two_skew_normals_and_sum(
    xi1 = -3.7, xi2 = 3.7,
    omega1=5, omega2=5,
    alpha1 = -0.8, alpha2 = 0.8
)
dev.off()
