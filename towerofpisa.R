# Normal distributions found in the Tower of Pisa
# www.overfitting.net
# https://www.overfitting.net/2026/01/distribuciones-normales-en-la-torre-de.html

library(Cairo)


# Function to plot N skewed weighted normal distributions and their sum
plot_skew_normals_and_sum <- function(
        xi = c(-0.2, 0.2),  # x axis locations
        omega = c(1, 1),  # widths (~stdev)
        alpha = c(-4, 4),  # skewness (asymmetry)
        weight = c(1, 1),  # normalization weights
        # When the sum of weights=1 the output sum distribution will be a valid
        # normalized probability density function with Area=1
        n = 5000, labels = FALSE  # resolution and labelling
) {
    # Skewed normal distribution function
    dskewnorm <- function(x, xi, omega, alpha) {
        z <- (x - xi) / omega
        2 / omega * dnorm(z) * pnorm(alpha * z)
    }
    
    # Calculations
    N = length(xi)  # number of curves
    from <- min(xi - 5*omega)
    to   <- max(xi + 5*omega)
    x <- seq(from, to, length.out = n)  # set x axis limits
    y=list()
    for (i in 1:N) y[[i]] <- weight[i] * dskewnorm(x, xi[i], omega[i], alpha[i])
    ysum <- Reduce(`+`, y)
    
    # Sanity checks
    SUMW = sum(weight)  # ensure sum distribution Area=1
    if (SUMW != 1) print(paste0("WARNING: weights do not sum 1 (sum = ",
                                SUMW, ")"))
    MINY = min(ysum)  # ensure no negative density values are reached
    if (MINY < 0) print(paste0("WARNING: sum reaches negative values (min = ",
                           MINY, ")"))
    
    # Plotting
    ymin=min(ysum, unlist(y))  # get y axis limits
    ymax=max(ysum, unlist(y))
    plot(x, ysum, type = "l", lwd = 8, ylim = c(ymin, ymax),
         xlab = "", ylab = "", axes = labels, ann = labels, cex.axis = 2)
    cols <- topo.colors(N)
    for (i in 1:N) lines(x, y[[i]], lwd = 3, col=cols[i])
}


# EXAMPLES

# Adjustments to fit the staircase in the Tower of Pisa
CairoPNG("plot_skew_normals.png", width=1920, height=1080)
    plot_skew_normals_and_sum(
        xi = c(-3.7, 3.7),
        omega = c(5, 5),
        alpha = c(-0.8, 0.8)
    )
dev.off()

# Multiple distribution example
CairoPNG("plot_skew_normals_multi.png", width=1024, height=600)
    plot_skew_normals_and_sum(
        xi = c(-4, -2, 4, 9),
        omega = c(3, 1, 5, 4),
        alpha = c(0, 0, 0, 5),
        weight = c(1, 1, 1, 1)/4,  # ensure sum distribution Area=1
        labels = TRUE
    )
dev.off()

# Multiple distribution example with negative weights
CairoPNG("plot_skew_normals_multi_neg.png", width=1024, height=600)
    plot_skew_normals_and_sum(
        xi = c(-4, -2, 4, 9),
        omega = c(3, 1, 5, 4),
        alpha = c(0, 0, 0, 5),
        weight = c(0.15, -0.1, 1, -0.05),  # ensure sum distribution Area=1
        labels = TRUE
    )
dev.off()


