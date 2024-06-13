## Visualize the negative logistic model parametrization:

# Define the sequence and calculate the model values
xx <- seq(-0.75, 5, by = 1/32)
yy <- 5 / (1 + exp((xx - 2) / 0.6)) # Negative logistic model
stopifnot(all.equal(yy, 5 / (1 + exp((xx - 2) / 0.6))))

# Load required graphics package
require(graphics)

# Set graphical parameters
op <- par(mar = c(0.5, 0, 3.5, 0))

# Plot the negative logistic model
plot(xx, yy, type = "l", axes = FALSE, ylim = c(0, 6), xlim = c(-1, 5),
     xlab = "", ylab = "", lwd = 2,
     main = "Parameters in the Negative Logistic Model")

# Add text for parameters
mtext(quote(list(phi[1] == "Asym", phi[2] == "xmid", phi[3] == "scal")))

# Define user coordinates
usr <- par("usr")

# Draw arrows for axes
arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)

# Add labels for axes
text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
text(-0.1, usr[4], "y", adj = c(1, 1))

# Draw horizontal asymptote
abline(h = 5, lty = 3)

# Add arrows and text for Asym
arrows(-0.8, c(2.1, 2.9), -0.8, c(0, 5), length = 0.1, angle = 25)
text(-0.8, 2.5, quote(phi[1]))

# Add segments and text for xmid
segments(c(2, 1.4, 1.4), c(0, 2.5, 3.5), c(2, 1.4, 2), c(2.5, 3.5, 2.5), lty = 2, lwd = 0.75)
text(2, -0.1, quote(phi[2]))

# Add arrows and text for scal
arrows(c(1.6, 1.8), 2.5, c(1.4, 2), 2.5, length = 0.08, angle = 25)
text(1.7, 2.5, quote(phi[3]))
text(1.2, 3, "1")

# Restore original graphical parameters
par(op)
