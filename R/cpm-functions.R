# Composite Product Mapping functions

# FUNCTIONS
#   cpm.plot()    : Composite product mapping
#   cpm.rotate()  : helper function that rotates points in 2d space
#   cpm.se()      : convenience function for standard error


###############################################
# cpm.plot()
#
# All CPM functions follow this point
###############################################
#
# Authors: James L. Alford, and Christopher N. Chapman
# Author contact: Chris Chapman, cnchapman@gmail.com
#
# CITATION FOR cpm.plot().  Use the main citation, or this one as you prefer:
#   Alford, JL, and Chapman, CN. (2012). cpm.plot: composite product mapping
#     for R. [Computer software, version 0.2]
#
# OVERVIEW
#   Takes input variables + vector of "brands" and produces a "composite product
#     map", showing how the brands are "positioned" relative to one another by
#     the variables.
#   This is done by performing a MANOVA for [variables ~ brands] and extracting
#     the canonical discriminant functions.
#


###############################################
# cpm.rotate()     :: utility function
#   rotates (X,Y) Cartesian point matrix around origin in 2d
#
# INPUT PARAMETERS
#   points         = 2-column (X,Y) matrix of Cartesian coordinates
#   rotate         = amount to rotate clockwise around origin, in degrees
# OUTPUT
#   matrix of rotated points rotated around the origin
# EXAMPLE
#   cpm.rotate(cbind(c(1,2,3), c(5,3,4)), 90)

cpm.rotate <- function(points, rotate) {
  if (ncol(points) != 2) {
    warning("Points could not be rotated; not Cartesian in cbind(X,Y) format.")
    return(points)
  } else {
    theta <- -1.0 * pi * rotate/180
    x.rot <- points[,1]*cos(theta) - points[,2]*sin(theta)
    y.rot <- points[,1]*sin(theta) + points[,2]*cos(theta)
    points.rot <- cbind(x.rot, y.rot)
    return(points.rot)
  }
}

###############################################
# cpm.se()     :: utility function
#   standard error of the mean for a vector
#
cpm.se <- function(vec.in) {
  sqrt(var(vec.in) / length(vec.in))
}

###############################################
# cpm.plot()     :: Main Function
#
# INPUT PARAMETERS
#   data.in      = data
#   brand.ids    = name of column with factor to discriminate; must have >2
#                    levels
#   measure.vars = names of columns with predictors of brand.id
#                  e.g. measure.vars = names(data.in)[2:4]
#   zoom.out     = scale factor for vectors from discriminant functions
#                  try values 0.5 - 10.0 to get the best looking chart
#   rotate       = amount of clockwise rotation for the chart (in degrees)
#   trim         = proportion of the range of measure.vars to exclude from plot
#   xdim         = which DA function to show on X-axis (of the #levels-1
#                    discriminant functions)
#                  WARNING: only tested with X == DA function 1
#   ydim         = which DA function to show on Y-axis
#                  WARNING: only tested with Y == DA function 2
#   aspect.lock  = make chart rectangular? (X-axis same length as Y-axis)
#   coeffs       = use "std" (standardized) or "raw" DA coefficients?
#                  generally should use "std" for better interpretation
#                  WARNING: only tested with coeffs=="std". removed "raw"
#                    function in version 0.2.
#   plot.brands  = include brands on the plot? (if not, just plot dimensions)
#   plot.CI      = plot confidence intervals around the brand means?
#   plot.scatter = plot individual responses?
#   offset       = offset to position of brand labels (multiplication factor;
#                    may help with label overplot)
#   ci.width     = number of Z scores for confidence intervals (95% CI == 1.96)
#                    around the brand means
#
# RETURN VALUE
#   ggplot2 chart object (and it draws the chart upon return)
#
# EXAMPLE CALL
#   cpm.plot(iris, "Species", names(iris)[1:4], zoom.out=10,
#            title.legend="Species")

cpm.plot <- function(data.in, brand.ids, measure.vars,
                     zoom.out = 1, trim = 0.0, xdim = 1, ydim = 2,
                     aspect.lock = TRUE, rotate = 0, coeffs = "std",
                     plot.brands = TRUE, plot.CI = FALSE, plot.scatter = TRUE,
                     offset = 1.0, ci.width = 1.96,
                     fontsize.vector=6, fontsize.brand=8,
                     title.main = "Perceptual Map", title.legend = "Brands") {

  require(grid)      # for arrow functionality
  require(ggplot2)   # for plots
  require(candisc)   # for discriminant extraction
  require(ggrepel)   # to separate text on plot

  # extract needed data from larger DF and remove NAs
  data.use <- na.omit(data.in[ ,c(brand.ids, measure.vars) ])

  # core discrimination model
  manova.out <- manova( as.matrix(data.use[ ,measure.vars]) ~
                          factor(data.use[ ,brand.ids]), data=data.use)

  # extract the discriminant functions. don't change! it seems very finicky
  candisc.obj <- candisc(manova.out, term="factor(data.use[, brand.ids])")

  # Calculate means, and trim data frame for ggplot
  means.points <- candisc.obj$means[ ,c(xdim, ydim)]
  if (!isTRUE(all.equal(rotate, 0))) {
    points.rot <- cpm.rotate(means.points[ ,1:2], rotate)
    means.points[ ,1] <- points.rot[ ,1]
    means.points[ ,2] <- points.rot[ ,2]
  }

  names(means.points) <- c("xDim","yDim")

  # Calculate discriminant function SEs if needed
  if (plot.CI) {
    # need a temporary extraction, b/c for some reason ...
    CI.scores.tmp <- candisc.obj$scores
    # ... it won't directly index from above :(
    CI.scores     <- CI.scores.tmp[ ,c(1, xdim+1, ydim+1)]
    # rotate the points if needed
    if (!isTRUE(all.equal(rotate, 0))) {
      CI.scores.rot <- cpm.rotate(CI.scores[ ,2:3], rotate)
      CI.scores[ ,2] <- CI.scores.rot[ ,1]
      CI.scores[ ,3] <- CI.scores.rot[ ,2]
    }
    # find confidence intervals
    CI.points1 <- ci.width * tapply( CI.scores[ ,2], CI.scores[ ,1], cpm.se)
    CI.points2 <- ci.width * tapply( CI.scores[ ,3], CI.scores[ ,1], cpm.se)
    CI.points <- cbind(CI.points1, CI.points2)

    CI.ends.upper <- means.points + CI.points
    CI.ends.lower <- means.points - CI.points
    CI.ends <- cbind(means.points, CI.ends.upper, CI.ends.lower)
    names(CI.ends) <- c("xDim", "yDim", "xDim.up", "yDim.up", "xDim.low",
                        "yDim.low")
  }

  # Calculate attribute points, and trim data frame for ggplot
  if (coeffs == "std") {
    attribute.points.1 <- as.data.frame(candisc.obj$coeffs.std)
    if (!isTRUE(all.equal(rotate, 0))) {
      points.rot <- cpm.rotate(attribute.points.1[,1:2], rotate)
      attribute.points.1[,1] <- points.rot[,1]
      attribute.points.1[,2] <- points.rot[,2]
    }
    attribute.points.1 <- zoom.out*attribute.points.1

  } else {
    ## Raw score functionality is deprecated in v0.2
    ## 'raw' coeffs only make sense if raw means are also calculated,
    ##   - candisc function only reports standardized means
    # } else if(coeffs == "raw") {
    #  attribute.points.1 <- as.data.frame(zoom.out*candisc.obj$coeffs.raw)

    ### ==> placeholder for raw or other coefficient handling in the future
    warning(paste("Error: undefined coeffs parameter specified.",
                  "Only 'std' is supported at this time."))

    # for now just do the same thing as "std" coeffs
    attribute.points.1 <- as.data.frame(candisc.obj$coeffs.std)
    if (!isTRUE(all.equal(rotate, 0))) {
      points.rot <- cpm.rotate(attribute.points.1[,1:2], rotate)
      attribute.points.1[,1] <- points.rot[,1]
      attribute.points.1[,2] <- points.rot[,2]
    }
    attribute.points.1 <- zoom.out * attribute.points.1
  }
  attribute.points.init <- attribute.points.1[ , c(xdim, ydim) ]
  names(attribute.points.init) <- c("xDim", "yDim")
  x.start <- rep(0, nrow(attribute.points.init))
  y.start <- rep(0, nrow(attribute.points.init))
  attribute.points <- cbind(attribute.points.init, x.start, y.start)

  # Find max and min points for graph limits
  x.min.init <- min(0, min(means.points$xDim), min(attribute.points$xDim))
  x.max.init <- max(0, max(means.points$xDim), max(attribute.points$xDim))
  y.min.init <- min(0, min(means.points$yDim), min(attribute.points$yDim))
  y.max.init <- max(0, max(means.points$yDim), max(attribute.points$yDim))
  # Add 10% pad to X and Y dimentions for nicer plotting
  x.padding <- 0.1 * (x.max.init - x.min.init)
  y.padding <- 0.1 * (y.max.init - y.min.init)
  x.min <- x.min.init - x.padding
  x.max <- x.max.init + x.padding
  y.min <- y.min.init - y.padding
  y.max <- y.max.init + y.padding

  # Add text vector to data frame for ggplot
  point.text   <- row.names(means.points)
  means.points <- cbind(means.points, point.text)
  means.points[ ,1] <- means.points[ ,1] * offset
  means.points[ ,2] <- means.points[ ,2] * offset

  # Trim attributes if needed, and add text vector to data frame for ggplot
  att.distance          <- sqrt(attribute.points$xDim^2 +
                                  attribute.points$yDim^2)
  attribute.plot.points <- as.data.frame(
    attribute.points[att.distance >= quantile(att.distance, (trim)), ] )
  att.distance.alpha    <- 0.25 + att.distance * (0.75 / (max(att.distance)))
  arrow.text            <- row.names(attribute.plot.points)
  att.distance.alpha    <- att.distance.alpha[att.distance >=
                                                quantile(att.distance, (trim))]
  attribute.plot.points <- cbind(attribute.plot.points, arrow.text,
                                 att.distance.alpha)

  # rescale axes if needed
  if (aspect.lock) {
    x.min <- min(x.min, y.min)
    y.min <- x.min
    x.max <- max(x.max, y.max)
    y.max <- x.max
  }

  # build the ggplot2 plot, layer at a time
  #   1. basic structure plus dimensions
  #   2. individual scatter plot of responses, if desired ("plot.scatter")
  #   3. brand centroids, if desired ("plot.brands")
  #   4. brand confidence intervals, if desired ("plot.CI")
  #
  # basic plot with dimensions and nothing else
  cpm.p <- ggplot() +
    # label titles and axes
    labs(colour = title.legend) +
    labs(title = title.main) +
    theme(legend.text = element_text(size=12)) +
    theme(plot.title = element_text(size=20, lineheight=1, face="bold")) +
    theme(axis.text.x = element_blank(), axis.title.x=element_blank())  +
    theme(axis.text.y = element_blank(), axis.title.y=element_blank())  +
    # draw the dimensional arrows from origin
    geom_segment(data = attribute.plot.points,
                 aes(x=x.start, y=y.start, xend=xDim, yend=yDim),  # cut: alpha
                 lwd=1, arrow=arrow(length=unit(0.3,"cm"), angle=30)) +
    # label the dimensional arrows
    geom_text_repel(data = attribute.plot.points,
                    aes(x=xDim, y=yDim, label=arrow.text),  # cut: alpha
                    # hjust=0.5, vjust=1.5,
                    size = I(fontsize.vector)) +
    # set the chart boundaries
    coord_cartesian(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) +
    # nice background
    theme(panel.background = element_rect(colour="white", fill="grey95"))

  if (plot.scatter) {
    # find individual scores
    ind.points <- candisc.obj$scores[ ,c(1, xdim+1, ydim+1)]
    if (!isTRUE(all.equal(rotate, 0))) {
      points.rot <- cpm.rotate(ind.points[ ,2:3], rotate)
      ind.points[ ,2] <- points.rot[ ,1]
      ind.points[ ,3] <- points.rot[ ,2]
    }

    names(ind.points) <- c("this.group", "xDim", "yDim");
    # scatter plot of individual responses
    cpm.p <- cpm.p +
      geom_point(data = ind.points,
                 aes(x=xDim, y=yDim, colour=factor(this.group)),
                 size=4, alpha=0.5)
  }
  if (plot.brands) {
    # label the centroids (brands)
    cpm.p <- cpm.p +
      geom_point(data=means.points, aes(x=xDim, y=yDim), pch=22,    # points
                 colour=I("blue"), fill=I("blue"), size=3) +
      geom_text(data=means.points, 																  # labels
                aes(x=xDim, y=yDim, label=point.text),
                hjust=0.5, vjust=1.5, size = I(fontsize.brand), colour="darkred")
  }
  if (plot.CI) {
    cpm.p <- cpm.p +
      geom_segment(data=CI.ends,
                   aes(x=xDim, y=yDim.low, xend=xDim, yend=yDim.up), # vertical arrows
                   lty=3, lwd=2, colour=I("blue")) +
      geom_segment(data=CI.ends,
                   aes(x=xDim.low, y=yDim, xend=xDim.up, yend=yDim), # horiz arrows
                   lty=3, lwd=2, colour=I("blue")) +
      geom_rect(data=CI.ends,
                mapping=aes(xmin=xDim.low, xmax=xDim.up,
                            ymin=yDim.low, ymax=yDim.up),   # shaded boxes
                fill=I("lightblue"), color="black", alpha=0.2)
  }

  return(cpm.p)
} # end cpm.plot()
