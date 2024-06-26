## NAMESPACE stuff.

#' @import Rcpp
#' @importFrom CircStats rvm
#' @importFrom fields image.plot
#' @importFrom graphics image legend locator par points
#' @importFrom grDevices dev.off x11
#' @importFrom stats rbinom rpois runif
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom viridis viridis
#' @useDynLib acreworkshop
NULL

#' Conduct a survey to measure spatial covariates
#'
#' Creates a plot of Phnom Prich Wildlife Sanctuary and invites the
#' user to select sampling locations to measure covariates.
#'
#' @return A data frame with columns for the coordinates of the
#'     selected locations, and a column for each observed spatial
#'     variable.
#'
#' @param skip.wait For development purposes only. Don't set to
#'     \code{TRUE}!
#' @export
measure.covariates <- function(skip.wait = FALSE){
    x11()
    z <- rep(0, nrow(acreworkshop::ppws))
    cols <- "grey"
    n.locs <- 24
    message(paste("\nClick on the map to select", n.locs, "locations at which to measure spatial covariates.\n"))
    image_xyz(acreworkshop::ppws[, 1], acreworkshop::ppws[, 2], z, asp = 1, zlim = c(0, 1), col = cols)
    locs <- matrix(0, nrow = n.locs, ncol = 2)
    for (i in 1:n.locs){
        l <- locator(1)
        locs[i, ] <- c(l$x, l$y)
        points(locs[i, , drop = FALSE], pch = 16)
    }
    h <- readline("Are you happy with these selections? \n\nType 'y' to deploy your team to collect data. Type 'n' to reselect.\n")
    happy <- FALSE
    if (h == "yes" | h == "y" | h == "Y" | h == "Yes"){
        happy <- TRUE
    }
    if (!happy){
        stop("You did not comfirm your selections. Please re-run the function to try again.")
    }
    if (!skip.wait){
        message("Flying to Cambodia...")
        pb.flight <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.1)
            setTxtProgressBar(pb.flight, i)
        }
        message("\nHiking to Phnom Prich Wildlife Sanctuary...")
        pb.hike <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.hike, i)
        }
        message("\nDeploying fieldworkers...")
        pb.field <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.025)
            setTxtProgressBar(pb.field, i)
        }
        message("\nTaking elevation measurements...")
        pb.elevation <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.01)
            setTxtProgressBar(pb.elevation, i)
        }
        message("\nMeasuring canopy...")
        pb.canopy <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.02)
            setTxtProgressBar(pb.canopy, i)
        }
        message("\nIdentifying tree species...")
        pb.tree <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.03)
            setTxtProgressBar(pb.tree, i)
        }
        message("\n")
    }
    closest.points <- which.closest(locs)
    canopy.height <- round(canopy.height.ppws[closest.points], 1)
    elevation <- round(elevation.ppws[closest.points], 1)
    forest.type <- forest.type.ppws[closest.points]
    dev.off()
    data.frame(x = locs[, 1], y = locs[, 2], canopy.height = canopy.height,
               elevation = elevation, forest.type = forest.type)
}

#' Conduct a survey to collect acoustic detection data
#'
#' Creates a plot of Phnom Prich Wildlife Sanctuary and invites the
#' user to select locations at which to deploy listening posts.
#'
#' @return A list with two components: \code{captures} includes the
#'     detection data, and \code{traps} includes the listening post
#'     locations.
#'
#' @param skip.wait For development purposes only. Don't set to
#'     \code{TRUE}!
#' @export
conduct.survey <- function(skip.wait = FALSE){
    x11()
    z <- rep(0, nrow(acreworkshop::ppws))
    cols <- "grey"
    n.sessions <- 18
    message(paste("\nClick on the map to select", n.sessions,
                  "locations at which to deploy clusters of three listening posts.\n"))
    image_xyz(acreworkshop::ppws[, 1], acreworkshop::ppws[, 2], z, asp = 1, zlim = c(0, 1), col = cols)
    points(acreworkshop::villages.df, pch = 16)
    legend("topleft", legend = "Village", pch = 16)
    traps <- vector(mode = "list", length = n.sessions)
    for (i in 1:n.sessions){
        l <- locator(1)
        traps[[i]] <- cbind(x = c(l$x - 1000, l$x, l$x + 1000), y = rep(l$y, 3))
        points(traps[[i]], pch = 4)
    }
    h <- readline("Are you happy with these selections? \n\nType 'y' to deploy your team to conduct acoustic surveys. Type 'n' to reselect.\n")
    happy <- FALSE
    if (h == "yes" | h == "y" | h == "Y" | h == "Yes"){
        happy <- TRUE
    }
    if (!happy){
        stop("You did not comfirm your selections. Please re-run the function to try again.")
    }
    out <- sim.det(s.ppws, traps, c(20, 500, 50))
    if (!skip.wait){
        message("\nRedeploying fieldworkers...")
        pb.field <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.025)
            setTxtProgressBar(pb.field, i)
        }
        message("\nListening for gibbons...")
        pb.listen <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.listen, i)
        }
        message("\nMatching detections to calls...")
        pb.match1 <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.match1, i)
        }
        message("\nMatching calls to gibbon groups...")
        pb.match2 <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.05)
            setTxtProgressBar(pb.match2, i)
        }
        message("\nCollating detection data...")
        pb.collate <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.01)
            setTxtProgressBar(pb.collate, i)
        }
        message("\nFlying home...")
        pb.home <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(0.1)
            setTxtProgressBar(pb.home, i)
        }
        message("\nSnuggling chickens...")
        pb.chicken <- txtProgressBar(min = 0, max = 100, style = 3)
        for (i in 1:100){
            Sys.sleep(ifelse(i < 5, 1, 0.05))
            setTxtProgressBar(pb.chicken, i)
        }
        message("\n")
    }
    dev.off()
    out
}

#' Plot a spatial covariate
#'
#' Plots a spatial covariate at the points provided in the first
#' argument.
#'
#' @param df A data frame with measured or interpolated covariate
#'     values.
#' @param cov.name A character string, indicating which column in
#'     \code{df} to plot.
#' 
#' @export
plotcov <- function(df, cov.name){
    locs <- data.frame(df$x, df$y)
    if (nrow(df) < nrow(acreworkshop::ppws)){
        closest.points <- which.closest(locs)
        cov <- rep(NA, nrow(acreworkshop::ppws))
        cov[closest.points] <- df[[cov.name]]
    } else {
        cov <- df[[cov.name]]
    }
    if (is.character(cov)){
        cov <- as.numeric(as.factor(cov))
    }
    z <- rep(0, nrow(acreworkshop::ppws))
    image_xyz(acreworkshop::ppws[, 1], acreworkshop::ppws[, 2], z, col = "grey", asp = 1)
    col <- viridis(20)
    image_xyz(acreworkshop::ppws[, 1], acreworkshop::ppws[, 2], cov, col = col, asp = 1, add = TRUE)
    title(main = cov.name)
    if (cov.name == "forest.type"){
        legend("topleft", legend = c("deciduous", "evergreen"), pch = 15, col = c(col[1], col[length(col)]))
    } else {
        image_xyz_legend(acreworkshop::ppws[, 1], acreworkshop::ppws[, 2], cov, col = col)
    }
}

#' Interpolate a spatial covariate
#'
#' Conducts spatial interpolation to impute values of covariates
#' throughout Phnom Prich Wildlife Sanctuary.
#'
#' @return A data frame. The first two columns are x- and
#'     y-coordinates covering Phnom Prich Wildlife
#'     Sanctuary. Remaining columns provide interpolated covariate
#'     values.
#' 
#' @param df A data frame with measured covariate values.
#' 
#' @export
interpolate.covs <- function(df){
    data.frame(x = acreworkshop::ppws[, 1], y = acreworkshop::ppws[, 2], acre:::par_extend_create(loc.cov = df, mask = list(acreworkshop::ppws))$output$data$mask[, -c(1, 2)])
    
}

#' A grid of cells covering Phnom Prich Wildlife Sanctuary
#'
#' A data frame specifying x- and y-coordinates for a grid of cells
#' that covers Phnom Prich Wildlife Sanctuary.
#'
#' @format ## `ppws`
#' A data frame with two columns: x and y.
"ppws"

#' Locations of villages in Phnom Prich Wildlife Sanctuary
#'
#' A data frame specifying x- and y-coordinates for villages.
#'
#' @format ## `ppws`
#' A data frame with two columns: x and y.
"villages.df"

rank.models <- function(){
    file.names <- list.files(pattern = "RData")
    n.files <- length(file.names)
    D.ssq <- numeric(n.files)
    for (i in 1:n.files){
        load(file.names[i])
        if (any(ls() == "fit.final")){
            D.pred <- acre:::predict_D_for_plot(fit.final, new_data = acreworkshop::ppws)[, 3]
            D.ssq[i] <- sum((D.pred - D.ppws)^2)
        } else {
            D.ssq[i] <- NA
        }
        rm(fit.final)
    }
    data.frame(file.names, D.ssq)[order(D.ssq), ]
}

fit.final <- NULL

image_xyz <- function(x, y, z, ...){
    u.x <- sort(unique(x))
    u.y <- sort(unique(y))
    z.mat <- squarify(cbind(x, y), z)
    image(u.x, u.y, z.mat, ..., xlab = "x-coordinate", ylab = "y-coordinate")
}

image_xyz_legend <- function(x, y, z, col){
    u.x <- sort(unique(x))
    u.y <- sort(unique(y))
    z.mat <- squarify(cbind(x, y), z)
    image.plot(u.x, u.y, z.mat, col = col, legend.only = TRUE)
}

calc.dists <- function(points1, points2){
    apply(points1, 1, function(x) sqrt((x[1] - points2[, 1])^2 + (x[2] - points2[, 2])^2))
}

calc.bearings <- function(points1, points2){
    x.diff <- apply(points2, 1, function(x) x[1] - points1[, 1])
    y.diff <- apply(points2, 1, function(x) x[2] - points1[, 2])
    out <- atan(x.diff/y.diff)
    out[y.diff < 0] <- out[y.diff < 0] + pi
    out[y.diff >= 0 & x.diff < 0] <- out[y.diff >= 0 & x.diff < 0] + 2*pi
    out
}

which.closest <- function(locs){
    d <- calc.dists(locs, acreworkshop::ppws)
    apply(d, 2, function (x) which(x == min(x))[1])
}

sim.pop <- function(df, D.calc){
    mask <- acreworkshop::ppws
    ## Calculating density for every mask cell.
    D <- D.calc(df)
    ## Number of cells in the mask.
    n.cells <- nrow(mask)
    ## Width of each cell, assuming they're in a grid and the first
    ## two are adjacent.
    cell.width <- max(abs(c(mask[2, 1] - mask[3, 1], mask[2, 2] - mask[3, 2])))
    ## Cell area in square metres.
    cell.area <- cell.width^2
    ## Cell area in hectares.
    cell.area.ha <- cell.area/10000
    ## Expected number of animals in each cell.
    exp.n.per.cell <- D*cell.area.ha
    ## Simulating number of animals in each cell.
    n.per.cell <- rpois(n.cells, exp.n.per.cell)
    ## Animal locations, at first in the centre of each cell.
    s <- cbind(rep(mask[, 1], n.per.cell), rep(mask[, 2], n.per.cell))
    ## Total number of animals.
    n.animals <- nrow(s)
    ## Jittering so that the locations are uniform within cells.
    s[, 1] <- s[, 1] + runif(n.animals, -cell.width/2, cell.width/2)
    s[, 2] <- s[, 2] + runif(n.animals, -cell.width/2, cell.width/2)
    s
}

sim.det <- function(s, traps, pars){
    ## Number of traps.
    n.sessions <- length(traps)
    ## Number of sessions.
    n.traps <- sapply(traps, nrow)
    ## Number of animals.
    n.animals <- nrow(s)
    ## Extracting parameters.
    lambda0 <- pars[1]
    sigma <- pars[2]
    kappa <- pars[3]
        ## Distances between animals and traps.
    dists <- lapply(traps, function(x) calc.dists(s, x))
    ## Bearings between animals and traps.
    bearings <- lapply(traps, function(x) t(calc.bearings(x, s)))
    ## Creating capture histories.
    captures <- vector(mode = "list", length = n.sessions)
    for (i in 1:n.sessions){
        det.probs <- 1 - exp(-lambda0*exp(-dists[[i]]^2/(2*sigma^2)))
        capt.sess <- matrix(rbinom(n.animals*n.traps[i], 1, det.probs),
                            nrow = n.animals, byrow = TRUE)
        bearing.sess <- matrix(0, nrow = n.animals, ncol = n.traps[i])
        dets <- which(capt.sess == 1, arr.ind = TRUE)
        n.dets <- nrow(dets)
        session.sess <- rep(i, n.dets)
        occasion.sess <- rep(1, n.dets)
        ID.sess <- trap.sess <- bearing.sess <- numeric(n.dets)
        for (j in seq_len(nrow(dets))){
            ID.sess[j] <- dets[j, 1]
            trap.sess[j] <- dets[j, 2]
            bearing.sess[j] <- rvm(1, bearings[[i]][dets[j, 1], dets[j, 2]],
                                   kappa)
        }
        detected <- apply(capt.sess, 1, function(x) any(x == 1))
        captures[[i]] <- data.frame(session = session.sess,
                                    ID = ID.sess,
                                    occasion = occasion.sess,
                                    trap = trap.sess,
                                    bearing = bearing.sess)
    }
    captures <- do.call(rbind, captures)
    u.animals <- unique(captures$ID)
    n.det.animals <- length(u.animals)
    ID.ordered <- numeric(nrow(captures))
    for (i in 1:n.det.animals){
        ID.ordered[captures$ID == u.animals[i]] <- i
    }
    captures$ID <- ID.ordered
    captures <- captures[order(captures$ID), ]
    list(captures = captures, traps = traps)
}

compare.cov.to.truth <- function(df){
    ppar <- par(mfrow = c(3, 2))
    true.df <- data.frame(elevation = elevation.ppws,
                          canopy.height = canopy.height.ppws,
                          forest.type = forest.type.ppws)
    plotcov(df, "elevation")
    plotcov(true.df, "elevation")
    plotcov(df, "canopy.height")
    plotcov(true.df, "canopy.height")
    plotcov(df, "forest.type")
    plotcov(true.df, "forest.type")
    par(ppar)
}

compare.D.to.truth <- function(fit){
    ppar <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 6))
    df <- data.frame(D = D.ppws)
    plot(fit, type = "Dsurf", new.data = acreworkshop::ppws)
    plotcov(df, "D")
    par(ppar)
}
