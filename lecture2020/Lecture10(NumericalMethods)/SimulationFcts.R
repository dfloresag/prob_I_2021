#######################################################################
# R fucntions for "Simulation"
#######################################################################
# Reference: "UniGE.ProbaI.2016/Cours/Simulation"
#######################################################################
# Last modified: 09.05.2016
#######################################################################
# Warranty: none.
#######################################################################
# Contact: Prof. Dr. Diego Kuonen, CStat PStat CSci, Statoo Consulting,
#          CH - www.statoo.info - kuonen@statoo.com
#######################################################################

particule = function(nb.depl=10, nb.chem=100, histo=FALSE, theo=FALSE, interval=0)
{

  par(mfrow = c(1, 1), pty = 's')

  if (histo) 
  {
    x.body <- c(0, nb.depl+0.55*nb.depl)
  }
  else
  {
    x.body <- c(0,nb.depl)     
  }

  y.body <- c(-nb.depl, nb.depl)

  x <- (0:nb.depl)
  y <- rep(0, nb.depl+1)
  plot(x, y, type="n", ylim=y.body, xlim=x.body, xlab="", ylab="")
  title(paste("nb.depl. =",nb.depl,"       nb.chem. =", nb.chem))

  freq <- rep(0, length=nb.depl+1)

  for (i in (1:nb.chem))
  {
    
    u <- runif(nb.depl)
    y.new <- rep(-1, length=nb.depl)
    y.new[u > 0.5] <- 1

    y.new <- cumsum(y.new)

    fin <- y.new[nb.depl]

    y.new <- c(0, y.new)    

    if (i>1)
    {
      lines(x, y.old, lwd=3,col="blue")
      lines(x, y.old, lwd=1)
    }

    lines(x, y.new, lwd=3, col="orange")

    y.old <- y.new

    freq[(fin+nb.depl)/2+1] <- freq[(fin+nb.depl)/2+1] + 1

    Sys.sleep(interval)

  }
  
  lines(x, y.new, lwd=3, col="blue")

  if (histo)
  {
    freq.th <- dbinom(0:nb.depl,nb.depl,1/2)*nb.chem

    max.freq <- max(freq,freq.th)

    for (k in (1:(nb.depl+1)))
    {
      x.emp <- (1.10*nb.depl) +  0.45*nb.depl/max.freq*c(0,freq[k])
      x.the <- (1.10*nb.depl) +  0.45*nb.depl/max.freq*c(0,freq.th[k])

      h <- 2*(k-1)-nb.depl
      y <- c(h,h)

      lines(x.emp, y, lty=2, lwd=3)
      if (theo)
      {
        lines(x.the,y-0.015*nb.depl,lwd=2, col="red")
      }
      text(1.05*nb.depl,h,freq[k])
    }
  }

  positions <- seq(-nb.depl,nb.depl,by=2)

  res <- data.frame("Positions"=positions, "Valeurs"=freq)

  res
}

################################################################################

flip.coin.DK = function (faces = 2, prob = NULL, border = "white", grid = "white", 
                         col = 1:2, type = "p", pch = 21, bg = "transparent", digits = 3) 
{
    nmax = ani.options("nmax")
    if (length(faces) == 1) {
        faces = as.factor(seq(faces))
    }
    else {
        faces = as.factor(faces)
    }
    if (length(faces) < 2) 
        stop("'faces' must be at least 2")
    lv = levels(faces)
    n = length(lv)
    res = sample(faces, nmax, replace = TRUE, prob = prob)
    frq = table(res)/nmax
    ylm = max(frq)
    x = runif(nmax, 1.1, 1.9)
    y = runif(nmax, 0, ylm)
    col = rep(col, length = n)
    y0 = numeric(n)
    s = seq(0, ylm, 1/nmax)
    for (i in 1:nmax) {
        dev.hold()
        plot(1, xlim = c(0, 2), ylim = c(0, ylm * 1.04), type = "n", 
            axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
        abline(v = 1)
        axis(1, (1:n - 0.5)/n, lv)
        axis(2)
        ##### mtext("Frequency", side = 2, line = 2)
        ##### mtext("Flip 'coins'", side = 4)
        k = as.integer(res[1:i])
        points(x[1:i], y[1:i], cex = 3, col = col[k], type = type, 
            pch = pch, bg = bg)
        text(x[1:i], y[1:i], res[1:i], col = col[k])
        y0[k[i]] = y0[k[i]] + 1
        rect(seq(n)/n - 1/n, 0, seq(n)/n, y0/nmax, border = border, 
            col = col)
        segments(0, s, 1, s, col = grid)
        abline(v = 1)
        axis(3, (1:n - 0.5)/n, paste(y0, " (", round(y0/nmax, 
            digits = digits), ")", sep = ""), tcl = 0, mgp = c(0, 
            0.5, 0))
        ###### axis(1, 1.5, paste("Number of Tosses:", i), tcl = 0)
        axis(1, 1.5, paste("Nombre de lancers:", i), tcl = 0)
        box()
        ani.pause()
    }
    invisible(list(freq = as.matrix(frq)[, 1], nmax = i))
}

################################################################################

roue = function(nb=1, histo=FALSE)
{
  random.angle <- runif(nb,0,2*pi)
  x <- cos(random.angle)
  y <- sin(random.angle)

  if(!histo) par(mfrow=c(1,1), pty='s')
  if(histo) par(mfrow=c(1,2), pty='s')
  
  plot(x,y,pch="+", col="red", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(-1,1),ylim=c(-1,1))  
  title("Exemple de la roue",cex=0.6)

  a <- seq(0,120,by=60)
  a <- a*2*pi/360

  for (aux in a)
  {
    x <- c(cos(aux),cos(aux+pi))
    y <- c(sin(aux),sin(aux+pi)) 
    lines(x,y)   
  }

  if(histo){

    random.angle <- random.angle*360/(2*pi)
    random.angle <- round(random.angle,2)

    hist(random.angle,breaks=seq(-1,361,length=7),xlim=c(-1,361),xaxt="n", col="cyan", 
         xlab="Angles aleatoires", main="Histogramme des\n angles aleatoires",
         ylab="Frequence absolue")
    axis(1,at=seq(0,360,by=60))
  }

  par(mfrow=c(1,1))

  random.angle
}

################################################################################

rSn <- function(r, n, pi){
  (rbinom(r, n, pi) - n*pi) / sqrt(n*pi*(1-pi))
}

################################################################################

lln.ani.DK = function (FUN = rnorm, mu = 0, np = 30, pch = 20, col.poly = "bisque", 
    col.mu = "gray", ylab=expression(bar(x)), col.pts="black", ...) 
{
    n = ani.options("nmax")
    m = x = NULL
    for (i in 1:n) {
        d = colMeans(matrix(replicate(np, FUN(i, mu)), i))
        m = c(m, d)
        x = rbind(x, range(d))
    }
    rg = range(m)
    xax = pretty(1:n)
    for (i in 1:n) {
        dev.hold()
        plot(1:n, ylim = rg, type = "n", xlab = paste("n =", 
            i), ylab = ylab, xaxt = "n")
        axis(1, xax[xax <= i])
        polygon(c(1:i, i:1), c(x[1:i, 1], x[i:1, 2]), border = NA, 
            col = col.poly)
        points(rep(1:i, each = np), m[1:(i * np)], pch = pch,
            col = col.pts,  ...)
        abline(h = mu, col = col.mu, lwd=2)
        ani.pause()
    }
}

################################################################################

quincunx.DK = function (balls = 200, layers = 15, pch.layers = 2, pch.balls = 19, 
    col.balls = sample(colors(), balls, TRUE), cex.balls = 2) 
{
    op = par(mar = c(1, 0.1, 0.1, 0.1), mfrow = c(2, 1))
    on.exit(par(op))
    if (ani.options("nmax") != (balls + layers - 2)) 
        warning("It's strongly recommended that ani.options(nmax = balls + layers -2)")
    nmax = max(balls + layers - 2, ani.options("nmax"))
    layerx = layery = NULL
    for (i in 1:layers) {
        layerx = c(layerx, seq(0.5 * (i + 1), layers - 0.5 * 
            (i - 1), 1))
        layery = c(layery, rep(i, layers - i + 1))
    }
    ballx = bally = matrix(nrow = balls, ncol = nmax)
    finalx = numeric(balls)
    for (i in 1:balls) {
        ballx[i, i] = (1 + layers)/2
        if (layers > 2) {
            tmp = rbinom(layers - 2, 1, 0.5) * 2 - 1
            ballx[i, i + 1:(layers - 2)] = cumsum(tmp) * 0.5 + 
                (1 + layers)/2
        }
        bally[i, (i - 1) + 1:(layers - 1)] = (layers - 1):1
        finalx[i] = ballx[i, i + layers - 2]
    }
    rgx = c(1, layers)
    rgy = c(0, max(table(finalx)))
    for (i in 1:ani.options("nmax")) {
        dev.hold()
        plot(1:layers, type = "n", ann = FALSE, axes = FALSE)
        points(layerx, layery, pch = pch.layers)
        points(ballx[, i], bally[, i], pch = pch.balls, col = col.balls, 
            cex = cex.balls)
        par(bty = "u")
        if (i < layers - 1) 
            plot.new()
        else {
            hist(finalx[1:(i - layers + 2)], breaks = 1:layers, 
                 xlim = rgx, ylim = rgy, main = "", xlab = "", 
                 ylab = "", ann = FALSE, axes = FALSE, col="lightgrey")
            # plot(density(finalx[1:(i - layers + 2)]))
        }
        ani.pause()
    }
    invisible(c(table(finalx)))
}

################################################################################

quincunx2.DK = function (balls = 200, layers = 15, pch.layers = 2, pch.balls = 19, 
    col.balls = sample(colors(), balls, TRUE), cex.balls = 2) 
{
    op = par(mar = c(1, 0.1, 0.1, 0.1), mfcol = c(4, 1))
    on.exit(par(op))
    if (ani.options("nmax") != (balls + layers - 2)) 
        warning("It's strongly recommended that ani.options(nmax = balls + layers -2)")
    nmax = max(balls + layers - 2, ani.options("nmax"))
    layerx = layery = newlayerx = newlayery = NULL
    flagi = ifelse(layers%%2, 1, 0)
    for (i in 1:layers) {
        if (flagi) {
            newi = ifelse(i%%2, 1, 2)
        }
        else {
            newi = ifelse(i%%2, 2, 1)
        }
        layerx = c(layerx, seq(0.5 * (i + 1), layers - 0.5 * 
            (i - 1), 1))
        layery = c(layery, rep(i, layers - i + 1))
        if (i > 1) {
            newlayerx = c(newlayerx, seq(0.5 * (newi + 1), layers - 
                0.5 * (newi - 1), 1))
            newlayery = c(newlayery, rep(i, layers - newi + 1))
        }
    }
    ballx = bally = matrix(nrow = balls, ncol = nmax)
    finalx = newfinalx = numeric(balls)
    for (i in 1:balls) {
        ballx[i, i] = (1 + layers)/2
        if (layers > 2) {
            tmp = rbinom(layers - 2, 1, 0.5) * 2 - 1
            ballx[i, i + 1:(layers - 2)] = cumsum(tmp) * 0.5 + 
                (1 + layers)/2
        }
        bally[i, (i - 1) + 1:(layers - 1)] = (layers - 1):1
        finalx[i] = ballx[i, i + layers - 2]
    }
    rgx = c(1, layers)
    rgy = c(0, max(table(finalx)))
    newballx = ballx
    diag(newballx) = finalx
    for (i in 1:balls) {
        tmp = rbinom(layers - 2, 1, 0.5) * 2 - 1
        tmp = ifelse(newballx[i, i] + cumsum(tmp) < rgx[2], tmp, 
            -1)
        tmp = ifelse(newballx[i, i] + cumsum(tmp) > rgx[1], tmp, 
            +1)
        newballx[i, i + 1:(layers - 2)] = newballx[i, i] + cumsum(tmp) * 
            0.5
        newfinalx[i] = newballx[i, i + layers - 2]
    }
    for (i in 1:ani.options("nmax")) {
        dev.hold()
        plot(1:layers, type = "n", ann = FALSE, axes = FALSE)
        points(layerx, layery, pch = pch.layers)
        points(ballx[, i], bally[, i], pch = pch.balls, col = col.balls, 
            cex = cex.balls)
        par(bty = "u")
        if (i < layers - 1) {
            plot.new()
        }
       else {
            hist(finalx[1:(i - layers + 2)], breaks = 1:layers, 
                xlim = rgx, ylim = rgy, main = "", xlab = "", 
                ylab = "", ann = FALSE, axes = FALSE, col="lightgrey")
        }
        if (i > (layers - 1)) {
            newi = i - layers + 1
            plot(1:layers, type = "n", ann = FALSE, axes = FALSE)
            points(newlayerx, newlayery, pch = pch.layers)
            points(newballx[, newi], bally[, newi] + 1, pch = pch.balls, 
                col = col.balls, cex = cex.balls)
            par(bty = "u")
            if (newi < layers - 1) 
                plot.new()
            else hist(newfinalx[1:(newi - layers + 2)], breaks = 1:layers, 
                xlim = rgx, ylim = rgy, main = "", xlab = "", 
                ylab = "", ann = FALSE, axes = FALSE, col="lightgrey")
        }
        else {
            plot(1:layers, type = "n", ann = FALSE, axes = FALSE)
            points(newlayerx, newlayery, pch = pch.layers)
            plot(1:layers, type = "n", ann = FALSE, axes = FALSE)
        }
        ani.pause()
    }
    invisible(list(top = c(table(finalx)), bottom = c(table(newfinalx))))
}

################################################################################
