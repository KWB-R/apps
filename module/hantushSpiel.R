library(kwb.hantush)

gamedatafile <- "C:/Users/mrustl/Documents/WC_AWS_Rrocks/RProjects/apps/gwamanager/data/spiel.RData"

# Save data for game -----------------------------------------------------------
if (FALSE) 
{
  gwa <- kwb.hantush::hantushDistancesBaseProps(
    x = seq(-800, 800, 0.5),
    baseProps = getGameParameters() 
  )

  save(gwa, file = gamedatafile)
}

# Spielen ----------------------------------------------------------------------

houseConfig <- list(
  distances = c(300, 96.5),
  houselengths = c(100, 200),
  depths = c(2, 7.5),
  heights = c(3, 9),
  roofoffsets = c(10, 2)
)

if (FALSE)
{
  load(gamedatafile)

  gwaData <- gwa$dat

  beckenPosition <- -234.5
  
  for (beckenPosition in seq(-300, 300, by = 20)) {
    gameResult <- hanstushSpiel(gwaData, beckenPosition = beckenPosition, flurabstand = 13)
    
    getFloodInCellar(gameResult,houseConfig)
    plotGameResult(
      gameResult, houseConfig, xlim = c(-400, 400), 
      basinLength = gwa$baseProps$basinLength
    )
  }
}

# getFloodInCellar -------------------------------------------------------------
getFloodInCellar <- function(gameResult, houseConfig)
{
  ileft <- max(which(gameResult$x <= -houseConfig$distances[1]))
  iright <- min(which(gameResult$x >= houseConfig$distances[2]))
  
  gwLeft <- gameResult$GWStand_unter_GOK[ileft]
  gwRight <- gameResult$GWStand_unter_GOK[iright]
  
  floodleft <- max(c(0, gwLeft + houseConfig$depths[1]))
  floodright <- max(c(0, gwRight + houseConfig$depths[2]))
  floodaverage <- mean(c(floodleft, floodright))
  
  list(left = floodleft, right = floodright, average = floodaverage)
}

# getGameParameters ------------------------------------------------------------
getGameParameters <- function()
{
  kwb.hantush::baseProperties(
    time = 100, 
    basinWidth = 50, 
    basinLength = 50,
    infiltrationRate = 4.3, 
    horizConductivity = 43.2, 
    iniHead = 40,
    specificYield = 0.2, 
    numberTimeSteps = 10
  )
}

# hanstushSpiel ----------------------------------------------------------------
hanstushSpiel <- function(gwaData, beckenPosition = 0, flurabstand = 4) 
{
  gwaData$x <- gwaData$x + beckenPosition
  gwaData$GWStand_unter_GOK <- - flurabstand + gwaData$WLincrease
  
  paras <-  list(
    beckenPosition = beckenPosition, 
    flurabstand = flurabstand
  )
  
  structure(gwaData, paras = paras)
}

# plotGameResult ---------------------------------------------------------------
plotGameResult <- function
(
  gameResult,  
  houseConfig,
  xlim = c(-400, 400),
  ylim = NULL,
  basinLength
)
{
  parameters <- attr(gameResult, "paras")

  if (is.null(ylim)) {
    ylim <- c(-(parameters$flurabstand + 5), max(houseConfig$heights) + 5)
  }
  
  emptyPlot(xlim, ylim)
  
  drawBasin(position = parameters$beckenPosition, basinLength)
  
  drawHouses(houseConfig)

  abline(h = 0)
  #abline(v = parameters$beckenPosition)

  lines(gameResult$x, gameResult$GWStand_unter_GOK, col = "blue", pch = 16)  
  
  flood <- getFloodInCellar(gameResult, houseConfig)
  
  title(main = sprintf(
    "Water in cellar \nleft: %0.2f m, average: %0.2f m, right: %0.2f m",
    flood$left, flood$average, flood$right)
  )
  
  segments(
    x0 = parameters$beckenPosition, 
    y0 = 0, 
    y1 = -(parameters$flurabstand - 1), 
    lty = 2
  )
  
  text(
    x = parameters$beckenPosition, 
    y = -parameters$flurabstand, 
    labels = sprintf("x = %0.1f m", parameters$beckenPosition)
  )
}

# emptyPlot --------------------------------------------------------------------
emptyPlot <- function(xlim, ylim, ...)
{
  plot(
    NA,
    type = "n",
    las = 1, 
    ylim = ylim, 
    xlim = xlim,
    xlab = "Basin position",
    ylab = "Groundwater level below surface (meter)",
    ...
  )
}

# drawBasin --------------------------------------------------------------------
drawBasin <- function(position, basinLength, height = 2)
{
  rect(
    xleft = position - basinLength/2, 
    xright = position + basinLength/2, 
    ybottom = 0, 
    ytop = height,
    col = "blue"
  )
}

# drawHouses -------------------------------------------------------------------
drawHouses <- function
(
  houseConfig
)
{
  distances <- houseConfig$distances
  houselengths <- houseConfig$houselengths
  depths <- houseConfig$depths
  heights <- houseConfig$heights
  roofoffsets <- houseConfig$roofoffsets
  
  drawHouse(distances, houselengths, heights, depths, side = 1)
  drawRoof(distances, houselengths, heights, roofoffsets, side = 1)
  
  drawHouse(distances, houselengths, heights, depths, side = 2)
  drawRoof(distances, houselengths, heights, roofoffsets, side = 2)
}

# drawHouse --------------------------------------------------------------------
drawHouse <- function(distances, houselengths, heights, depths, side)
{
  if (side == 1) {
    xleft <- -(distances[side] + houselengths[side])
    xright <- -distances[side]
  }
  else {
    xleft <- distances[side]
    xright <- distances[side] + houselengths[side]
  }
  
  rect(
    ybottom = -depths[side],
    xleft = xleft, 
    xright = xright,
    ytop = heights[side]
  )
}

# drawRoof ---------------------------------------------------------------------
drawRoof <- function(distances, houselengths, heights, roofoffsets, side)
{
  if (side == 1) {
    x2 <- -(distances[side] + houselengths[side])
    x3 <- -distances[side]
  }
  else {
    x2 <- distances[side]
    x3 <- distances[side] + houselengths[side]
  }
  
  x1 <- x2 - roofoffsets[1]
  x4 <- x3 + roofoffsets[1]
  
  y1 <- heights[side]
  y2 <- heights[side] + roofoffsets[2]
  
  # Draw line segments of roof
  segments(
    c(x1,x2,x3,x1), 
    c(y1,y2,y2,y1),
    c(x2,x3,x4,x4),
    c(y2,y2,y1,y1),
    col = "red"
  )
}
