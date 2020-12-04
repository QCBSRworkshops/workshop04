col.plotcorr <- function(corr, ...){
  colsc=c(rgb(241, 54, 23, maxColorValue=255), 'white', rgb(0, 61, 104, maxColorValue=255))
  colramp = colorRampPalette(colsc, space='Lab')
  colors = colramp(100)
  cols=colors[((corr + 1)/2) * 100]
  my.plotcorr(corr, col=cols, diag='none', upper.panel="number", mar=c(0,0.1,0,0),...)
}
