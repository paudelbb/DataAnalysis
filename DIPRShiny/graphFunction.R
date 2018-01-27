#=====================================================================================================
# Graph functions to get population proliferaion dynamics with error bar
# Takes summary output that has the mean +-se or sd as desired;
#=====================================================================================================
require(Hmisc)
graph <- function(data, time, nl2, Conc, title, sub, col=col, xmin, xmax, ymin, ymax)
{
  y_min =  ymin
  y_max =  ymax
  x_min =  xmin
  x_max =  xmax
  d = data
  plot(  time, nl2, 
         xlim=c(x_min,x_max), ylim=c(y_min, y_max),
         xlab='',ylab='',
         main=title, sub=sub, col=col)
  with (
    data = d, expr = errbar(Time, nl2, nl2+sd, nl2-sd, add=T, pch=16, cap=.002, col=col))
  for(tc in unique(Conc))
  {
    ccc <- Conc[tc==Conc]
    lines(time[tc==Conc], nl2[tc==Conc], col= col, lwd=1.0, lty=1)
    text((max(d$Time)-0), nl2[tc==Conc][length(ccc)], label=paste0(ccc), pos=4, cex=0.8)
  }
}
#=====================================================================================================