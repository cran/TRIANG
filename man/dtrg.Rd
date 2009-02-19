\name{dtrg}
\alias{dtrg}
\title{ Discrete triangular distributions }
\description{The function plots discrete triangular distributions}
\usage{
dtrg(c, a, h, y)
}
\arguments{
  \item{c}{ The center $c$ is a non-negative integer }
  \item{h}{ The order $h$ is a positive real number }
  \item{a}{ The arm $a$ is non-negative integer }
  \item{y}{ The vector of observations}
}
\details{
  The discrete triangular distribution have probability mass functions   

               $ P(y) = ((a+1)^h - (abs(y-c))^h)/A $

where $ A=(2*a+1)*(a+1)^h-2*sum(k^h), k=1,2,...,a,$ is the normalizing constant. 
The mean is equal to $c$ and the variance is given by
$V(a,h)=(1/A)(a(2*a+1)(a+1)^(h+1))/3 - 2*sum(k^h), k=0,1,2,...,a.$
 }
\value{
  The value returned is a vector of numbers in the interval [0,1]
  }
\references{
 Kokonendji, C.C., Senga Kiess\'e, T. and Zocchi, S.S. (2007). Discrete
triangular distributions and non-parametric estimation for probability mass
function. Journal of Nonparametric Statistics 19, 241--254.
 }
\author{ Tristan Senga Kiess\'e, Silvio S. Zocchi, C\'elestin C. Kokonendji  }
\examples{
##These examples provide some discrete triangular distributions of order 
## h  in {1/12, 1/2, 1, 2, 12} centered in c=5 with arm a=4 .
y=0:10
a=4
c=5
h=12 
T12=dtrg(c,a,h,y)
h=2 
T2=dtrg(c,a,h,y)
h=1
T1=dtrg(c,a,h,y)  ##The case h=1 provides a discrete triangular distribution 
##said to be pyramidal
h=1/2 
T_05=dtrg(c,a,h,y)
h=1/12 
T_012=dtrg(c,a,h,y)
plot(y,T1,xlab="y",ylab="Probab(y)",xlim=c(0,11),ylim=c(0,0.7),
main="Some Discrete triangular distributions centered in c=y=5 with arm a=4",cex.lab=1.5,
cex.axis=1.5,pch=20)
lines(y,T1,pch=20,lty=1)
points(y,T_012,pch=17)  
lines(y,T_012,lty=1)
points(y,T2) 
lines(y,T2,lty=2)
points(y,T12) 
lines(y,T12, lty=1)
points(y,T_05, pch=17) 
lines(y,T_05,lty=2)
op <- par(bg="white")
legend(8,0.7,c("h=1/12", "h=1/2","h=1",  "h=2", "h=12"),pch=c(17,17,20,1,1),
lty=c(1,2,1,2,1),cex = 1.2)
par(op)
## The function is defined as
function(c,a,h,y){  K=rep(0,length(y));                 
  u=0;
if (h==Inf)
{
 if (a==0)
  {K=rep(1,length(y))
  }
 else 
  {K=rep(1/(2*a+1),length(y))
  }
}
else
{   
 if (a==0)
  { u=0
  }
 else
  {for (k in 1:a)
   {u=u+k^h
    }
  } 
 A=(2*a+1)*(a+1)^h-2*u            # Normalizing constant 
  
 {for (j in 1:length(y))             # Loop in j for each observation y
    {
     if (y[j]>=(c-a) & y[j]<=(c+a)) # Support {c-a,...,c...,c+a}
        {K[j]= ((a+1)^h - (abs(y[j]-c))^h)/A  # Discrete triangular distribution
         }
     else{
          K[j]=0
	  } 
     }
     
   } 
 }
  return(K)
}
}
\keyword{distribution}


