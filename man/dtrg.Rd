\name{dtrg}
\alias{dtrg}
\title{ Symmetric discrete triangular distributions }
\description{The function plots symmetric discrete triangular distributions}
\usage{
dtrg(c, a, h, y)
}
\arguments{
  \item{c}{ The center $c$ is an integer }
  \item{h}{ The order $h$ is a positive real number }
  \item{a}{ The arm $a$ is a non-negative integer }
  \item{y}{ The vector of entire observations}
}
\details{
  The symmetric discrete triangular distribution has the probability mass function  

               $$ Pr(Y=y) = [(a+1)^h - \{abs(y-c)\}^h]/A $$

where $ A=(2a+1)(a+1)^h-2sum_{k=1}^{a}k^h$ is the normalizing constant. 
The mean is equal to $c$ and the variance is given by
$V(a,h)=(1/A)\{a(2a+1)(a+1)^{h+1}/3 - 2sum_{k=1}^{a}k^{h+2}\}.$
 }
\value{
  The function returns probability mass function in [0,1] of the corresponding $y$ value. 
  }
\references{
Kokonendji, C.C. and Zocchi, S.S. (2010). Extensions of discrete 
triangular distributions and boundary bias in kernel estimation for discrete functions.
Statistics and Probability Letters, 80, 1655--1662.

 Kokonendji, C.C., Senga Kiess\'e, T. and Zocchi, S.S. (2007). Discrete
triangular distributions and non-parametric estimation for probability mass
function. Journal of Nonparametric Statistics 19, 241--254.
 }
\author{ Tristan Senga Kiess\'e, Silvio S. Zocchi, C\'elestin C. Kokonendji  }

\seealso{ 'dtrgg' for the general discrete triangular distribution }

\examples{
##These examples provide some symmetric discrete triangular distributions of order 
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
main="Symmetric discrete triangular distributions centered in c=5 with arm a=4",
cex.lab=1.5,cex.axis=1.5,pch=20)
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

## The function is currently defined as
function(c,a,h,y){T=rep(0,length(y));
 
 if (a==0)
 {
    {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]==c)   
        T[j]= 1  # Dirac distribution at c 
         
       else{
           T[j]=0
	    }
      } 
    }
  }
  

else
{ 
if (h==0)
 {
    {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]==c)  
        T[j]= 1  # Dirac distribution at c 
         
       else{
           T[j]=0
	    }
      } 
    }
  }


else if (h==Inf)
 {
    {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]>=(c-a) & y[j]<=(c+a)& y[j]==as.integer(y[j]))  
          # Support {c-a,...,c,...c+a}
        
	T[j]= 1/(2*a+1)  
	  # Discrete uniform distribution  
         
       else{
           T[j]=0
	    }
      } 
    }
  }
 
 
 else
{ u=0
   
 {for (k in 1:a)
 
   { 
    u=u+k^h
    }
    
   } 
 
  A=(2*a+1)*(a+1)^h-2*u                # Normalizing constant 
  
 {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]>=(c-a) & y[j]<=(c+a) & y[j]==as.integer(y[j]))   
          # Support {c-a,...,c,...c+a}
        
	T[j]= ((a+1)^h - (abs(y[j]-c))^h)/A  
	   # Symmetric discrete triangular distribution 
         
        
      
        else{
           T[j]=0
	    }
      } 
    }
 }  

  }


 return(T) }
}
