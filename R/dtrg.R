`dtrg` <-
function(c,a,h,y){T=rep(0,length(y));
 
 if (a==0)
 {
    {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]==c)   #  Support {c-a,...,c,...c+a}
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
    
      {if (y[j]==c)  #  Support {c-a,...,c,...c+a}
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
    
      {if (y[j]>=(c-a) & y[j]<=(c+a)& y[j]==as.integer(y[j]))   #  Support {c-a,...,c,...c+a}
        T[j]= 1/(2*a+1)  # Dirac distribution at c 
         
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
    
      {if (y[j]>=(c-a) & y[j]<=(c+a) & y[j]==as.integer(y[j]))   #  Support {c-a,...,c,...c+a}
        T[j]= ((a+1)^h - (abs(y[j]-c))^h)/A  # Discrete triangular distribution 
         
        
      
        else{
           T[j]=0
	    }
      } 
    }
 }  

}


 return(T) }

