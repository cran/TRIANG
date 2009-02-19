`dtrg` <-  function(c,a,h,y){ 

K=rep(0,length(y));                 
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
 
   { 
    u=u+k^h
    }
    
   } 
 
  A=(2*a+1)*(a+1)^h-2*u            # Normalizing constant 
  
 
    {for (j in 1:length(y))             # Loop in j for each observation y
    
      {if (y[j]>=(c-a) & y[j]<=(c+a)) # Support {c-a,...,c...,c+a}
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

