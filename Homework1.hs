result a b c | (d > 0) = (x1,x2)
             | (d == 0) = (x,x)
			 | (d < 0 || ((b == 0) && (c>0) ))= error "No roots"
	where
      d = b^2 - 4*a*c
      sd = sqrt d
      x1 = (-b + sd)/(2*a)
      x2 = (-b - sd)/(2*a)
      x = -b/(2*a)	  
	  
	  
	  
    