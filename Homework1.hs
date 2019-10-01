result a b c  | (d >= 0 && a /= 0) = (x1,x2)
              | (a == 0 && b /= 0) = (x,x)
              | (a == 0 && b == 0 && c == 0) = error "Any x"
              | ((a /= 0 && d < 0) || (a == 0 && b == 0 && c /= 0)) = error " No x "
	where
      d = b^2 - 4*a*c
      x1 = (-b + sqrt d)/(2*a)
      x2 = (-b - sqrt d)/(2*a)
      x = -c/b
	  
	  
    
