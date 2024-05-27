let fact = fn rec fact n =>
  if n < 2 then
    1
  else 
    n * fact (n - 1)
in 
fact 4
