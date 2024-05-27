let fact = fn rec fact n =>
  if n then
    1
  else 
    n * fact (n + 1)
in 
fact 10
