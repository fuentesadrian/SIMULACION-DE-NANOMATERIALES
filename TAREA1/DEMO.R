paso = function(posicioninicial, dimension) {
  d = sample(1:5, 1);
  if (runif(1) < 0.5) {
    posicioninicial[d] = posicioninicial[d] - 1;
  } else {
    posicioninicial[d] = posicioninicial[d] + 1;
  }
  return(posicioninicial);
}

dimension = 5
largo = 30
posicioninicial = rep(0, dimension)
for (t in 1:largo) {
  posicioninicial = paso(posicioninicial, dim)
}
cat(posicioninicial, '\n')

