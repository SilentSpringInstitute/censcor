real log_sub_exp(real a, real b) {
  return a > b ? a + log(1 - exp(b - a)) : b + log(exp(a - b) - 1);
}
