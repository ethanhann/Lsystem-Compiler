def compute gcd(int a, int b){
  int t = 0;
  while (b != 0){
    t = b;
    b = a-(a/b)*b;
    a = t;
  }
  return a;
}

def compute main(){   
  double r = 0;
  r = gcd(27,72);
  print(r);    
}
