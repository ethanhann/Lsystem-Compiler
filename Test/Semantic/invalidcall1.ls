def compute test(int a, double b)
{
return (a + b);
}


def compute main()
{
double x = 3;
double y = 2.0;
double r = 0;
r = test(x, y); #double can't be used in place of int. 
print(r);

}

