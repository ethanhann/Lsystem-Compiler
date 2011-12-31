def compute test()
{
	int x = 0;
	if(true)
	{
		x = 1;
	}
	else
	{
		return x;
	}
	return x;
}
def compute main()
{
	print(test());
}
