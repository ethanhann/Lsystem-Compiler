def draw test(int level){
	alphabet: (A,B);
	rules:{
		lambda -> A;
		A -> l B f r A f A r f B l;
		B -> r A f l B f B l f A r;
	}
}

def compute main()
{
	test(5);
}
