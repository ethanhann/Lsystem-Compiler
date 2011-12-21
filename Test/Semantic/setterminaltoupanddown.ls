def draw bla(int level){
    alphabet: (A , B, C, D);
    rules:{
        lambda -> A;					
        A  ->  l B f r A f A r f B l;	
        B  ->  r C f l B f B l f C r;	
        C  ->  l D f r C f C r f D l;
        D  ->  r A f l D f D l f A r;	 
        f = forward(1);
        l = turn(-80);
        r = turn(80);
		u = up();
		d = down();
	#	a = resetPosition(500, 20);
    }
}

def compute main()
{
	bla(3);
}
