def draw main(int level, int angle){
    alphabet:  (A,B);
    rules:{
        lambda -> A;					# the function to call in main
        A  ->  l B f r A f A r f B l;	# the A sequence: A is an if statement with a list of functions in the body
        B  ->  r A f l B f B l f A r;	# the B sequence
        #f means "draw forward", l means "turn left 90°", and r means "turn right 90°" 
    }
}

def compute main()
{
	print("MainIsDrawFunction");
	main(4, 45);
}
