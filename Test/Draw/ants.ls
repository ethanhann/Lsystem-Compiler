def draw ants(int level){
    alphabet:  (X, Y, d, u, h, g);
    rules:{
        lambda -> d X l l X l l X; 
        X -> X l X r r X l d Y g;
        Y -> X r u X l r d X l u X h;
		X = forward(5);
		Y = forward(-1);
        r = turn(90);
        l = turn(-90);
        h = turn(80);
        g = turn(45);
    }
}

def compute main(){
	int x = 0;
	
	while(x < 7)
	{	
		x = x + 1;
		ants(x);
	    up();
	    ants(x);
	   	down();
	    ants(x);	
	}
	

}
