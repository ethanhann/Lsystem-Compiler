def draw circuitComet(int level){
    alphabet:  (X, Y, Z, d, u, h, g);
    rules:{
        lambda -> X; 
        X -> X l X r X l r X r X l Y;
        Y -> X l X r X l r X r X l X l l l Z;
        Z -> Y l Y r Y l r Y r Y l Y l l l Z;
		X = forward(1);
		Y = forward(-9);
        r = turn(-90);
        l = turn(90);
        h = turn(80);
        g = turn(45);
    }
}

def compute main(){
	int x = 0;
	
	while(x < 7)
	{	
		x = x + 1;
		#complexCurve(x);
	}
	
	circuitComet(x);

}
