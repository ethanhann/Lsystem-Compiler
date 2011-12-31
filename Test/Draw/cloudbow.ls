def draw cloudbow(int level){
    alphabet:  (X, Y, Z, d, u, h, g);
    rules:{
        lambda -> X; 
        X -> X l X r X l r X r X l Y;
        Y -> X l X r X l r X r X l X l l l Z;
        Z -> Y l Y r Y l r Y r Y l Y l l l Z Z Z;
		X = forward(1);
		Y = forward(-1000);
        r = turn(-135);
        l = turn(45);
    }
}

def compute main(){	
	cloudbow(7);
}
