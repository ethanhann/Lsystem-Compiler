def draw australia(int level){
    alphabet:  (X, Y, Z);
    rules:{
        lambda -> X; 
        X -> X l X r X l r X r X l Y;
        Y -> X l X r X l r X r X l X l l l Z;
        Z -> Y l Y r Y l r Y r Y l Y l l l Z;
		X = forward(1);
		Y = forward(-1000);
        r = turn(-90);
        l = turn(90);
    }
}

def compute main(){
	int x = 0;
	
	while(x < 7)
	{	
		x = x + 1;
		australia(x);
	}
}
