def draw dancingMonster(int level){
    alphabet:  (X, Y, d, u, h);
    rules:{
        lambda -> d X l l X l l X; 
        X -> X l X r r X l d Y;
        Y -> X r u X l r d X l u X h;
		X = forward(1);
		Y = forward(-1);
        r = turn(90);
        l = turn(-90);
        h = turn(80);
    }
}

def compute main(){
	int x = 7;
	
	while(x > 0)
	{	
		x = x - 1;
		
	
	}
	
	x = 8;
	
	dancingMonster(x);
}
