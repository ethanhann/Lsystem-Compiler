def draw dragonCurve(int level){
    alphabet:  (X,Y,Z);
    rules:{
        lambda -> Z;
        Z -> f X;
        X -> X l Y f;
        Y -> f X r Y;
    }
}

def compute main(){	
	dragonCurve(12);
}