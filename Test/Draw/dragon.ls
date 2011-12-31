def draw dragonCurve(int level){
	alphabet: (X,Y);
	rules:{
		lambda -> f X;
		X -> X l Y f;
		Y -> f X r Y;
	}
}

def compute main(){	
	dragonCurve(11);
}
