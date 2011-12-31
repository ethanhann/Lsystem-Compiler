def draw levycCurve(int level){
	alphabet: (X);
	rules:{
		lambda -> X;
		X -> r f X l l f X r;
		l = turn(-45);
		r = turn(45);
	}
}

def compute main(){
	levycCurve(12);
}
