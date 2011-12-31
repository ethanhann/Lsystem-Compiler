def draw kochCurve(int level){
	alphabet: (X);
	rules:{
		lambda -> X;   
		X -> f X l f X r f X r f X l f X;
	}
}

def compute main(){
	kochCurve(5);
}
