def draw sierpinskiTriangle(int level){
	alphabet: (A,B);
	rules:{
		lambda -> A;
		A -> B l A l B;
		B -> A r B r A;
		A = forward(1);
		B = forward(1);
		l = turn(-60);
		r = turn(60);
	}
}

def compute main(){
	sierpinskiTriangle(5);
}
