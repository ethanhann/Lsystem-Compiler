def draw peano_gosper(int level){
    alphabet: (A,B,f,l,r);
    rules:{
        lambda -> A;
        A -> A r B f r r B f l f A l l f A f A l B f r;
        B -> l f A r B f B f r r B f r f A l l f A l B;
        r = turn(60);
        l = turn(-60);
    }
}

def compute main()
{   
    peano_gosper(5);    
}
