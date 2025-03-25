class My_Test {


    public static void main(String[] a) {
        //if(2>1) System.out.println(1);
        // else System.out.println(2);

        // if(false || true) System.out.println(3);
        // else System.out.println(4);

        // System.out.println(10/2);

        // System.out.println(true);

        //System.out.println(true==false);

        //if(2>1) System.out.println(1);
        
        // System.out.println(new TestFor().run());

        // System.out.println(new TestWhile().run());

        // System.out.println(new TestDoWhile().run());

        System.out.println(new TestString().run());
    }
}

class TestString{
    public string run() {
        string s1 = "Hello ";
        string s2 = "World";
        string s = s1 + s2;
        return s;
    }
}

class TestFor{
    public int run() {
        int i;
        for(i=0;i<5;i=i+1) {
            System.out.println(i);
        }
        return 0;
    }
}

class TestWhile {
    public int run(){
        int j;
        int size;
        size = 5;
        j = 5 ;
        while (j < size) {
	    System.out.println(j);
	    j = j + 1 ;
	    }
        return 0;
    }
    
}

class TestDoWhile {
    public int run(){
        int j;
        int size;
        size = 5;
        j = 5 ;
        do { 
            System.out.println(j);
            j = j + 1 ; 
        } 
        while (j < size);
        return 0;
    }
    
}
