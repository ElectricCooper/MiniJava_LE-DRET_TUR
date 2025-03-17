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
        
        System.out.println(new Test().run());
    }
}

class Test{
    public int run() {
        int i;
        for(i=0;i<5;i=i+1) {
            System.out.println(i);
        }
        return 0;
    }
}
