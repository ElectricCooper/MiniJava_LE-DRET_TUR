class Break {
    public static void main(String[] args) {
        System.out.println(new BreakTest().runLoop());
    }
}

class BreakTest {
    public int runLoop() {
        int i;
        for (i=0;i < 10; i = i + 1) {
            if(i>4){
                break;
            }
            System.out.println(i);
        }
        return 0;
    }
}