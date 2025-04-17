class For {
    public static void main(String[] args) {
        System.out.println(new ForTest().runLoop());
    }
}

class ForTest {
    public int runLoop() {
        int i;
        for (i=0;i < 5; i = i + 1) {
            System.out.println(i);
        }
        return 0;
    }
}