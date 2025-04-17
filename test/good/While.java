class While {
    public static void main(String[] args) {
        System.out.println(new WhileTest().runLoop());
    }
}

class WhileTest {
    public int runLoop() {
        int j;
        j = 0;
        while (j < 5) {
            System.out.println(j);
            j = j + 1;
        }
        return 10;
    }
}