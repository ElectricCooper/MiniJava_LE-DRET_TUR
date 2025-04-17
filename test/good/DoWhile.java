class DoWhile {
    public static void main(String[] args) {
        System.out.println(new DoWhileTest().runLoop());
    }
}

class DoWhileTest {
    public int runLoop() {
        int j = 5;
        j = 0;
        do {
            System.out.println(j);
            j = j + 1;
        } while (j < 5);
        return 10;
    }
}