class DivisionOperator {
    public static void main(String[] args) {
        System.out.println(new DivisionTest().performDivision());
    }
}

class DivisionTest {
    public int performDivision() {
        int dividend = 10;
        int divisor = 2;
        return dividend / divisor;
    }
}

