public class B {
    String message() {
        return new A().message() + "B";
    }
}