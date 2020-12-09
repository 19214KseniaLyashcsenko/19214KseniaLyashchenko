import org.junit.Test;
import org.junit.jupiter.api.Assertions;

public class Tests {

    @Test
    public void intTypeTests() {
        Stack mStack = new Stack();
        mStack.push(79);
        mStack.push(35);
        Assertions.assertEquals(mStack.count(), 2);
        Assertions.assertEquals(mStack.pop(), 35);
    }

    @Test
    public void differentTypeTests() {
        Stack mStack = new Stack();
        mStack.push(79);
        mStack.push("Hello World");
        mStack.push(54.34f);
        mStack.push('F');
        mStack.push(false);
        Assertions.assertEquals(mStack.count(), 5);
        Assertions.assertEquals(mStack.pop(), false);
    }

    @Test
    public void bigTests() {
        Stack mStack = new Stack();
        mStack.push(79);
        mStack.push("Hello");
        mStack.push(35);
        mStack.push(66);
        mStack.push("Hello World");
        mStack.push(5);
        mStack.push(1);
        mStack.push('F');
        mStack.push(false);
        mStack.push(79);
        mStack.push("Hello");
        mStack.push(35);
        mStack.push(66);
        mStack.push("Hello World");
        mStack.push(5);
        mStack.push(1);
        mStack.push('F');
        mStack.push(false);
        mStack.push(79);
        mStack.push("Hello");
        mStack.push(35);
        mStack.push(66);
        mStack.push("Hello World");
        mStack.push(5);
        mStack.push(1);
        mStack.push('F');
        mStack.push(false);
        mStack.push(79);
        mStack.push("Hello");
        mStack.push(35);
        mStack.push(66);
        mStack.push("Hello World");
        mStack.push(5);
        mStack.push(1);
        mStack.push('F');
        mStack.push(false);
        mStack.push(79);
        mStack.push("Hello");
        mStack.push(35);
        mStack.push(66);
        mStack.push("Hello World");
        mStack.push(5);
        mStack.push(1);
        mStack.push('F');
        mStack.push(false);
        mStack.push(79);
        mStack.push("Hello");
        mStack.push(35);
        mStack.push(66);
        mStack.push("Hello World");
        mStack.push(5);
        mStack.push(1.453f);
        mStack.push('F');
        mStack.push("bla bla bla");
        Assertions.assertEquals(mStack.count(), 54);
        Assertions.assertEquals(mStack.pop(), "bla bla bla");
        Assertions.assertEquals(mStack.count(), 53);
        Assertions.assertEquals(mStack.pop(), 'F');
        Assertions.assertEquals(mStack.pop(), 1.453f);
        Assertions.assertEquals(mStack.count(), 51);
    }
}
