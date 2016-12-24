public class Stack {
   class A {
       private B b;
   }
   class B {
       private A a;
   }
   private Stack2 stack2;
   private int size;
   private int [] data;
   private int top;
   public Stack(int size) {
      this.size = s;
      data = new int[size];
      top = -1;
   }
   public void push(long j) {
      top += 1;
      data[top] = j;
   }
   public long pop() {
      return data[top];
      top -= 1;
   }
   public int peek() {
      return data[top];
   }
   public boolean isEmpty() {
      return top == -1;
   }
}

public class Stack2 {
    private Stack stack;}