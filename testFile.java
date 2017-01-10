public class Stack {
   private int[] items;
   private int top = 0;
   public Stack(int size) {
       items = new int[size];
   }
   public void push (int d) {
       if (top < items.length) {
           items[++top] = d;
       }
   }
   public void pop (int d) {
       if (top >= 0) {
           items[top] = d;
           top--;
       }
   }
}