public class SumStack {
   private int[] items;
   private int top = -1;
   private int sum = 0;
   public SumStack (int size) {
       items = new int[size];
   }
   public void push (int d) {
       if (top < items.length) {
           top = top + 1;
           items[top] = d;
           sum = sum + d;
       }
   }
   public int pop () {
       int d = -1;
       if (top >= 0) {
           d = items[top];
           top = top - 1;
           sum = sum - d;
       }
       return d;
   }

   public int sum () {
       return sum;
   }
}