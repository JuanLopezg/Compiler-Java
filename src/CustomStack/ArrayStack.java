package CustomStack;

import java.util.Stack;

public class ArrayStack<X> extends Stack<X> {
    private static final short DEF_SIZE = 128;
    private int actualStackSize;
    private int actualTop;
    private X[] stackArray;

    public ArrayStack() {
        init(DEF_SIZE);
    }
    public ArrayStack (int initSize) {
        init(initSize);
    }
    @SuppressWarnings("unchecked")
    private void init(int initSize) {
        if(initSize <= 0)
            throw new IllegalArgumentException();
        this.actualStackSize = initSize;
        this.actualTop = 0;
        this.stackArray = (X[])new Object[actualStackSize];
    }

    @SuppressWarnings("unchecked")
    private void expandArraySize() {
        actualStackSize *= 2;
        X[] nStackArray = (X[])new Object[actualStackSize];
        if (actualTop >= 0)
            System.arraycopy(stackArray, 0, nStackArray, 0, actualTop);
        stackArray = nStackArray;
    }

    public X peek() {
        if(actualTop <= 0)
            return null;
        return stackArray[actualTop - 1];
    }

    public X pop() {
        if(actualTop <= 0)
            return null;
        return stackArray[--actualTop]; 
    }

    public X pop(int numPops) {
        if(numPops < 0 || actualTop - numPops < 0)
            throw new IllegalArgumentException();
        actualTop -= numPops;
        return stackArray[actualTop];
    }

    public X push(X val) {
        if(actualTop == actualStackSize)
            expandArraySize();
        stackArray[actualTop++] = val;
        return val;
    }

    /**
     * getFromTop devuelve el valor X desplazado desp unidades del inicio de la pila
     * @param desp unidades de desplazamiento
     * @return valor X
     */
    public X getFromTop(int desp) {
        if(desp < 0 || actualTop - desp < 0)
            throw new IllegalArgumentException();
        return stackArray[actualTop - desp - 1];
    }

    public void pushStack(ArrayStack<X> stack) {
        int l = stack.length();
        for(int i = 0; i < l; i++)
            this.push(stack.stackArray[i]);
    }
    public boolean isEmpty() {
        return actualTop == 0;
    }
    @Override
    public boolean equals(Object o) {
        if(!(o instanceof ArrayStack<?>))
            return false;
        ArrayStack<?> val = (ArrayStack<?>)o;
        int top = val.actualTop;
        if(top != this.actualTop)
            return false;
        boolean out = true;
        for(int i = 0; i < top && out; i++)
            out = val.stackArray[i].equals(stackArray[i]);
        return out;
    }

    public int length() {
        return actualTop;
    }

    /**
     * Usado durante el testing de las pilas P y AUX
     * @return
     */
    @Override
    public String toString() {
        String out = "ArrayStack:\n";
        for(int i = actualTop - 1; i >= 0; i--) 
            out = String.format("%s %s\n", out, stackArray[i].toString());
        return out;
    }


}
