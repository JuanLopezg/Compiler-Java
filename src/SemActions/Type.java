package SemActions;

import Codes.TCodes;
import CustomStack.ArrayStack;

public class Type {
    private byte typeVal;
    private final ArrayStack<Byte> typeStack;
    private final byte typeRet;
    private static final int DEF_SIZE = 8;
    
    public Type(byte typeVal) {
        this.typeVal = typeVal;
        this.typeRet = 0;
        if(typeVal == TCodes.list.id) {
            this.typeStack = new ArrayStack<>(DEF_SIZE);
            this.typeStack.push(TCodes.vacio.id);
        } else
            this.typeStack = null;
    }

    // For function type
    public Type(Type args, Type retType) {
        if(args.typeVal != TCodes.list.id || isListOrFunc(retType.typeVal))
            throw new IllegalArgumentException();
        this.typeVal = TCodes.func.id;
        this.typeStack = new ArrayStack<>(args.typeStack.length());
        this.typeRet = retType.typeVal;
        typeStack.pushStack(args.typeStack);
    }
    public void setType(Type type) {
        if(isListOrFunc(type.typeVal) || isListOrFunc(this.typeVal))
            throw new IllegalArgumentException();
        this.typeVal = type.typeVal;
    }

    /**
     * pushTypeVal in a list type, we use this stack to store all the types of a list.
     * If while trying to push a new type, the previous one is 'vacio', we remove it
     * and add the new typeValue.
     */
    public void pushTypeVal(Type typeValue) {
        if(this.typeVal != TCodes.list.id || isListOrFunc(typeValue.typeVal))
            throw new IllegalArgumentException();
        if(typeStack.peek() == TCodes.vacio.id)
            typeStack.pop();
        typeStack.push(typeValue.typeVal);
    }

    private boolean isListOrFunc(byte val) {
        return val == TCodes.list.id || val == TCodes.func.id;
    }

    @Override
    public boolean equals(Object o) {
        if(!(o instanceof Type))
            return false;
        Type val = (Type)o;
        if(val.typeVal == this.typeVal)
            return !isListOrFunc(val.typeVal) ||
                (val.typeStack.equals(this.typeStack) && val.typeRet == this.typeRet);
        return false;
    }

    public byte equalsArgs(Type args) {
        if(args.typeVal == TCodes.list.id && this.typeVal == TCodes.func.id && this.typeStack.equals(args.typeStack))
            return this.typeRet;
        return TCodes.tipo_err.id;
    }
    
    public boolean typeValIn(byte... typeValues) {
        for(byte typeValue: typeValues)
            if(this.typeVal == typeValue)
                return true;
        return false;
    }
    @Override
    public String toString() {
        if(!isListOrFunc(typeVal))
            return TCodes.getName(typeVal);
        if(typeVal == TCodes.list.id)
            return getStackList();
        if(typeVal == TCodes.func.id)
            return String.format("function: %s -> %s", getStackList(), TCodes.getName(typeRet));
        return "";
    }

    public String typeString() {
        String out = "\t\t+ tipo : ";
        if(!isListOrFunc(typeVal))
            return String.format("%s'%s'\n", out, TCodes.getName(typeVal));
        if(typeVal == TCodes.list.id)
            out = String.format("list=%s", getStackList());
        if(typeVal == TCodes.func.id)
            out = String.format("%s'funcion'\n%s\t\t\t+ TipoRetorno : '%s'\n", out, getStackFunc(), TCodes.getName(typeRet));
        return out;
    }

    private String getStackFunc() {
        int l = typeStack.length();
        String out = String.format("\t\t\t+ numParam : %2d\n", l);
        for(int i = 0; i < l; i++)
            out = String.format("%s\t\t\t+ TipoParam%02d : '%s'\n", out, i + 1, TCodes.getName(typeStack.getFromTop(i)));
        return out;
    }

    private String getStackList() {
        int l = typeStack.length();
        String out = TCodes.getName(typeStack.getFromTop(0));
        for(int i = 1; i < l; i++)
            out = String.format("%s X %s", out, TCodes.getName(typeStack.getFromTop(i)));
        return out;
    }
}
