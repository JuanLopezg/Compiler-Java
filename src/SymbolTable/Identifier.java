package SymbolTable;

import Codes.TCodes;
import SemActions.Type;

public class Identifier {
    private String idName;
    private Type tipo = null;
    private int desp = -1;
    private String etqFunc = null;
    Identifier(String idName) {
        this.idName = idName;
    }

    public Type getTipo() {
        if(tipo == null)
            tipo = new Type(TCodes.tipo_err.id);
        return tipo;
    }

    public void setTipo(Type tipo) {
        this.tipo = tipo;
    }
    public void setDesp(int desp) {
        this.desp = desp;
    }

    public String getName() {
        return idName;
    }

    public void setEtq(int num) {
        this.etqFunc = String.format("Et%s%02d", this.idName, num);
    }

    public String toString() {
        String out = String.format("\t* LEXEMA : '%s'\n", idName);
        if(tipo != null)
            out = String.format("%s%s", out, tipo.typeString());
        if(desp != -1)
            out = String.format("%s\t\t+ despl : %d\n", out, desp);
        if(etqFunc != null)
            out = String.format("%s\t\t+ EtiqFuncion : '%s'\n", out, etqFunc);
        return out;
    }
}
