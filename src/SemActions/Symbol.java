package SemActions;
import Codes.NTCodes;
import Codes.TkCodes;
import Codes.TCodes;
public class Symbol {
    public byte symbolVal;
    private Type tipo, tipoRet;
    public boolean inSwitch, inFunc;
    private int ancho;
    private int posTS;
    public Symbol(byte symbolVal) {
        this.symbolVal = symbolVal;
    }

    /**
     * getSymbolType devuelve el tipo de simbolo, 
     *
     * @return 1 -> Terminal / 2 -> No Terminal / 3 -> SemCode
     */
    public int getSymbolType() {
        if(TkCodes.isTkCode(symbolVal))
            return 1;
        if(NTCodes.isNTCode(symbolVal))
            return 2;
        if(symbolVal < 0)
            return 3;
        return 0;
    }

    public Type getTipo() {
        if(this.tipo == null)
            this.tipo = new Type(TCodes.tipo_err.id);
        return tipo;
    }
    public void setTipo(Object o) {
        tipo = checkType(o);
    }
    
    public void setTipoRet(Object o) {
        tipoRet = checkType(o);
    }
    public int getPosTS() {
        return posTS;
    }
    public void setPosTS(int posTS) {
        this.posTS = posTS;
    }

    public int getAncho() {
        return this.ancho;
    }

    public void setAncho(int ancho) {
        this.ancho = ancho;
    }

    private Type checkType(Object o) {
        if(o instanceof Byte)
            return new Type((byte)o);
        else if(o instanceof Type)
            return (Type)o;
        else
            throw new IllegalArgumentException();
    }

    public Type getTipoRet() {
        if(this.tipoRet == null)
            this.tipoRet = new Type(TCodes.tipo_err.id);
        return tipoRet;
    }

    @Override
    public String toString() {
        String out = "";
        if(TkCodes.isTkCode(symbolVal)){
            out = String.format("%s ", TkCodes.getTkName(symbolVal));
            if(symbolVal == TkCodes.ID.id && posTS != 0)
                out = String.format("%s posTS=%d |", out, posTS);
        }else if(NTCodes.isNTCode(symbolVal)) {
            out = String.format("%s |", NTCodes.getNTName(symbolVal));
            if(tipo != null)
                out = String.format("%s tipo=%s", out, tipo.toString());
            if(tipoRet != null)
                out = String.format("%s tipoRet=%s", out, tipoRet.toString());
            if(inSwitch)
                out = String.format("%s inSwitch", out);
            if(inFunc)
                out = String.format("%s inFunc", out);
            if(ancho != 0)
                out = String.format("%s ancho=%d", out, ancho);
            if(posTS != 0)
                out = String.format("%s posTS=%d", out, posTS);
        } else if(symbolVal < 0)
            out = String.format("SemCode %d", -symbolVal);
        return out;
    }
}
