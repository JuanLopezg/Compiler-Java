package AnLexico;

public class Token<VAL_TYPE> {
    private byte tokenCode;
    private VAL_TYPE tokenValue = null;

    /*
     * Crea un nuevo token
     * Por defecto tokenValue es null
     */
    public Token(byte tokenCode) {
        this.tokenCode = tokenCode;
    }
    public Token(byte tokenCode, VAL_TYPE tokenValue) {
        this.tokenCode = tokenCode;
        this.tokenValue = tokenValue;
    }

    public byte getTokenCode() {
        return tokenCode;
    }

    public VAL_TYPE getTokenValue() {
        return tokenValue;
    }
    
    @Override
    public String toString() {
        String s = tokenValue instanceof String? "<%s, \"%s\">": "<%s, %s>";
        String tokenValueString = tokenValue != null? tokenValue.toString() : "";
        return String.format(s, tokenCode, tokenValueString);
    }
}
