package Codes;

public enum TkCodes {
    //Codigos de los tokens reconocidos por el procesador
    CTE_INT(1),
    CTE_STRING(2),
    CTE_BOOLEAN(3),
    SUM(4),
    MULT(5),
    MEN(6),
    MAY(7),
    AND(8),
    OR(9),
    ASIG(10),
    ASDIV(11),
    PARAB(12),
    PARC(13),
    LLAVAB(14),
    LLAVC(15),
    DOSP(16),
    COMA(17),
    EOL(18),
    LET(19),
    INT(20),
    STRING(21),
    BOOLEAN(22),
    PRINT(23),
    IN(24),
    FUNC(25),
    RET(26),
    IF(27),
    SWITCH(28),
    CASE(29),
    DEF(30),
    BREAK(31),
    ID(32),
    EOF(33),
    //No son codigos de token, solo es para codificar el valor de true y false
    FALSE(34),
    TRUE(35)
    ;
    public static final int MIN_TK_VAL = 1;
    public static final int MAX_TK_VAL = 33;
    private static final String[] tkNames = new String[] {
        "constante entera", "cadena", "constante booleana", "+", "*", "<", ">", "&&", "||", "=", "/=", "(", ")", "{", "}", ":",
        ",", ";", "let", "int", "string", "boolean", "print", "input", "function", "return", "if", "switch", "case", "default",
        "break", "identificador", "EOF"
    };
    public final byte id;
    TkCodes(int id) {
        this.id = (byte)id;
    }
    public static boolean isTkCode(byte b) {
        return b >= MIN_TK_VAL && b <= MAX_TK_VAL;
    }

    public static String getTkName(byte tkByte) {
        if(tkByte < MIN_TK_VAL || tkByte > MAX_TK_VAL)
            return "";
        return tkNames[tkByte - 1];
    }

}