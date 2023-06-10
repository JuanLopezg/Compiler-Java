package Codes;

public enum NTCodes {
    //Codigos de los simbolos no terminales (empezamos por el 60 para no haber interseccion
    //con los codigos de token (TkCodes))
    P(60), 
    F(61),
    SentS(62), 
    SentC(63), 
    SentSw(64), 
    ExpDec(65), 
    Ig(66), 
	Args(67), 
    ArgsE(68),
    Vargs(69),
    VargsE(70),
    BloqF(71),
    BloqSw(72), 
    Type(73), 
    TypeE(74),
	Val(75), 
    ValRet(76), 
    ValE(77), 
    And(78), 
    AndE(79), 
    Comp(80), 
    CompE(81), 
    Sum(82), 
    SumE(83), 
    Prod(84), 
    ProdE(85), 
    Unit(86), 
    UnitE(87),
    PE(88);

    private static final String[] ntNames = {"P", "F", "SentS", "SentC", "SentSw", "ExpDec", "Ig", "Args", "ArgsE", "Vargs",
        "VargsE", "BloqF", "BloqSw", "Type", "TypeE", "Val", "ValRet", "ValE", "And", "AndE", "Comp", "CompE", "Sum",
        "SumE", "Prod", "ProdE", "Unit", "UnitE", "PE"};
    public static final byte MIN_NT_VAL = 60;
    public static final byte MAX_NT_VAL = 88;
    public final byte id;
    NTCodes(int id) {
        this.id = (byte)id;
    }

    public static boolean isNTCode(byte b) {
        return b >= MIN_NT_VAL && b <= MAX_NT_VAL;
    }

    public static String getNTName(byte b) {
        if(!isNTCode(b))
            throw new IllegalArgumentException();
        return ntNames[b - MIN_NT_VAL];
    }
}
