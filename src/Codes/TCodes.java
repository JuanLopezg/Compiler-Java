package Codes;

public enum TCodes {
    tipo_err(0),
    tipo_ok(1),
    vacio(2),
    entero(3),
    cadena(4),
    bool(5),
    func(6),
    list(7);
    public byte id;
    TCodes(int typeCode) {
        this.id = (byte)typeCode;
    }

    private static String[] tNames = {"tipo_error", "tipo_ok", "vacio", "entero", "cadena", "bool", "funcion", "lista"};

    public static String getName(byte typeVal) {
        if(typeVal < 0 || typeVal > tNames.length)
            throw new IllegalArgumentException();
        return tNames[typeVal];
    }
}
