package Codes;

public enum LexActCodes {
    AC_0_1,     //lex := car; leer;
    AC_1_1,     //lex := lex (+) car; leer;
    AC_1_9, 	//GenPalabraRes o GenIdentif;
    AC_0_2,		//val :=valor(car); leer;
    AC_2_2,		//val := val*10 + valor(car); leer;
    AC_2_10,	//if(val<32768) then GenToken(CTE_INT, val);
    AC_0_3,     //lex := ""; cont = 0; leer;
    AC_3_3,     //lex := lex (+) car; cont++; leer;
    AC_3_11,	//leer; if(cont<=64) GenToken(CTE_STRING, lex);
    AC_4_12,	//leer; GenToken(AND, -);
    AC_5_13,	//leer; GenToken(OR, -);
    AC_6_14,	//leer; GenToken(ASDIV, -);
    AC_0_15,	//leer; GenToken(SUM, -);
    AC_0_16,	//leer; GenToken(MULT, -);
    AC_0_17,	//leer; GenToken(MEN, -);
    AC_0_18,	//leer; GenToken(MAY, -);
    AC_0_19,	//leer; GenToken(ASIG, -);
    AC_0_20,	//leer; GenToken(PARAB, -);
    AC_0_21,	//leer; GenToken(PARC, -);
    AC_0_22,	//leer; GenToken(LLAVAB, -);
    AC_0_23,	//leer; GenToken(LLAVC, -);
    AC_0_24,	//leer; GenToken(DOSP, -);
    AC_0_25,	//leer; GenToken(COMA, -);
    AC_0_26,	//leer; GenToken(EOL, -);
    AC_0_27,    //GenToken(EOF, -);
	AC_LEER,	//Comprende las acciones 0:0, 0:4, 0:5, 0:6, 6:7, 7:7, 7:8, 8:7, 8:8, 8:0
    AC_ERROR;
}
