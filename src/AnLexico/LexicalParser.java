package AnLexico;

import ReaderWriter.Writer;
import ReaderWriter.Reader;
import SymbolTable.SymbolTable;

import java.util.HashMap;
import java.util.Map;

import Codes.LexActCodes;
import Codes.TkCodes;
import ErrorManager.ErrorHandler;

public class LexicalParser {
    public static boolean printTokenNames = false;
    private final Reader fileReader;
    private final Writer tokenWriter;
    private final ErrorHandler errorHandler;
    private static Map<String, TkCodes> reservedWordsTable;
    private final SymbolTable symbolTable;
    private Character actualChar;
    private final static Lex_AFD AFD = new Lex_AFD();
    private boolean closed;
    public LexicalParser(ErrorHandler errorHandler, String filePath) {
        this.errorHandler = errorHandler;
        this.fileReader = errorHandler.getFileReader();
        this.symbolTable = errorHandler.getTS();
        errorHandler.setAnLexico(this);
        if(reservedWordsTable == null)
            initReservedWordsTable();
        closed = errorHandler.isClosed();
        //Le pasamos el analizador de tokens para la gestión de errores en el sintáctico
        if(!closed) {
            this.tokenWriter = new Writer(errorHandler, filePath, "_tokens.txt");
            leer();
            closed = tokenWriter.isClosed();
        } else {
            this.tokenWriter = null;
        }
    }
    public boolean isClosed() {
        return closed;
    }
    /**
     * initReservedWordsTable inicia tabla de palabras reservadas
     * que es un mapa que asocia los valores de la palabras reservadas con su codigos
     */
    private void initReservedWordsTable() {
        LexicalParser.reservedWordsTable = new HashMap<>();
        reservedWordsTable.put("let",       TkCodes.LET);
        reservedWordsTable.put("int",       TkCodes.INT);
        reservedWordsTable.put("string",    TkCodes.STRING);
        reservedWordsTable.put("boolean",   TkCodes.BOOLEAN);
        reservedWordsTable.put("print",     TkCodes.PRINT);
        reservedWordsTable.put("input",     TkCodes.IN);
        reservedWordsTable.put("function",  TkCodes.FUNC);
        reservedWordsTable.put("return",    TkCodes.RET);
        reservedWordsTable.put("if",        TkCodes.IF);
        reservedWordsTable.put("switch",    TkCodes.SWITCH);
        reservedWordsTable.put("case",      TkCodes.CASE);
        reservedWordsTable.put("default",   TkCodes.DEF);
        reservedWordsTable.put("break",     TkCodes.BREAK);
        reservedWordsTable.put("false",     TkCodes.FALSE);
        reservedWordsTable.put("true",      TkCodes.TRUE);
    }

    /**
     * leer lee el siguiente caracter del fichero
     */
    private void leer() {
        if(!fileReader.isClosed()) {
            actualChar = fileReader.read();
        }
    }

    //Variables:
    private String lexema;
    private int valor;
    private int cont;

    /**
     * requestToken devuelve el siguiente token leido por el analizador lexico
     * @return newToken
     */
    public Token<?> requestToken() {
    	int actualState = 0;
        int nextState = 0;
    	LexActCodes acc;
        if(actualChar == null)
            return new Token<>(TkCodes.EOF.id);
    	while(actualState >= 0 && actualState < 9) {
    		nextState = AFD.getNextState(actualState, actualChar);
    		acc = AFD.getAction(actualState, nextState);
            switch(acc) {
                case AC_LEER: 
                    leer(); break;
                case AC_0_1:
                    lexema = String.format("%c", actualChar);
                    leer(); break;
                case AC_1_1: 
                    lexema = String.format("%s%c", lexema, actualChar); 
                    leer(); break;
    	    	case AC_1_9: 
    	    		TkCodes pos = reservedWordsTable.get(lexema);
    	    		if(pos != null) {
                        if(pos.equals(TkCodes.FALSE))
                            return genToken(TkCodes.CTE_BOOLEAN, 0);
    	    			else if(pos.equals(TkCodes.TRUE)) 
    	    				return genToken(TkCodes.CTE_BOOLEAN, 1);
    	    			else 
                            return genToken(pos);
    	    		} else {
    	    			int posTs = symbolTable.getPosTS(lexema);
    	    			if(posTs == 0)
                            posTs = symbolTable.insertLexTS(lexema); // Control of the zona_dec is made at TS
                        return genToken(TkCodes.ID, posTs);
    	    		}
    	    	case AC_0_2: 
                    valor = valor(actualChar); 
                    leer(); break;		
    	    	case AC_2_2: 
                    valor = valor * 10 + valor(actualChar); 
                    leer(); break;
    	    	case AC_2_10: 
                    if(valor < 32768) 
                        return genToken(TkCodes.CTE_INT, valor); 
                    else 
                        errorHandler.error(47, String.format("%d", valor));
                    break;
                case AC_0_3:
                    lexema = "";
                    cont = 0;
                    leer(); break;
                case AC_3_3:
                    if(actualChar != 10) { // char != '\n'
                        lexema = String.format("%s%c", lexema, actualChar);
                        cont++; leer();
                    } else {
                        errorHandler.error(49, lexema);
                        nextState = -1;
                    }
                    break;
                case AC_3_11:
                    leer(); 
                    if(cont <= 64) 
                        return genToken(TkCodes.CTE_STRING, lexema); 
                    else 
                        errorHandler.error(48, lexema);
                    break;
    	    	case AC_4_12: 
                    leer(); 
                    return genToken(TkCodes.AND);
    	    	case AC_5_13: 
                    leer(); 
                    return genToken(TkCodes.OR); 
    	    	case AC_6_14: 
                    leer(); 
                    return genToken(TkCodes.ASDIV);
    	    	case AC_0_15: 
                    leer(); 
                    return genToken(TkCodes.SUM);
    	    	case AC_0_16: 
                    leer(); 
                    return genToken(TkCodes.MULT);
    	    	case AC_0_17: 
                    leer(); 
                    return genToken(TkCodes.MEN);
    	    	case AC_0_18: 
                    leer(); 
                    return genToken(TkCodes.MAY);
    	    	case AC_0_19: 
                    leer(); 
                    return genToken(TkCodes.ASIG);
    	    	case AC_0_20: 
                    leer(); 
                    return genToken(TkCodes.PARAB);
    	    	case AC_0_21: 
                    leer(); 
                    return genToken(TkCodes.PARC);
    	    	case AC_0_22: 
                    leer(); 
                    return genToken(TkCodes.LLAVAB);
    	    	case AC_0_23: 
                    leer(); 
                    return genToken(TkCodes.LLAVC); 
    	    	case AC_0_24: 
                    leer(); 
                    return genToken(TkCodes.DOSP); 
    	    	case AC_0_25: 
                    leer(); 
                    return genToken(TkCodes.COMA);
    	    	case AC_0_26: 
                    leer(); 
                    return genToken(TkCodes.EOL);
                case AC_0_27:
                    return genToken(TkCodes.EOF);
                case AC_ERROR:
                    manageStateError(actualState);
                    leer(); break;
            }
            actualState = nextState;
        }
        return errorHandler.manageLexError();
    }

    /**
     * manageStateError interpreta el estado actual para saber que codigo de error y
     * argumentos debe pasar al gestor de errores.
     * @param actualState estado actual
     */
    private void manageStateError(int actualState) {
        int i = 0;
        int errorCode = 0;
        switch(actualState) {
            case 0:
                errorHandler.error(40, "" + actualChar);
                break;
            case 6:
                i++;
                errorCode = actualChar == null? 45: 42;
            case 5:
                i++;
                errorCode = errorCode != 0? actualChar == null? 45: 43: 
                    actualChar;
            case 4:
                i++;
                errorCode = errorCode != 0? actualChar == null? 45: 44: 
                    actualChar;
            case 3:
                errorCode = actualChar == null && actualState != 3? 45: 41 + i;
                if(errorCode != 41 && errorCode <= 44)
                    errorHandler.error(errorCode, "" + actualChar);
                else 
                    errorHandler.error(errorCode, lexema);
                break;
            case 7: case 8:
                errorHandler.error(46, "");
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    /**
     * Genera un token de palabra reservada con la id pasada como parametro y lo escribe en el fichero de tokenes.
     * @return el token generado
     */
    private Token<Integer> genToken(TkCodes tkCode) {
		Token<Integer> tk = new Token<>(tkCode.id);
        writeToken(tkCode, tk);
        if(TkCodes.EOF.equals(tkCode))
            tokenWriter.close();
		return tk;
	}
    /**
     * Genera un token identificador con la id pasada como parametro y con la posicion que este ocupa en la tabla de simbolos
     * y lo escribe en el fichero de tokenes
     * @return el token generado
     */
    private Token<String> genToken(TkCodes tkCode, String lex) {
		Token<String> tk = new Token<>(tkCode.id, lex);
        writeToken(tkCode, tk);
		return tk;
	}
     /**
     * Genera un token identificador con la id pasada como parametro y con la posicion que este ocupa en la tabla de simbolos
      * y lo escribe en el fichero de tokenes
     * @return el token generado
     */
    private Token<Integer> genToken(TkCodes tkCode, int val) {
		Token<Integer> tk = new Token<>(tkCode.id, val);
        writeToken(tkCode, tk);
		return tk;
	}
    /**
     * writeToken escribe el token en el fichero de tokens
     * Se puede modificar el formato del fichero con el boolean printTokenName
     * Si true, imprime los tokens con su nombre sino con su codigo
     * @param tkCode codigo del token
     * @param tk token que se ha generado para escribirse en el fichero
     */
    private void writeToken(TkCodes tkCode, Token<?> tk) {
        String tkVal = tk.getTokenValue() == null? "": tk.getTokenValue().toString();
        boolean hasWritten;
        if(printTokenNames) {
            String s = tk.getTokenValue() instanceof String? "<%s, \"%s\">\n": "<%s, %s>\n";
            hasWritten = tokenWriter.write(String.format(s, tkCode.name(), tkVal));
        } else
            hasWritten = tokenWriter.write(String.format("%s\n", tk));
        if(!hasWritten)
            errorHandler.error(9, tokenWriter.getName());
    }
    
    /**
     * Metodo que recibe un numero como char y devuelve el valor int del mismo.
     * @return devuelve el valor del digito
     */
	private int valor(char d) {
		return d - '0';
	}

    /**
     * cierra el analizador lexico, cierra el lector del fichero y si no se ha cerrado el escritor de tokenes
     * genera el token de fin de fichero y lo cierra.
     */
    public void close() {
        fileReader.close();
        if(!tokenWriter.isClosed()){
            genToken(TkCodes.EOF);
            tokenWriter.close();
        }
        closed = true;
    }
}