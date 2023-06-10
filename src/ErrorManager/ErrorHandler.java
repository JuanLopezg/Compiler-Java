package ErrorManager;

import AnLexico.Token;
import Codes.NTCodes;
import Codes.TCodes;
import Codes.TkCodes;

import java.util.*;

import AnLexico.LexicalParser;
import ReaderWriter.*;
import SemActions.Symbol;
import SemActions.Type;
import SymbolTable.SymbolTable;

public class ErrorHandler {
    private final int MIN_LEX_ERROR = 40;
    private final int MAX_LEX_ERROR = 49;

    private final Reader fileReader;
    private final Writer errorFile;
    private LexicalParser anLexico;
    private final SymbolTable TS;
    private int lastErrorCode;
    private final Queue<Integer> semErrorsQueue;
    private final Queue<String> errorInfoQueue;
    private boolean closed;
    private boolean lexicoIsPanicking = false;
    public ErrorHandler(String filePath) {
        this.semErrorsQueue = new LinkedList<>();
        this.errorInfoQueue = new LinkedList<>();
        this.fileReader = new Reader(this, filePath);
        if(fileReader.isClosed()) {
            this.TS = null;
            this.errorFile = null;
            closed = true;
        } else {
            this.errorFile = new Writer(this, filePath, "_errores.txt");
            closed = errorFile.isClosed();
            this.TS = new SymbolTable(this, filePath);
            closed = TS.isClosed();
        }
    }

    public boolean isClosed() {
        return closed;
    }

    public Reader getFileReader() {
        return this.fileReader;
    }
    public SymbolTable getTS() {
        return this.TS;
    }
    public void setAnLexico(LexicalParser anLexico) {
        this.anLexico = anLexico;
    }

    /**
     * error hace que el gestor de errores escriba une error en el fichero de error
     */
    public void error(int errorCode, String lexCode) {
        int SINT_EQUI_ERROR = 60;
        int SINT_NO_RULE_ERROR = 61;
        if(errorCode >= 1 && errorCode <= 11) {
            switch(errorCode) {
                case 1:
                    System.out.printf("Mensaje del gestor de errores:\n\t+ Error 1: No se encuentra el fichero '%s' en el path especificado, no se puede abrir el fichero de salida de errores\n", lexCode);
                    break;
                case 2:
                    System.out.printf("Mensaje del gestor de errores:\n\t+ Error 2: El nombre del fichero '%s' no tiene una extensión válida de fichero\n", lexCode);
                    break;
                case 3:
                    errorFile.write(String.format("\t+ Error 3: Se ha producido un error durante la lectura del fichero '%s', en la linea %d\n", lexCode, fileReader.getLine()));
                    break;
                case 4:
                    errorFile.write(String.format("\t+ Error 4: Se ha producido un error durante el cierre del lector del fichero '%s'\n", lexCode));
                    break;
                case 5:
                    errorFile.write(String.format("\t+ Error 5: Se ha producido un error durante el cierre del escritor del fichero '%s'\n", lexCode));
                    break;
                case 6:
                    errorFile.write(String.format("\t+ Error 6: Se ha producido un error durante la apertura del escritor del fichero '%s'\n", lexCode));
                    break;
                case 7:
                    errorFile.write(String.format("\t+ Error 7: Se ha producido un error durante la creacion del fichero '%s'\n", lexCode));
                    break;
                case 8:
                    errorFile.write(String.format("\t+ Error 8: Se ha producido un error durante la escritura en el fichero '%s'\n", lexCode));
                    break;
                case 9:
                    errorFile.write(String.format("\t+ Error 9: Tratando de escribir tokenes al fichero de tokenes '%s' cuando ya esta cerrado\n", lexCode));
                    break;
                case 10:
                    errorFile.write(String.format("\t+ Error 10: Tratando de escribir las acciones sintacticas en '%s' cuando ya esta cerrado\n", lexCode));
                    break;
                case 11:
                    errorFile.write(String.format("\t+ Error 11: Tratando de escribir la tabla de simbolos en '%s' cuando ya esta cerrado\n", lexCode));
                    break;
            }
        } else if(errorCode >= MIN_LEX_ERROR && errorCode <= MAX_LEX_ERROR)
            lexError(errorCode, lexCode);
        else if(errorCode == SINT_EQUI_ERROR) //lexCode de la forma tkName ha recibido tkName
            errorFile.write(String.format("Error Sintactico 60: Error de equiparacion, esperando %s, en la linea %d\n", lexCode, fileReader.getLine()));
        else if(errorCode == SINT_NO_RULE_ERROR)
            errorFile.write(String.format("Error Sintactico 61: Valor inesperado '%s', en la linea %d\n", lexCode, fileReader.getLine()));
        lastErrorCode = errorCode;
    
    }

    /**
     * pushSemError mete en la pila de errores semanticos el codigo de error y la linea en la que se ha detectado.
     * @param errorCode codigo de error semantico
     */
    public void pushSemError(int errorCode) {
        semErrorsQueue.add(errorCode);
        semErrorsQueue.add(fileReader.getLine());
    }

    /**
     * pushSemError mete en la pila de errores semanticos el codigo de error y la linea, y los strings de informacion adicional
     * @param errorCode codigo de error semantico
     * @param idName informacion adicional del error
     */
    public void pushSemError(int errorCode, String idName) {
        pushSemError(errorCode);
        errorInfoQueue.add(idName);
    }

    /**
     * semErrorCallback libera la pila de errores semanticos y los escribe en el fichero de errores.
     */
    public void semErrorCallback() {
        int line, semCodeError;
        if(semErrorsQueue.isEmpty()) {
            errorFile.write("Solo se han detectado errores sintacticos en las zonas que no ha ocurrido salto\n");
            return;
        }
        errorFile.write("Se han detectado errores semanticos en el codigo:\n");
        while(!semErrorsQueue.isEmpty()) {
            semCodeError = semErrorsQueue.poll();
            line = semErrorsQueue.poll();
            semError(semCodeError, line);
        }
    }

    /**
     * semError escribe el error semantico usando el codigo de error, la linea y, si hace falta, la informacion almacenada
     * en la pila de nameStack
     * @param semCodeError codigo de error semantico
     * @param line linea a partir de la cual hay un error
     */
    public void semError(int semCodeError, int line) {
        int MIN_SEM_ERROR = 80;
        if(semCodeError < MIN_SEM_ERROR)
            throw new IllegalArgumentException();
        String out = "\t+ Error Semantico %d:\n\t\t%s, antes de la linea %d\n";
        String extra = "";
        switch(semCodeError) {
            case 80:
                extra = "Ha tratado de usar un return fuera de una funcion";
                break;
            case 81:
                extra = String.format("El identificador '%s' usado para el input no es del tipo cadena o entero", errorInfoQueue.poll());
                break;
            case 82:
                extra = "Ha tratado de usar un break fuera de un switch";
                break;
            case 83:
                extra = String.format("Tratando de asignar un valor al identificador '%s', que es una funcion", errorInfoQueue.poll());
                break;
            case 84:
                extra = String.format("Tratando de asignar un valor de un tipo %s", errorInfoQueue.poll());
                break;
            case 85:
                extra = String.format("El identificador '%s' no es una funcion y se esta tratando como si lo fuera", errorInfoQueue.poll());
                break;
            case 86:
                extra = String.format("La funcion %s que no se corresponden", errorInfoQueue.poll());
                break;
            case 87:
                extra = String.format("Esperando un tipo booleano en la condicion del if, ha recibido un %s", errorInfoQueue.poll());
                break;
            case 88:
                extra = String.format("En 'switch(Val)', Val debe ser un valor entero, sin embargo es un %s", errorInfoQueue.poll());
                break;
            case 89:
                extra = String.format("En 'case Val:', Val debe ser un valor entero, sin embargo es un %s", errorInfoQueue.poll());
                break;
            case 90:
                extra = String.format("returns inconsistentes en el switch, devolviendo %s al mismo tiempo", errorInfoQueue.poll());
                break;
            case 91:
                extra = String.format("returns inconsistentes en los bloques de la funcion o del switch, devolviendo %s al mismo tiempo", errorInfoQueue.poll());
                break;
            case 92:
                extra = errorInfoQueue.poll(); break;
            case 93:
                extra = String.format("Con el operando ||, se ha tratado de hacer %s || bool, cuando solo se puede bool || bool", errorInfoQueue.poll());
                break;
            case 94:
                extra = String.format("Con el operando &&, se ha tratado de hacer %s && bool, cuando solo se puede bool && bool", errorInfoQueue.poll());
                break;
            case 95:
                extra = String.format("Con el operando < o >, se ha tratado de hacer %s < entero, cuando solo se puede entero < entero", errorInfoQueue.poll());
                break;
            case 96:
                extra = String.format("Con el operando <, se ha tratado de hacer %s < entero, cuando solo se puede entero < entero", errorInfoQueue.poll());
                break;
            case 97:
                extra = String.format("Con el operando >, se ha tratado de hacer %s > entero, cuando solo se puede entero > entero", errorInfoQueue.poll());
                break;
            case 98:
                extra = String.format("Con el operando +, se ha tratado de hacer %s + entero, cuando solo se puede entero + entero", errorInfoQueue.poll());
                break;
            case 99:
                extra = String.format("Con el operando *, se ha tratado de hacer %s * entero, cuando solo se puede entero * entero", errorInfoQueue.poll());
                break;
            case 100:
                extra = String.format("El identificador '%s' es una funcion que no devuelve ningun valor y se esta tratando de usarla para asignar un valor", errorInfoQueue.poll());
                break;
            case 101:
                extra = String.format("El identificador '%s' se esta tratando de redeclarar su tipo", errorInfoQueue.poll());
                break;
        }
        errorFile.write(String.format(out, semCodeError, extra, line));
    }

    /**
     * lexError escribe los errores lexicos
     * @param errorCode codigo del error lexico
     * @param lexCode informacion del codigo del error
     */
    public void lexError(int errorCode, String lexCode) {
        String out = "Error Lexico %d:\n\t%s, en la linea %d\n";
        String errorInfo = null;
        int line = fileReader.getLine();
        switch (errorCode) {
            case 40://Caracter no reconocido por el procesador(State = 0)
                errorInfo = String.format("Ha llegado el caracter '%s' no reconocido por el procesador", lexCode);
                break;
            case 41: //Fin de fichero inesperado
                errorInfo = String.format("Finalizacion de fichero inesperada sin cerrar la cadena '%s'", lexCode);
                break;
            case 45:
                errorInfo = "Finalizacion de fichero inesperada esperando un caracter";
                break;
            case 46:
                errorInfo = "Finalizacion de fichero inesperada dentro de un comentario";
                break;
            case 42://Esperando char ha recibido otro
            case 43:
            case 44:
                errorInfo = errorCode == 42? "&": errorCode == 43? "|": "=' o '*";
                errorInfo = String.format("Esperando '%s', ha recibido '%s'", errorInfo, lexCode);
                break;
            case 47:
                errorInfo = String.format("El valor = %s supera el valor maximo de 32767", lexCode);
                break;
            case 48:
                errorInfo = String.format("En la cadena '%s' se excede la cantidad maxima de 64 caracteres", lexCode);
                break;
            case 49:
                errorInfo = String.format("La cadena '%s' termina inesperadamente con un salto de linea",
                        lexCode.substring(0, lexCode.length() - 1)); break;
        }
        errorFile.write(String.format(out, errorCode, errorInfo, line));
    }

    /**
     * manageLexError maneja los errores lexicos para que el analizador pueda continuar con errores lexicos
     * @return devuelve un token para poder continuar el analisis
     */
    public Token<?> manageLexError() {
        String out = "\t-> Manejando el error lexico: %s\n\n";
        Token<?> tk = null;
        if(lastErrorCode < MIN_LEX_ERROR || lastErrorCode > MAX_LEX_ERROR)
            throw new IllegalStateException("Tried to manage an error that is not a lexical error at manageLexError\n");
        switch(lastErrorCode) {
            case 40: case 41: case 45: case 46:
                out = String.format(out, "Pidiendo otro token al lexico");
                errorFile.write(out);
                tk = anLexico.requestToken(); break;
            case 42:
                out = String.format(out, "Continuara como si hubiera leido un '&&'");
                tk = new Token<>(TkCodes.AND.id);
                errorFile.write(out); break;
            case 43:
                out = String.format(out, "Continuara como si hubiera leido un '||'");
                tk = new Token<>(TkCodes.OR.id);
                errorFile.write(out); break;
            case 44:
                if(lexicoIsPanicking) {
                   return null;
                } else {
                    out = String.format(out, "No se puede diferenciar entre un error de inicio de comentario ('/*' o al escribir un '/=', asi que continuara solo el Lexico");
                    lexicoIsPanicking = true;
                    errorFile.write(out);
                    tk = panicLexico();
                    out = "+ Fin de panic, recuperando el semantico y el sintactico para finalizar analisis\n\n";
                    errorFile.write(out);
                }
                break;
            case 47:
                out = String.format(out, "Continuara el sintactico como si hubiera leido un entero valido");
                tk = new Token<>(TkCodes.CTE_INT.id);
                errorFile.write(out); break;
            case 48:
                out = String.format(out, "Continuara el sintactico como si hubiera leido una cadena valida");
                tk = new Token<>(TkCodes.CTE_STRING.id);
                errorFile.write(out); break;
            case 49:
                out = String.format(out, "Debido a la cadena faltandole las comillas de cierre, devolvemos solo un ';'");
                tk = new Token<>(TkCodes.EOL.id);
                errorFile.write(out);
        }

        return tk;
    }

    /**
     * manageSyntError maneja el error sintactico manipulando el P stack y el AUX stack, para poder continuar el analisis
     * semantico y el sintactico
     * @param actualPStack P stack
     * @param actualAUXStack AUX stack
     * @param tk token actual
     * @return el siguiente token
     */
    public Token<?> manageSyntError(Stack<Symbol> actualPStack, Stack<Symbol> actualAUXStack, Token<?> tk) {
        if(tk.getTokenCode() == TkCodes.EOF.id) { // Si el tk es EOF
            lookFor(actualPStack, NTCodes.P.id);
            lookFor(actualAUXStack, NTCodes.F.id, NTCodes.P.id);
            if(actualAUXStack.peek().symbolVal == NTCodes.P.id) {
                actualAUXStack.push(new Symbol(NTCodes.SentC.id));
                actualAUXStack.peek().setTipo(TCodes.tipo_err.id);
            }
            actualAUXStack.peek().setTipo(TCodes.tipo_err.id);
            return tk;
        }
        String out = "";
        errorFile.write("\t -> Pausando el semantico y continuando el lexico hasta encontrar punto de seguro de salto:\n");
        //Pop actualPStack until we reach a NoTerminal use to generate new sentences or blocks
        byte auxByte = actualAUXStack.peek().symbolVal;
        int aux;
        Symbol lastTk;
        if(auxByte == NTCodes.P.id || auxByte == NTCodes.BloqF.id) {
            aux = auxByte == NTCodes.P.id? -3:// P -> SentC P {}
                    -52; // BloqF -> SentC BloqF {}
            actualPStack.push(new Symbol((byte)aux)); // push the semantic action
            actualPStack.push(actualAUXStack.peek()); // StackAUX: P -> StackP o StackAUX: BloqF -> StackP
            actualAUXStack.push(new Symbol(NTCodes.SentC.id)); // SentC -> StackAUX
            actualAUXStack.peek().setTipo(TCodes.tipo_err.id); // SentC.tipo = tipo_err
            actualAUXStack.peek().setTipoRet(TCodes.vacio.id); // SentC.tipoRet = vacio
            if(auxByte == NTCodes.P.id)
                aux = searchForState(tk,  EOL_STATE, LLAV_AB_C_STATE); // ; o {}
            else // BloqF
                aux = searchForState(tk, EOL_STATE, LLAV_AB_C_STATE, LLAVC_STATE);
            if(aux == LLAVC_STATE)
                tk = new Token<>(TkCodes.LLAVC.id);
            else if(aux != 0)
                tk = anLexico.requestToken();
            else
                tk = new Token<>(TkCodes.EOF.id);
        } else {
            lastTk = lookFor(actualAUXStack, NTCodes.SentC.id, NTCodes.F.id, NTCodes.BloqSw.id); // pop until SentC or F
            auxByte = actualAUXStack.peek().symbolVal;
            if(auxByte == NTCodes.BloqSw.id) {
                lookFor(actualAUXStack, NTCodes.BloqF.id, NTCodes.P.id);
                lookFor(actualPStack, actualAUXStack.peek().symbolVal);
                actualAUXStack.push(new Symbol(NTCodes.SentC.id)); // SentC -> StackAUX
                actualAUXStack.peek().setTipo(TCodes.tipo_err.id); // SentC.tipo = tipo_err
                actualAUXStack.peek().setTipoRet(TCodes.vacio.id); // SentC.tipoRet = vacio
                out = "+ Informacion del error sintactico anterior: Error en la declaracion de los bloques del switch, %s";
                if(lastTk == null)
                    out = String.format(out, "esperando case Valor : [SentSwitch] o default Valor : [SentSwitch]\n");
                else if(lastTk.symbolVal == TkCodes.CASE.id)
                    out = String.format(out, "esperando case Valor : [SentSwitch]\n");
                else
                    out = String.format(out, "esperando default Valor : [SentSwitch]\n");
                searchForState(tk, LLAVC_STATE);
                tk = anLexico.requestToken();
            } else {
                actualAUXStack.peek().setTipo(TCodes.tipo_err.id);
                actualAUXStack.peek().setTipoRet(TCodes.vacio.id);
                out = String.format("+ Informacion del error sintactico anterior: %s\n", getSyntacticErrorInfo(lastTk));
                if(auxByte == NTCodes.F.id) {
                    lookFor(actualPStack, NTCodes.P.id);
                    aux = searchForState(tk, LLAV_AB_C_STATE);
                    if(!TS.globalIsActual) {
                        TS.deleteActualTable();
                        TS.zona_dec = false;
                    }
                } else { // AUXStack SentC
                    lookFor(actualPStack, NTCodes.BloqF.id, NTCodes.P.id, NTCodes.SentSw.id);
                    auxByte = actualPStack.peek().symbolVal;
                    if(auxByte == NTCodes.BloqF.id || auxByte == NTCodes.SentSw.id)
                        aux = searchForState(tk, EOL_STATE, LLAV_AB_C_STATE, LLAVC_STATE);
                    else
                        aux = searchForState(tk, EOL_STATE, LLAV_AB_C_STATE);
                }
                if(aux == LLAVC_STATE)
                    tk = new Token<>(TkCodes.LLAVC.id);
                else if(aux != 0)
                    tk = anLexico.requestToken();
                else
                    tk = new Token<>(TkCodes.EOF.id);
            }
        }
        errorFile.write(String.format("%s\t -> Reiniciando analizador sintactico a partir de la linea %d con la palabra '%s'\n\n", out, fileReader.getLine(),
                TkCodes.getTkName(tk.getTokenCode())));
        return tk;
    }

    public Token<?> panicLexico() {
        if(anLexico == null)
            throw new IllegalStateException();
        Token<?> tk = null;
        while(tk == null || tk.getTokenCode() != TkCodes.EOF.id)
            tk = anLexico.requestToken();
        return tk;
    }

    private static final int EOL_STATE = 1;
    private static final int LLAV_AB_C_STATE = 2;
    private static final int LLAVC_STATE = 3;
    /**
     * searchForState busca unos tokenes especificos para el reinicio del analizador en ese punto
     * @param states : 1 -> ; / 2 -> { } / 3 -> }
     */
    public int searchForState(Token<?> actualTk, int... states) {
        boolean found = false;
        int l = states.length;
        int actualState = 0;
        int tkCode, countPar = 0;
        Token<?> tk = actualTk;
        while(!found) {
            tkCode = tk.getTokenCode();
            if(tkCode == TkCodes.EOF.id)
                return 0;
            if(actualState <= 3) {
                actualState = tkCode == TkCodes.EOL.id? EOL_STATE:
                        tkCode == TkCodes.LLAVC.id? LLAVC_STATE:
                                tkCode == TkCodes.LLAVAB.id? 4: 0;
                if(actualState == 4)
                    countPar = 1;
            } else {
                countPar += tkCode == TkCodes.LLAVAB.id? 1: tkCode == TkCodes.LLAVC.id? -1: 0;
                if(countPar == 0)
                    actualState = LLAV_AB_C_STATE;
            }
            if(actualState != 0 && actualState <= 3) {
                for(int i = 0; i < l && !found; i++)
                    found = states[i] == actualState;
            }
            if(!found)
                tk = anLexico.requestToken();
        }
        // Returns next token
        return actualState;
    }
    private Symbol lookFor(Stack<Symbol> stack,  byte... byteVals) {
        boolean found = false;
        int l = byteVals.length;
        Symbol aux;
        Symbol out = null;
        while(!stack.isEmpty() && !found) {
            aux = stack.peek();
            if(TkCodes.isTkCode(aux.symbolVal))
                out = aux;
            if(aux.symbolVal > 0) { // We are never looking for a semantic action
                for(int i = 0; i < l && !found; i++)
                    found = aux.symbolVal == byteVals[i];
            }
            if(!found)
                stack.pop();
        }
        return out;
    }

    /**
     * getSyntacticErrorInfo obtiene informacion adicional del error que se ha producido
     * @param lastTk ultimo token extraido de la pila AUX
     * @return la informacion del error
     */
    private String getSyntacticErrorInfo(Symbol lastTk) {
        int auxByte = lastTk.symbolVal;
        if(auxByte == TkCodes.IF.id)
            return "Error en la declaracion del if, esperando if ( ValorBool ) SentenciaSimple";
        else if(auxByte == TkCodes.LET.id) {
            TS.zona_dec = false;
            return "Error en la declaracion del let, esperando let Identificador Tipo [= Valor] ;";
        } else if(auxByte == TkCodes.SWITCH.id)
            return "Error en la declaracion del switch, esperando switch ( ValorEntero ) { BloqSw }";
        else if(auxByte == TkCodes.FUNC.id)
            return "Error en la declaracion de la funcion, esperando function Identificador [Tipo] ( [Tipo Id] ) { BloqF }";
        else if(auxByte == TkCodes.ID.id) {
            Type type = TS.searchType(lastTk.getPosTS());
            String out;
            if (type.typeValIn(TCodes.func.id))
                out = "Error en el paso de argumentos de a la funcion '%s' (Identificador ( [Argumentos] ) ;)";
            else
                out = "Error en la asignacion de la variable '%s' (Identificador = Valor ;)";
            return String.format(out, TS.getIdName(lastTk.getPosTS()));
        } else if(auxByte == TkCodes.RET.id)
            return "Error en el return, esperando return [Valor] ;";
        else if(auxByte == TkCodes.PRINT.id)
            return "Error en el print, esperando print [Valor] ;";
        else if(auxByte == TkCodes.IN.id)
            return "Error en el input, esperando input Identificador ;";
        else if(auxByte == TkCodes.BREAK.id)
            return "Error en el break, esperando break ;";
        return "";
    }

    /**
     * cierra el gestor de errores
     */
    public void close() {
        errorFile.close();
        closed = true;
    }
}
