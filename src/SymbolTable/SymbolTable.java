package SymbolTable;

import ErrorManager.ErrorHandler;
import ReaderWriter.Writer;
import SemActions.Type;
public class SymbolTable {
    private Table actualTable, localTable, globalTable;
    private final Writer writer;
    private int tableVal;
    private int nextEtqVal;
    public boolean zona_dec;
    public boolean globalIsActual;
    private boolean closed;
    private final ErrorHandler errorHandler;
    public SymbolTable(ErrorHandler errorHandler, String filePath) {
        this.actualTable = this.localTable = this.globalTable = null;
        this.errorHandler = errorHandler;
        this.writer = new Writer(errorHandler, filePath, "_tablaS.txt");
        closed = writer.isClosed();
        this.tableVal = 1;
        this.nextEtqVal = 1;
    }

    public boolean isClosed() {
        return closed;
    }
    
    /** 
     * createTable crea una nueva tabla de Símbolos.
     * Si la global no ha sido creada previamente y no se ha cerrado, hacemos:
     *      TSG = crearTS();
     *      despG = 0;
     *      TSA = TSG;
     *      zona_dec = false;
     * Sino si ya existe la global, creamos una local:
     *      TSL = crearTS();
     *      despL = 0;
     *      TSA = TSL;
     */
    public void createTable(String tableName) {
        if(globalTable == null) {
            if(tableVal != 1)
                throw new IllegalAccessError("Tried to open another global table after the first was closed\n");
            globalTable = new Table(tableName, tableVal++);
            actualTable = globalTable;
            globalIsActual = true;
            zona_dec = false;
        } else if (globalIsActual) {
            int posTS = globalTable.getPos(tableName);
            localTable = new Table(tableName, tableVal++, posTS);
            actualTable = localTable;
            globalIsActual = false;
        } else
            throw new IllegalAccessError("Tried to create a new table when a local already existed\n");
    }
    /*
     * getPosTS returns the position of the stored symbol
     * Positive positions for the global table and negative for the local
     */
    public int getPosTS(String idName) {
        if(actualTable == null)
            throw new IllegalStateException("TS is already closed\n");
        int posTS = actualTable.getPos(idName);
        if(!zona_dec && posTS == 0) {
            posTS = globalTable.getPos(idName);
        } else {
            posTS = globalIsActual? posTS: -posTS;
        }
        return posTS;
    }

    /**
     * getIdName devuelve el nombre del identificador en la idPos
     * @param idPos posicion del identificador ('< 0' tabla actual y '> 0' tabla global)
     * @return el nombre del identificador
     */
    public String getIdName(int idPos) {
        if(globalIsActual && idPos < 0)
            throw new IllegalArgumentException("While looking for the identifier name, the lookup table doesn't exist\n");
        if(idPos < 0)
            return actualTable.getId(-idPos).getName();
        return globalTable.getId(idPos).getName();
    }

    /**
     * searchType busca el tipo del identificador en idPos
     * @param idPos posicion del identificador ('< 0' tabla actual y '> 0' tabla global)
     * @return el tipo del identificador
     */
    public Type searchType(int idPos) {
        if(globalIsActual && idPos < 0)
            throw new IllegalArgumentException("While looking for type, the lookup table doesn't exist\n");
        if(idPos < 0)
            return actualTable.getType(-idPos);
        return globalTable.getType(idPos);
    }

    /**
     * insertTypeAndDesp insertar tipo y desplazamiento en el identificador al que se indica
     * @param idPos posicion del identificador ('< 0' tabla actual y '> 0' tabla global)
     * @param type tipo a insertar
     * @param ancho ancho del tipo
     */
    public void insertTypeAndDesp(int idPos, Type type, int ancho) {
        if(globalIsActual && idPos < 0)
            throw new IllegalArgumentException("Tried to introduce type and desp in a table that doesn't exist\n");
        if(idPos < 0)
            actualTable.insertTypeAndDesp(-idPos, type, ancho);
        else
            globalTable.insertTypeAndDesp(idPos, type, ancho);
    }

    /**
     * insertType insertar el tipo del identificador (usado para insertar el tipo funcion)
     * @param idPos posicion del identificador ('< 0' tabla actual y '> 0' tabla global)
     * @param type tipo a insertar
     */
    public void insertType(int idPos, Type type) {
        if(globalIsActual && idPos < 0)
            throw new IllegalArgumentException("Tried to introduce type and desp in a table that doesn't exist\n");
        if(idPos < 0)
            actualTable.insertType(-idPos, type);
        else
            globalTable.insertType(idPos, type);
    }

    /**
     * insertEtq inserta la etiqueta de la funcion en el identificador
     * @param idPos posicion del identificador ('< 0' tabla actual y '> 0' tabla global)
     */
    public void insertEtq(int idPos) {
        if(globalIsActual && idPos < 0)
            throw new IllegalArgumentException("Tried to introduce type and desp in a table that doesn't exist\n");
        if(idPos < 0)
            actualTable.insertEtq(-idPos, nextEtqVal++);
        else
            globalTable.insertEtq(idPos, nextEtqVal++);
    }

    /**
     * insertLexTS insertar lexema en la tabla de simbolos.
     * Si la zona_dec es true, lo inserta en la global, sino en la tabla actual
     * @param idName nombre del identificador
     * @return posicion del identificador
     */
    public int insertLexTS(String idName) {
        if(closed)
            throw new IllegalStateException("TS is already closed\n");
        int posTS;
        if(zona_dec) { // if at zona_dec, insert at actual table, else insert always at global
            posTS =  actualTable.insertLex(idName);
            return globalIsActual? posTS: -posTS;
        }
        return globalTable.insertLex(idName);
    }

    /**
     * deleteActualTable borra la tabla actual y la escribe donde indica el escritor.
     */
    public void deleteActualTable() {
        if(closed)
            return;
        if(!actualTable.writeTable(writer))
            errorHandler.error(11, writer.getName());
        if(globalIsActual) {
            actualTable = globalTable = null;
        } else {
            actualTable = globalTable;
            localTable = null;
            globalIsActual = true;
        }
    }

    /**
     * close cierra el escritor
     */
    public void close() {
        writer.close();
        closed = true;
    }

}
