package SymbolTable;

import java.util.HashMap;
import java.util.Map;

import ReaderWriter.Writer;
import SemActions.Type;

public class Table {
    private final int tableNumber;
    private final String tableName;
    private int actualDesp;
    private Identifier[] idTable;
    private int nextIdPos;
    private int tableSize;
    private final Map<String, Integer> idNameToPos;

    /*
     * Table es una tabla de símbolos
     * La inicializamos guardando su nombre, el identificador
     * de la tabla
     */
    Table(String tableName, int tableVal) {
        int MIN_TABLE_SIZE = 16;
        this.tableName = tableName;
        tableNumber = tableVal;
        idTable = new Identifier[MIN_TABLE_SIZE];
        actualDesp = 0;
        idNameToPos = new HashMap<>(MIN_TABLE_SIZE);
        tableSize = MIN_TABLE_SIZE;
        nextIdPos = 0;
    }
    Table(String tableName, int tableVal, int posTSG) {
        int MIN_TABLE_SIZE = 16;
        this.tableName = tableName;
        tableNumber = tableVal;
        idTable = new Identifier[MIN_TABLE_SIZE];
        actualDesp = 0;
        idNameToPos = new HashMap<>(MIN_TABLE_SIZE);
        idNameToPos.put(tableName, -posTSG);
        tableSize = MIN_TABLE_SIZE;
        nextIdPos = 0;
    }

    /**
     * insertLex inserta el lexema del identificador en la tabla.
     * Lo inserta en la primera posicion libre de la tabla, y lo anade al hashmap
     * para poder buscarlo despues.
     * @param idName nombre del lexema
     * @return su posicion en la tabla
     */
    public int insertLex(String idName) {
        if(nextIdPos == tableSize)
            growTableSize();
        idNameToPos.put(idName, nextIdPos + 1);
        idTable[nextIdPos++] = new Identifier(idName);
        return nextIdPos;
    }

    /**
     * getPos devuelve la posicion en la tabla de simbolos del lexema
     * @param idName lexema
     * @return su posicion en la tabla
     */
    public int getPos(String idName) {
        Integer pos = idNameToPos.get(idName);
        return pos == null? 0: pos;
    }

    /**
     * getId devuelve el identificador
     * @param idPos posicion del identificador
     * @return identificador en idPos
     */
    public Identifier getId(int idPos) {
        if(idPos <= 0)
            throw new IllegalArgumentException();
        return idTable[idPos - 1];
    }

    /**
     * getType devuelve el tipo del identificador en idPos
     * @param idPos posicion del identificador
     * @return tipo del identificador
     */
    public Type getType(int idPos) {
        return getId(idPos).getTipo();
    }

    /**
     * insertTypeAndDesp inserta el tipo y el desplazamiento en el identificador
     * @param idPos pos del identificador
     * @param type tipo a insertar
     * @param ancho ancho del tipo
     */
    public void insertTypeAndDesp(int idPos, Type type, int ancho) {
        Identifier id = getId(idPos);
        id.setTipo(type);
        id.setDesp(actualDesp);
        actualDesp += ancho;
    }

    /**
     * insertType inserta el tipo (usado para insertar funciones)
     * @param idPos pos del identificador
     * @param type tipo a insertar
     */
    public void insertType(int idPos, Type type) {
        getId(idPos).setTipo(type);
    }

    /**
     * insertEtq inserta la etiqueta de la funcion
     * @param idPos posicion del identificador
     * @param numEtq numero de la etiqueta
     */
    public void insertEtq(int idPos, int numEtq) {
        getId(idPos).setEtq(numEtq);
    }

    /**
     * growTableSize crece el tamano de la zona de almacenamiento de la tabla.
     * Duplica su tamano cada vez que se llama.
     */
    private void growTableSize() {
        tableSize *= 2;
        Identifier[] newTable = new Identifier[tableSize];
        int l = idTable.length;
        System.arraycopy(idTable, 0, newTable, 0, l);
        idTable = newTable;
    }

    /**
     * writeTable escribe la tabla en el escritor indicado
     * @param writer escritor del documento de las tablas de simbolos
     */
    public boolean writeTable(Writer writer) {
        boolean isWriting;
        isWriting = writer.write(this.toString());
        for(int i = 0; i < nextIdPos && isWriting; i++)
            isWriting = writer.write(idTable[i].toString());
        return isWriting;
    }

    /**
     * EL titulo de la tabla de simbolos
     * @return el titulo de la tabla
     */
    @Override
    public String toString() {
        return String.format("TABLA %s #%d:\n", tableName, tableNumber);
    }
}
