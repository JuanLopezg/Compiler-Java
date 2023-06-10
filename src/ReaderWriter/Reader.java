package ReaderWriter;

import ErrorManager.ErrorHandler;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileInputStream;

public class Reader {
    private final File actualFile;
    private FileInputStream fis = null;
    private int actualLine;
    private boolean closed = false;
    private ErrorHandler errorHandler;
    public Reader(ErrorHandler errorHandler, String filePath) {
        this.actualFile = new File(filePath);
        this.errorHandler = errorHandler;
        closed = !open();
    }

    /*
     * isClosed devuelve si el lector esta cerrado en ese instante
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * leer devuelve el siguiente caracter leido del fichero
     * Si el lector esta cerrado lanza un IllegalAccessError y si ha 
     * llegado al fin del fichero, devuelve null y cierra el lector 
     * @return  readChar
     */
    public Character read() {
        if(closed)
            return null;
        try {
            int actualByte = fis.read();
            //Checks for '/n' new line character or '/r' carriage return character
            if(actualByte == 10)
                actualLine++;
            if(actualByte == -1)
                return null;
            return (char)actualByte;
        } catch (IOException e) {
            errorHandler.error(3, actualFile.getName());
        }
        return null;

    }

    /**
     * getLine devuelve la linea en la que esta el lector en ese momento
     * @return actualLine
     */
    public int getLine() {
        return actualLine;
    }

    /**
     * close cierra el lector
     */
    public void close() {
        try{
            fis.close();
            fis = null;
            closed = true;
        } catch (IOException e) {
            errorHandler.error(4, actualFile.getName());
        }
    }
    /**
     * open abre el lector en su primera linea
     * @return true si lo ha abierto correctamente, sino false
     */
    public boolean open() {
        try {
            if(this.fis == null) {
                this.fis = new FileInputStream(actualFile);
                this.actualLine = 1;
                return true;
            }
        } catch (FileNotFoundException e1) {
            errorHandler.error(1, actualFile.getName());
        } 
        return false;
    }
}
