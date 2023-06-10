package ReaderWriter;

import ErrorManager.ErrorHandler;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

public class Writer {
    private final File outputFile;
    private FileOutputStream fos = null;
    private final ErrorHandler errorHandler;
    public boolean closed;
    public boolean hasWritten;
    /**
     * Crea un nuevo escritor, el cual se abre por defecto
     * Si ocurriera alguna excepcion, el escritor se cierra
     * @param originalFilePath path original del fichero
     * @param addOn anadido al final del nombre del fichero
     */
    public Writer(ErrorHandler errorHandler, String originalFilePath, String addOn) {
        this.errorHandler = errorHandler;
        String fileName = getFileWOextension(originalFilePath);
        if(fileName == null) {
            closed = true;
            this.outputFile = null;
        } else {
            this.outputFile = new File(String.format("%s%s", fileName, addOn));
            if(outputFile.exists())
                outputFile.delete();
            hasWritten = false;
        }
    }

    public String getName() {
        return outputFile.getName();
    }
    /**
     * getFileWOextension devuelve el filePath sin su estension
     * Ej) ../ejemplo/ejemplo.javascript -> ../ejemplo/ejemplo
     * @param filePath path del fichero
     * @return devuelve el nombre del fichero sin la extension .txt
     */
    private String getFileWOextension(String filePath) {
        int l = filePath.length();
        int i = l - 1;
        while(i >= 0 && filePath.charAt(i) != '.') {
            i--;
        }
        if(i < 0 || i == l - 1) {
            errorHandler.error(2, filePath);
            return null;
        }
        return filePath.substring(0, i);
    }

    /**
     * write escribe en el fichero abierto los strings que le van llegando
     *
     * @param toBeWritten string que se va a escribir
     */
    public boolean write(String toBeWritten) {
        if(!hasWritten)
            generateFileAndOpen();
        if(closed)
            return false;

        if(fos != null) {
            try {
                fos.write(toBeWritten.getBytes());
                return true;
            } catch (IOException e) {
                errorHandler.error(8, outputFile.getName());
            }
        }
        return false;
    }

    /**
     * close cierra el escritor
     */
    public void close() {
        try {
            if(fos != null) fos.close();
            fos = null;
            closed = true;
        } catch (IOException e) {
            errorHandler.error(5, outputFile.getName());
        }
    }

    /**
     * open abre el escritor
     * @return Si no puede porque se produce alguna excepcion devuelve false
     */
    public boolean open() {
        try {
            this.fos = new FileOutputStream(outputFile);
            return true;
        } catch(FileNotFoundException e) {
            errorHandler.error(6, outputFile.getName());
        }
        return false;
    }
    
    /*
     * generateFile crea un nuevo fichero con el nombre de outputFile, si ya existiera
     * borra el antiguo y abre uno nuevo
     */
    private void generateFileAndOpen() {
        try {
            outputFile.createNewFile();
            closed = !open();
            hasWritten = true;
        } catch(IOException e) {
            errorHandler.error(7, outputFile.getName());
        }
    }
    public boolean isClosed() {
        return closed;
    }
}
