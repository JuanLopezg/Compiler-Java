import AnLenguaje.*;
import AnLexico.LexicalParser;

import java.util.InputMismatchException;
import java.util.Scanner;

public class App {
   public static void main(String[] args) {
       Scanner sc = new Scanner(System.in);
       int val;
       String path = "";
       boolean isClosed = false;
       LanguageParser lP;
       System.out.println("Bienvenido al analizador de lenguajes del grupo 33\nFormado por:\n\t+ Pedro Amaya Moreno\t+ Juan Lopez Gonzalez\t+ Pablo Rodriguez Beceiro\n");
       while(!isClosed) {
           System.out.print("Seleccione lo que desea hacer:\n\t1) Run all tests\n\t2) Analizar un archivo\n\t3) Cambiar el formato de los tokens\n\t4) Salir\nR:");
           val = requestInt(sc);
           switch(val) {
               case 1:
                   System.out.println("Running all tests\n");
                   testAnSintactico();
                   System.out.println("Done\n");
                   break;
               case 2:
                   boolean done = false;
                   while(!done) {
                       if(val == 2) {
                           System.out.print("Introduzca el path al archivo o la ruta relativa:");
                           sc.nextLine();
                           path = sc.nextLine();
                       }
                       lP = new LanguageParser(path);
                       lP.run();
                       System.out.print("¿Quiere que el archivo vuelva a pasar por el analizador?\n\t1) Si\t2) Introducir otro archivo\t 3) Salir\nR:");
                       val = requestInt(sc);
                       while(val < 0 || val > 3) {
                           System.out.print("Vuelva a introducir el numero por favor:");
                           val = requestInt(sc);
                       }
                       done = val == 3;
                   }
                   break;
               case 3:
                   LexicalParser.printTokenNames = !LexicalParser.printTokenNames;
                   System.out.println("Formato cambiado para la generacion de tokenes");
                   break;
               case 4:
                   isClosed = true;
                   break;
               default:
                   do {
                       System.out.print("Por favor, introduzca una opcion reconocida:");
                       val = requestInt(sc);
                   } while(val <= 0 || val > 4);
           }
       }
       sc.close();
   }
    public static void testAnSintactico() {
        String filePath = "./CodeExamples/Ejemplo%s/Ejemplo%s.txt";
        for(int i = 3; i < 11; i++) {
            LanguageParser anLenguaje = new LanguageParser(String.format(filePath, i, i));
            anLenguaje.run();
        }
    }
    public static int requestInt(Scanner sc) {
       int out = 0;
       boolean done = false;
       while(!done) {
           try {
               out = sc.nextInt();
               done = true;
           } catch(InputMismatchException e){
               System.out.print("Por favor, introduzca solo un numero: ");
               sc.nextLine();
           }
       }
       return out;
   }


}
