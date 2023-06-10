package AnLenguaje;

import AnLexico.LexicalParser;
import AnLexico.Token;
import Codes.NTCodes;
import Codes.TkCodes;
import CustomStack.ArrayStack;
import ErrorManager.ErrorHandler;
import ReaderWriter.Writer;
import ReaderWriter.Reader;
import SemActions.SemActions;
import SemActions.Symbol;
import SymbolTable.SymbolTable;
import TabDescendente.DescendingTable;

public class LanguageParser {
    private final LexicalParser lexicalAnalyzer;
    private final ErrorHandler errorHandler;
    private final ArrayStack<Symbol> stackP, stackAUX;
    private final SemActions semActionRunner;
    private final SymbolTable TS_Actual;
    private final Writer syntacticTreeWriter;
    private static final DescendingTable descendingTable = new DescendingTable();
    private boolean isClosed;
    public LanguageParser(String filePath) {
        this.errorHandler = new ErrorHandler(filePath);
        this.TS_Actual = errorHandler.getTS();
        this.stackP = new ArrayStack<>();
        stackP.push(new Symbol(TkCodes.EOF.id));
        stackP.push(new Symbol(NTCodes.PE.id));
        this.stackAUX = new ArrayStack<>();
        this.semActionRunner = new SemActions(errorHandler, stackP, stackAUX);
        if(errorHandler.isClosed()) {
            isClosed = true;
            this.lexicalAnalyzer = null;
            this.syntacticTreeWriter = null;
        } else {
            this.lexicalAnalyzer = new LexicalParser(errorHandler, filePath);
            isClosed = lexicalAnalyzer.isClosed();
            this.syntacticTreeWriter = new Writer(errorHandler, filePath, "_arbSint.txt");
            isClosed = syntacticTreeWriter.isClosed();
        }
    }

    /**
     * run inicia y lanza el analizador del lenguaje
     */
    public void run() {
        if(isClosed)
            return;
        Token<?> tk;
        int numProd;
        Symbol sym;
        // Running first action to initialize the symbol table
        stackAUX.push(stackP.pop()); // Pushing PE to the AUX stack
        writeSyntacticTree(1); // Writing first syntactic rule
        descendingTable.insertProductionToStack(1, stackP); // SemAction1 P SemAction2
        semActionRunner.runSemAction(stackP.pop().symbolVal); // Run SemAction1 (initializes the SymbolTable)
        tk = lexicalAnalyzer.requestToken();
        do {
            sym = stackP.pop();
            switch (sym.getSymbolType()) {
                case 1://Terminal
                    stackAUX.push(sym);
                    if (sym.symbolVal == tk.getTokenCode()) {
                        if (sym.symbolVal == TkCodes.ID.id)
                            sym.setPosTS((int) tk.getTokenValue());
                        tk = lexicalAnalyzer.requestToken();
                    } else {
                        errorHandler.error(60, String.format("%s ha recibido %s",
                                TkCodes.getTkName(sym.symbolVal), TkCodes.getTkName(tk.getTokenCode())));
                        tk = errorHandler.manageSyntError(stackP, stackAUX, tk);
                    }
                    break;
                case 2: //No Terminal
                    stackAUX.push(sym);
                    numProd = descendingTable.getNextProdNum(sym.symbolVal, tk.getTokenCode()); //ntterminal y terminal
                    if (numProd != 0) {
                        writeSyntacticTree(numProd);
                        descendingTable.insertProductionToStack(numProd, stackP);
                    } else {
                        errorHandler.error(61, TkCodes.getTkName(tk.getTokenCode()));
                        tk = errorHandler.manageSyntError(stackP, stackAUX, tk);
                    }
                    break;
                case 3: //SemAction
                    semActionRunner.runSemAction(sym.symbolVal);
            }
        } while(stackP.peek().symbolVal != TkCodes.EOF.id || stackAUX.peek().symbolVal != NTCodes.PE.id);

        syntacticTreeWriter.close();
        TS_Actual.close();
        errorHandler.close();
        lexicalAnalyzer.close();
    }

    public void writeSyntacticTree(int numProd) {
        boolean hasWritten;
        if(!syntacticTreeWriter.hasWritten)
            syntacticTreeWriter.write("Descendente ");
        hasWritten = syntacticTreeWriter.write(String.format("%d ", numProd));
        if(!hasWritten)
            errorHandler.error(10, syntacticTreeWriter.getName());
    }
}
