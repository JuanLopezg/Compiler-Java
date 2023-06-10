package TabDescendente;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import Codes.NTCodes;
import Codes.TkCodes;
import SemActions.Symbol;

public class DescendingTable {
    private static Productions productions = null;
    private static List<Map<Byte, Integer>> tabla = null;

    /**
     * DescendingTable es la clase que contiene la tabla descendente que usamos para el
     * analisis semantico. Para poder usarla multiples veces sin tener que generarla cada
     * vez, declaramos sus campos como static
     */
    public DescendingTable() {
        if(productions == null)
            productions = new Productions();
        if(tabla == null)
            initDescendingTable();
    }

    /**
     * initDescendingTable inicializa la tabla descendente. En producciones tenemos todos
     * los codigos en bytes de los terminales, no terminales y las acciones semanticas. Las tenemos organizadas segun
     * su orden descrito en la gramatica del lenguaje (GramS.txt) y las vamos introduciendo en ese mismo
     * orden en la tabla. Lo que hacemos es asociar cada no terminal (PE, P, SentC...) a una fila de la tabla
     * y cada columna la asociamos con un codigo de produccion (EJ, PE -> P, asociamos P con el codigo 1).
     * Ademas cada linea de un no terminal es un mapa en el que realizamos la asociacion de las keys (los terminales
     * que hemos derivado al comprobar si la gramatica era LL(1)) con los codigos de produccion.
     */
    private void initDescendingTable() {
        int l = NTCodes.MAX_NT_VAL - NTCodes.MIN_NT_VAL;
        DescendingTable.tabla = new ArrayList<>();
    
        for(int i = 0; i <= l; i++)
            DescendingTable.tabla.add(new HashMap<>(TkCodes.MAX_TK_VAL));
        int prodCode = 1;
        //1 We do not use this specific entry (can cause errors if first token is an Identifier)
        newTabEntry(NTCodes.PE.id, prodCode++, TkCodes.LET.id, TkCodes.IF.id, TkCodes.SWITCH.id, TkCodes.PRINT.id,
            TkCodes.IN.id, TkCodes.RET.id, TkCodes.BREAK.id, TkCodes.ID.id, TkCodes.FUNC.id, TkCodes.EOF.id);
        //2
        newTabEntry(NTCodes.P.id, prodCode++, TkCodes.LET.id, TkCodes.IF.id, TkCodes.SWITCH.id, 
            TkCodes.PRINT.id, TkCodes.IN.id, TkCodes.RET.id, TkCodes.BREAK.id, TkCodes.ID.id);
        newTabEntry(NTCodes.P.id, prodCode++, TkCodes.FUNC.id);
        newTabEntry(NTCodes.P.id, prodCode++, TkCodes.EOF.id);
        //5
        newTabEntry(NTCodes.SentS.id, prodCode++, TkCodes.PRINT.id);
        newTabEntry(NTCodes.SentS.id, prodCode++, TkCodes.IN.id);
        newTabEntry(NTCodes.SentS.id, prodCode++, TkCodes.RET.id);
        newTabEntry(NTCodes.SentS.id, prodCode++, TkCodes.BREAK.id);
        newTabEntry(NTCodes.SentS.id, prodCode++, TkCodes.ID.id);
        //10
        newTabEntry(NTCodes.ExpDec.id, prodCode++, TkCodes.ASIG.id);
        newTabEntry(NTCodes.ExpDec.id, prodCode++, TkCodes.PARAB.id);
        newTabEntry(NTCodes.ExpDec.id, prodCode++, TkCodes.ASDIV.id);
        //13
        newTabEntry(NTCodes.Vargs.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.CTE_STRING.id, TkCodes.ID.id);
        newTabEntry(NTCodes.Vargs.id, prodCode++, TkCodes.PARC.id);
        //15
        newTabEntry(NTCodes.VargsE.id, prodCode++, TkCodes.COMA.id);
        newTabEntry(NTCodes.VargsE.id, prodCode++, TkCodes.PARC.id);
        //17
        newTabEntry(NTCodes.SentC.id, prodCode++, TkCodes.LET.id);
        newTabEntry(NTCodes.SentC.id, prodCode++, TkCodes.IF.id);
        newTabEntry(NTCodes.SentC.id, prodCode++, TkCodes.SWITCH.id);
        newTabEntry(NTCodes.SentC.id, prodCode++, TkCodes.PRINT.id, TkCodes.IN.id, TkCodes.RET.id, TkCodes.BREAK.id, 
            TkCodes.ID.id);
        //21
        newTabEntry(NTCodes.Ig.id, prodCode++, TkCodes.ASIG.id);
        newTabEntry(NTCodes.Ig.id, prodCode++, TkCodes.EOL.id);
        //23
        newTabEntry(NTCodes.Type.id, prodCode++, TkCodes.INT.id);
        newTabEntry(NTCodes.Type.id, prodCode++, TkCodes.STRING.id);
        newTabEntry(NTCodes.Type.id, prodCode++, TkCodes.BOOLEAN.id);
        //26
        newTabEntry(NTCodes.BloqSw.id, prodCode++, TkCodes.CASE.id);
        newTabEntry(NTCodes.BloqSw.id, prodCode++, TkCodes.DEF.id);
        newTabEntry(NTCodes.BloqSw.id, prodCode++, TkCodes.LLAVC.id);
        //29
        newTabEntry(NTCodes.SentSw.id, prodCode++, TkCodes.PRINT.id, TkCodes.IN.id, TkCodes.RET.id, TkCodes.BREAK.id, 
            TkCodes.ID.id, TkCodes.LET.id, TkCodes.IF.id, TkCodes.SWITCH.id);
        newTabEntry(NTCodes.SentSw.id, prodCode++, TkCodes.CASE.id, TkCodes.DEF.id, TkCodes.LLAVC.id);
        //31
        newTabEntry(NTCodes.F.id, prodCode++, TkCodes.FUNC.id);
        //32
        newTabEntry(NTCodes.TypeE.id, prodCode++, TkCodes.INT.id, TkCodes.STRING.id, TkCodes.BOOLEAN.id);
        newTabEntry(NTCodes.TypeE.id, prodCode++, TkCodes.PARAB.id);
        //34
        newTabEntry(NTCodes.Args.id, prodCode++, TkCodes.INT.id, TkCodes.STRING.id, TkCodes.BOOLEAN.id);
        newTabEntry(NTCodes.Args.id, prodCode++, TkCodes.PARC.id);
        //36
        newTabEntry(NTCodes.ArgsE.id, prodCode++, TkCodes.COMA.id);
        newTabEntry(NTCodes.ArgsE.id, prodCode++, TkCodes.PARC.id);
        //38
        newTabEntry(NTCodes.BloqF.id, prodCode++, TkCodes.PRINT.id, TkCodes.IN.id, TkCodes.RET.id, TkCodes.BREAK.id, 
            TkCodes.ID.id, TkCodes.LET.id, TkCodes.IF.id, TkCodes.SWITCH.id);
        newTabEntry(NTCodes.BloqF.id, prodCode++, TkCodes.LLAVC.id);
        //40
        newTabEntry(NTCodes.ValRet.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_STRING.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.ID.id);
        newTabEntry(NTCodes.ValRet.id, prodCode++, TkCodes.EOL.id);
        //42
        newTabEntry(NTCodes.Val.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_STRING.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.ID.id);
        //43
        newTabEntry(NTCodes.ValE.id, prodCode++, TkCodes.OR.id);
        newTabEntry(NTCodes.ValE.id, prodCode++, TkCodes.PARC.id, TkCodes.EOL.id, TkCodes.DOSP.id, TkCodes.COMA.id);
        //45
        newTabEntry(NTCodes.And.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_STRING.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.ID.id);
        //46
        newTabEntry(NTCodes.AndE.id, prodCode++, TkCodes.AND.id);
        newTabEntry(NTCodes.AndE.id, prodCode++, TkCodes.OR.id, TkCodes.PARC.id, TkCodes.EOL.id, TkCodes.DOSP.id, TkCodes.COMA.id);
        //48
        newTabEntry(NTCodes.Comp.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_STRING.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.ID.id);
        //49
        newTabEntry(NTCodes.CompE.id, prodCode++, TkCodes.MEN.id);
        newTabEntry(NTCodes.CompE.id, prodCode++, TkCodes.MAY.id);
        newTabEntry(NTCodes.CompE.id, prodCode++, TkCodes.AND.id, TkCodes.OR.id, TkCodes.PARC.id, TkCodes.EOL.id, TkCodes.DOSP.id, TkCodes.COMA.id);
        //52
        newTabEntry(NTCodes.Sum.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_STRING.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.ID.id);
        //53
        newTabEntry(NTCodes.SumE.id, prodCode++, TkCodes.SUM.id);
        newTabEntry(NTCodes.SumE.id, prodCode++, TkCodes.MEN.id, TkCodes.MAY.id, TkCodes.AND.id, TkCodes.OR.id, TkCodes.PARC.id, TkCodes.EOL.id, TkCodes.DOSP.id, TkCodes.COMA.id);
        //55
        newTabEntry(NTCodes.Prod.id, prodCode++, TkCodes.PARAB.id, TkCodes.CTE_INT.id, TkCodes.CTE_STRING.id, TkCodes.CTE_BOOLEAN.id, 
            TkCodes.ID.id);
        //56
        newTabEntry(NTCodes.ProdE.id, prodCode++, TkCodes.MULT.id);
        newTabEntry(NTCodes.ProdE.id, prodCode++, TkCodes.SUM.id, TkCodes.MEN.id, TkCodes.MAY.id, TkCodes.AND.id, TkCodes.OR.id, TkCodes.PARC.id, 
            TkCodes.EOL.id, TkCodes.DOSP.id, TkCodes.COMA.id);
        //58
        newTabEntry(NTCodes.Unit.id, prodCode++, TkCodes.PARAB.id);
        newTabEntry(NTCodes.Unit.id, prodCode++, TkCodes.CTE_INT.id);
        newTabEntry(NTCodes.Unit.id, prodCode++, TkCodes.CTE_BOOLEAN.id);
        newTabEntry(NTCodes.Unit.id, prodCode++, TkCodes.CTE_STRING.id);
        newTabEntry(NTCodes.Unit.id, prodCode++, TkCodes.ID.id);
        //63
        newTabEntry(NTCodes.UnitE.id, prodCode++, TkCodes.PARAB.id);
        newTabEntry(NTCodes.UnitE.id, prodCode, TkCodes.MULT.id, TkCodes.SUM.id, TkCodes.MULT.id, TkCodes.MEN.id, TkCodes.MAY.id, TkCodes.AND.id, TkCodes.OR.id, TkCodes.PARC.id,
            TkCodes.EOL.id, TkCodes.DOSP.id, TkCodes.COMA.id);
    }

    /**
     * newTabEntry inserta todas las keys (terminales) en la fila especificada (no terminal)
     * @param ntCode codigo del no terminal
     * @param prodCode codigo de la produccion
     * @param tkValues codigo de los terminales asociados a la produccion
     */
    private void newTabEntry(byte ntCode, int prodCode, byte... tkValues) {
        Map<Byte, Integer> ntLine = DescendingTable.tabla.get(ntCodToTbCod(ntCode));
        for(byte tkVal: tkValues)
            ntLine.put(tkVal, prodCode);
    }

    /**
     * ntCodtoTbCod traduce el byte code de un codigo no terminal (60,...) a su posicion en la
     * tabla de simbolos (0,...).
     * @param ntCode codigo no terminal
     * @return posicion en la tabla
     */
    private int ntCodToTbCod(int ntCode) {
        return ntCode - NTCodes.MIN_NT_VAL;
    }

    /**
     * getNextProdNum devuelve el codigo de la produccion siguiente.
     * @param ntCode codigo del no terminal al principio de la pila P
     * @param tkCode codigo del token leido por el analizador lexico
     * @return codigo de la siguiente produccion
     */
    public int getNextProdNum(byte ntCode, byte tkCode) {
        if(!NTCodes.isNTCode(ntCode)|| !TkCodes.isTkCode(tkCode))
            throw new IllegalArgumentException("Invalid ntCod or tkCode\n");
        Integer out = DescendingTable.tabla.get(ntCodToTbCod(ntCode)).get(tkCode);
        return out != null? out: 0;
    }

    /**
     * insertProductionToStack inserta la produccion al stack indicado.
     * @param codProd codigo de la produccion, siempre distinto de 0
     * @param stack stack (P) donde se introducen los símbolos que generan la produccion
     */
    public void insertProductionToStack(int codProd, Stack<Symbol> stack) {
        //getProdPos apunta primero al 0 con el que termina la produccion
        int prodPosition = productions.getProdPos(codProd) - 1;
        byte btAt = codProd > 0? productions.getByteAt(prodPosition): 0;
        while(btAt != 0) {
            stack.push(new Symbol(btAt));
            btAt = --prodPosition >= 0? productions.getByteAt(prodPosition): 0;
        }
    }

    /**
     * toString usado durante el testing para comprobar la tabla
     * @return descendingTable to string
     */
    @Override
    public String toString() {
        String out = "TabDescendente(TkCode = NumProd): \n";
        byte actTermCode = NTCodes.MIN_NT_VAL;
        while(actTermCode <= NTCodes.MAX_NT_VAL) {
            out = String.format("%s NT Code %d: %s\n", out, actTermCode, tabla.get(ntCodToTbCod(actTermCode)).toString());
            actTermCode++;
        }
        return out;
    }
}
