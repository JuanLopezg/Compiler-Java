package TabDescendente;
import java.util.HashMap;
import java.util.Map;

import Codes.NTCodes;
import Codes.TkCodes;
class Productions {
    private final Map<Integer, Integer> prodNumToPos;
    public static int num_prod;
    /**
     * PROD contiene todas las producciones con el mismo orden que en la gramatica
     * Los TkCodes son todos mayores que 0 y menores que 40 los TmCodes son todos menores, por lo que no
     * hay solapamiento de codigos. El 0 lo usamos para separar producciones
     * En el mapa se asocia el num de la produccion con el cero mas cercano a su derecha
     * ej) 1 -> 2
     * PROD tambien contiene los codigos de las acciones semanticas (los bytes negativos)
     */
    private static final byte[] PROD = new byte[]
    {   
        //1
        -1, NTCodes.P.id, -2, 0,
        //2
        NTCodes.SentC.id, NTCodes.P.id, -3, 0,
        NTCodes.F.id, NTCodes.P.id, -4,0,
        -5, 0,
        //5
        TkCodes.PRINT.id, NTCodes.Val.id, TkCodes.EOL.id, -6, 0,
        TkCodes.IN.id, TkCodes.ID.id, -80, TkCodes.EOL.id, -7, 0, // We had an error while writing the semantic actions
        TkCodes.RET.id, NTCodes.ValRet.id, TkCodes.EOL.id, -8, 0,
        TkCodes.BREAK.id, TkCodes.EOL.id, -9, 0, 
        TkCodes.ID.id, -10, NTCodes.ExpDec.id, TkCodes.EOL.id, -11, 0,
        //10
        TkCodes.ASIG.id, NTCodes.Val.id, -12, 0, 
        TkCodes.PARAB.id, NTCodes.Vargs.id, TkCodes.PARC.id, -13, 0,
        TkCodes.ASDIV.id, NTCodes.Val.id, -14, 0,
        //13
        NTCodes.Val.id, NTCodes.VargsE.id, -15, 0,
        -16, 0,
        //15
        TkCodes.COMA.id, NTCodes.Val.id, NTCodes.VargsE.id, -17, 0,
        -18, 0,
        //17
        -19, TkCodes.LET.id, TkCodes.ID.id, NTCodes.Type.id, -20, NTCodes.Ig.id, TkCodes.EOL.id, -21, 0,
        TkCodes.IF.id, TkCodes.PARAB.id, NTCodes.Val.id, TkCodes.PARC.id, -22, NTCodes.SentS.id, -23, 0,
        TkCodes.SWITCH.id, TkCodes.PARAB.id, NTCodes.Val.id, TkCodes.PARC.id, TkCodes.LLAVAB.id, -24, NTCodes.BloqSw.id, TkCodes.LLAVC.id, -25, 0,
        -26, NTCodes.SentS.id, -27, 0,
        //21
        TkCodes.ASIG.id, NTCodes.Val.id, -28, 0,
        -29, 0,
        //23
        TkCodes.INT.id, -30, 0, 
        TkCodes.STRING.id, -31, 0,
        TkCodes.BOOLEAN.id, -32, 0,
        //26
        TkCodes.CASE.id, NTCodes.Val.id, TkCodes.DOSP.id, -33, NTCodes.SentSw.id, -34, NTCodes.BloqSw.id, -35, 0, 
        TkCodes.DEF.id, TkCodes.DOSP.id, -36, NTCodes.SentSw.id, -37, 0,
        -38, 0,
        //29
        -39, NTCodes.SentC.id, -83, NTCodes.SentSw.id, -40, 0,
        -41, 0,
        //31
        TkCodes.FUNC.id, TkCodes.ID.id, -42, NTCodes.TypeE.id, TkCodes.PARAB.id, NTCodes.Args.id, TkCodes.PARC.id, -43, TkCodes.LLAVAB.id, NTCodes.BloqF.id, TkCodes.LLAVC.id, -44, 0,
        //32
        NTCodes.Type.id, -45, 0,
        -46, 0,
        //34
        NTCodes.Type.id, TkCodes.ID.id, -81, NTCodes.ArgsE.id, -47, 0,
        -48, 0,
        //36
        TkCodes.COMA.id, NTCodes.Type.id, TkCodes.ID.id, -82, NTCodes.ArgsE.id, -49, 0,
        -50, 0,
        //38
        -51, NTCodes.SentC.id, NTCodes.BloqF.id, -52, 0,
        -53, 0,
        //40
        NTCodes.Val.id, -54, 0,
        -55, 0,
        //42
        NTCodes.And.id, NTCodes.ValE.id, -56, 0,
        //43
        TkCodes.OR.id, NTCodes.And.id, NTCodes.ValE.id, -57, 0, 
        -58, 0,
        //45
        NTCodes.Comp.id, NTCodes.AndE.id, -59, 0,
        //46
        TkCodes.AND.id, NTCodes.Comp.id, NTCodes.AndE.id, -60, 0,
        -61, 0,
        //48
        NTCodes.Sum.id, NTCodes.CompE.id, -62, 0,
        //49
        TkCodes.MEN.id,NTCodes.Sum.id, NTCodes.CompE.id, -63, 0,
        TkCodes.MAY.id,NTCodes.Sum.id, NTCodes.CompE.id, -64, 0,
        -65, 0,
        //52
        NTCodes.Prod.id, NTCodes.SumE.id, -66, 0,
        //53
        TkCodes.SUM.id, NTCodes.Prod.id, NTCodes.SumE.id, -67, 0, 
        -68, 0,
        //55
        NTCodes.Unit.id, NTCodes.ProdE.id, -69, 0,
        //56
        TkCodes.MULT.id, NTCodes.Unit.id, NTCodes.ProdE.id, -70, 0,
        -71, 0,
        //58
        TkCodes.PARAB.id, NTCodes.Val.id, TkCodes.PARC.id, -72, 0,
        TkCodes.CTE_INT.id, -73, 0,
        TkCodes.CTE_BOOLEAN.id, -74, 0,
        TkCodes.CTE_STRING.id, -75, 0,
        TkCodes.ID.id, -76, NTCodes.UnitE.id, -77, 0,//61
        //63
        TkCodes.PARAB.id, NTCodes.Vargs.id, TkCodes.PARC.id, -78, 0, 
        -79, 0 //64
    };

    Productions() {
        prodNumToPos = new HashMap<>(num_prod);
        generateProdNumbers();
    }

    /*
     * generateProdNumbers inserta el numero de la produccion asociandolo con el 0 mas cercano por 
     * su derecha
     */
    private void generateProdNumbers() {
        int l = PROD.length;
        int nextProdNum = 1;
        for(int i = 0; i < l; i++) {
            if(PROD[i] == 0)
                prodNumToPos.put(nextProdNum++, i);
        }
        num_prod = nextProdNum - 1;
    }

    /**
     * getProdPos devuelve la posicion en la lista PROD de la produccion
     * @param codProd codigo de produccion
     * @return posicion en la lista PROD
     */
    public int getProdPos(int codProd) {
        if(codProd < 1 || codProd > num_prod)
            throw new IllegalArgumentException(String.format("Invalid prodNumber %d", codProd));
        return prodNumToPos.get(codProd);
    }

    /**
     * getByteAt devuelve el byte en PROD en pos
     * @param pos posicion en PROD
     * @return byte en PROD[pos]
     */
    public byte getByteAt(int pos) {
        if(pos < 0 || pos >PROD.length)
            throw new IllegalArgumentException();
        return PROD[pos];
    }

}
