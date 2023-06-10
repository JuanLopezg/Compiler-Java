package SemActions;

import SymbolTable.SymbolTable;

import Codes.TCodes;
import CustomStack.ArrayStack;
import ErrorManager.ErrorHandler;

public class SemActions {
    private final SymbolTable TS;
    private final ErrorHandler errorHandler;
    private final ArrayStack<Symbol> P,  AUX;

    public SemActions(ErrorHandler errorHandler, ArrayStack<Symbol> P, ArrayStack<Symbol> AUX) {
        this.errorHandler = errorHandler;
        this.TS = errorHandler.getTS();
        this.P = P;
        this.AUX = AUX;
    }

    /**
     * runSemAction realiza la accion semantica indicada con el semCode.
     * Hemos anadido comentarios al lado de los casos del switch, para si habia algun error
     * poderlo ver mas directamente.
     * @param semCode codigo de la accion semantica
     */
    public void runSemAction(byte semCode) {
        if(semCode > 0) // We will use negative bytes to indicate the action code
            throw new IllegalArgumentException("Not a Semantic Action");
        Symbol syntSym, auxSym1, auxSym2;
        Type auxType1, auxType2;
        byte auxBt;
        int desp, aux;
        switch(semCode) {
            case -1: // PE -> {} P
                TS.createTable("Principal"); break;
            case -2: // PE -> P {}
                auxType1 = AUX.getFromTop(0).getTipo(); // P.tipo
                if(auxType1.typeValIn(TCodes.tipo_err.id))
                    errorHandler.semErrorCallback();
                AUX.pop(1);
                TS.deleteActualTable(); break;
            case -3: // P -> SentC P1 {}
            case -4: // P -> F P1 {}
                syntSym = AUX.getFromTop(2); // P
                auxSym1 = AUX.getFromTop(0); // P1
                auxType1 = AUX.getFromTop(1).getTipo(); // SentC.tipo o F.tipo
                // SentC.tipo = tipo_ok o F.tipo = tipo_ok
                if(auxType1.typeValIn(TCodes.tipo_ok.id))
                    syntSym.setTipo(auxSym1.getTipo()); // P.tipo = P1.tipo
                else
                    syntSym.setTipo(TCodes.tipo_err.id);
                AUX.pop(2); break;
            case -5: // P -> Lambda {}
            case -29:// Ig -> Lambda {}
                AUX.getFromTop(0).setTipo(TCodes.tipo_ok.id); // P.tipo = tipo_ok
                break;
            case -6: // SentS -> print Val ; {}
            case -8: // SentS -> return ValRet ; {}
                syntSym = AUX.getFromTop(3); // SentS
                auxSym1 = AUX.getFromTop(1); // Val o Id o ValRet
                auxType1 = auxSym1.getTipo(); // Val.tipo or BuscaTipoTS(Id.posTS) o ValRet.tipo
                if(!auxType1.typeValIn(TCodes.tipo_err.id))
                    syntSym.setTipo(TCodes.tipo_ok.id);
                else
                    syntSym.setTipo(TCodes.tipo_err.id);
                if(semCode == -6)
                    syntSym.setTipoRet(TCodes.vacio.id);
                else {
                    if(syntSym.inFunc)
                        syntSym.setTipoRet(TCodes.vacio.id);
                    else{
                        syntSym.setTipoRet(TCodes.tipo_err.id);
                        errorHandler.pushSemError(80);
                    }
                }
                syntSym.setTipoRet(semCode != -8? TCodes.vacio.id:
                    syntSym.inFunc? auxType1:  TCodes.tipo_err.id);
                AUX.pop(3); break;
            case -7: // SentS -> input Id ; {}
                syntSym = AUX.getFromTop(3);
                aux = AUX.getFromTop(1).getPosTS();
                auxType1 = TS.searchType(aux);
                if(auxType1.typeValIn(TCodes.cadena.id, TCodes.entero.id))
                    syntSym.setTipo(TCodes.tipo_ok.id);
                else {
                    syntSym.setTipo(TCodes.tipo_err.id);
                    errorHandler.pushSemError(81, TS.getIdName(aux));
                }
                syntSym.setTipoRet(TCodes.vacio.id);
                AUX.pop(3); break;
            case -9: // SentS -> break ; {}
                syntSym = AUX.getFromTop(2);
                if(syntSym.inSwitch)
                    syntSym.setTipo(TCodes.tipo_ok.id);
                else {
                    syntSym.setTipo(TCodes.tipo_err.id);
                    errorHandler.pushSemError(82);
                }
                syntSym.setTipoRet(TCodes.vacio.id);
                AUX.pop(2); break;
            case -10:// SentS -> Id {} ExpDec ;
            case -76:// Unit -> Id {} UnitE
            case -80:// SentS -> input Id {} ;
                auxSym1 = AUX.getFromTop(0); // Id
                auxType1 = TS.searchType(auxSym1.getPosTS());
                // BuscaTipoTS(id.posTS) != tipo_err
                if(auxType1.typeValIn(TCodes.tipo_err.id))
                    TS.insertTypeAndDesp(auxSym1.getPosTS(), new Type(TCodes.entero.id), 1);
                if(semCode != -80)
                    P.peek().setPosTS(auxSym1.getPosTS()); //ExpDec.posTS = Id.posTS
                break;
            case -11:// SentS -> Id ExpDec ; {}
                syntSym = AUX.getFromTop(3); // SentS
                syntSym.setTipo(AUX.getFromTop(1).getTipo());
                syntSym.setTipoRet(TCodes.vacio.id);
                AUX.pop(3); break;
            case -12: // ExpDec -> = Val {}
            case -14: // ExpDec -> /= Val {}
            case -28: // Ig -> = Val {}
                syntSym = AUX.getFromTop(2); // ExpDec o Ig
                aux = syntSym.getPosTS();
                auxType1 = TS.searchType(aux); // BuscaTipoTS(Ig.posTS o ExpDec.posTS)
                auxType2 = AUX.getFromTop(0).getTipo(); // Val.tipo
                if(auxType1.equals(auxType2)){
                    if(semCode == -14 && auxType1.typeValIn(TCodes.entero.id))
                        syntSym.setTipo(TCodes.tipo_ok.id);
                    else if(semCode != -14 && !auxType1.typeValIn(TCodes.tipo_err.id))
                        syntSym.setTipo(TCodes.tipo_ok.id);
                    else 
                        syntSym.setTipo(TCodes.tipo_err.id);
                } else {
                    syntSym.setTipo(TCodes.tipo_err.id);
                    if(auxType1.typeValIn(TCodes.func.id))
                        errorHandler.pushSemError(83, TS.getIdName(aux));
                    else if(!auxType2.typeValIn(TCodes.tipo_err.id)) {
                        String out = String.format("%s al identificador '%s' de tipo %s", auxType2, TS.getIdName(aux), auxType1);
                        errorHandler.pushSemError(84, out);
                    }

                }
                AUX.pop(2); break;
            case -13: // ExpDec -> ( Vargs ) {}
            case -78: // UnitE -> ( Vargs ) {}
                syntSym = AUX.getFromTop(3);
                aux = syntSym.getPosTS();
                auxType1 = TS.searchType(aux); // BuscarTipoTS(ExpDec.posTS)
                auxType2 = AUX.getFromTop(1).getTipo(); // VArgs
                if(auxType1.typeValIn(TCodes.func.id)) {
                    auxBt = auxType1.equalsArgs(auxType2); // t
                    if(auxBt != TCodes.tipo_err.id) {
                        if (semCode == -13)
                            auxBt = TCodes.tipo_ok.id;
                        else if (auxBt == TCodes.vacio.id) {
                            errorHandler.pushSemError(100, TS.getIdName(aux));
                            auxBt = TCodes.tipo_err.id;
                        }
                    } else {
                        String out = String.format("'%s' (%s), esta recibiendo estos parametros %s",
                                TS.getIdName(aux), auxType1, auxType2);
                        errorHandler.pushSemError(86, out);
                    }
                } else {
                    auxBt = TCodes.tipo_err.id;
                    errorHandler.pushSemError(85, TS.getIdName(aux));
                }
                syntSym.setTipo(auxBt);
                AUX.pop(3); break;
            case -15: // Vargs -> Val VargsE {} 
            case -17: // VargsE -> , Val VargsE1 {}
                desp = semCode == -15? 2: 3;
                syntSym = AUX.getFromTop(desp);
                auxType1 = AUX.getFromTop(0).getTipo(); // VArgsE
                auxType1.pushTypeVal(AUX.getFromTop(1).getTipo()); // Val.tipo x VArgsE.tipo
                syntSym.setTipo(auxType1);
                AUX.pop(desp); break;
            case -16: // Vargs -> Lambda {}
            case -18: // VargsE -> Lambda {}
            case -48: // Args -> Lambda {}
            case -50: // ArgsE -> Lambda {}
                syntSym = AUX.getFromTop(0);
                // Starts the list and adds a vacio type
                syntSym.setTipo(new Type(TCodes.list.id));
                break;
            case -19: // SentC -> {} let Id Type Ig ;
                TS.zona_dec = true; break;
            case -20: // SentC -> let Id Type {} Ig ;
                aux = AUX.getFromTop(1).getPosTS(); //Id.posTS
                auxSym1 = AUX.getFromTop(0); // Type
                if(TS.searchType(aux).typeValIn(TCodes.tipo_err.id))
                    TS.insertTypeAndDesp(aux, auxSym1.getTipo(), auxSym1.getAncho());
                else
                    errorHandler.pushSemError(101, TS.getIdName(aux));
                TS.zona_dec = false;
                P.peek().setPosTS(aux); // Ig.tipo = Type.tipo
                break;
            case -21: // SentC -> let Id Type Ig ; {}
                syntSym = AUX.getFromTop(5);
                auxSym1 = AUX.getFromTop(1);
                syntSym.setTipo(auxSym1.getTipo());
                syntSym.setTipoRet(TCodes.vacio.id);
                AUX.pop(5); break;
            case -22: // SentC -> if ( Val ) {} SentS
            case -26: // SentC -> {} SentS
            case -39: // SentSw -> {} SentC SentSw1
                desp = semCode == -22? 4: 0;  // SentC o SentSw
                syntSym = AUX.getFromTop(desp);
                auxSym1 = P.peek();
                auxSym1.inFunc = syntSym.inFunc;
                if(semCode == -39)
                    auxSym1.inSwitch = true;
                else
                    auxSym1.inSwitch = syntSym.inSwitch;
                break;
            case -23: // SentC -> if ( Val ) SentS {}
                syntSym = AUX.getFromTop(5); // SentC
                auxType1 = AUX.getFromTop(2).getTipo(); // Val.tipo
                auxSym1 = AUX.getFromTop(0); // SentS
                if(auxType1.typeValIn(TCodes.bool.id))
                    syntSym.setTipo(auxSym1.getTipo());
                else {
                    pushInvalidTypeError(87, auxType1);
                    syntSym.setTipo(TCodes.tipo_err.id);
                }

                syntSym.setTipoRet(auxSym1.getTipoRet());
                AUX.pop(5); break;
            case -24: // SentC -> switch ( Val ) { {} BloqSw }
            case -33: // BloqSw -> case Val : {} SentSw BloqSw1
            case -34: // BloqSw -> case Val : SentSw {} BloqSw1
            case -36: // BloqSw -> default : {} SentSw
            case -83: // SentSw -> SentC {} SentSw1
                desp = semCode == -24? 5:
                    semCode == -33? 3:
                    semCode == -34? 4:
                    semCode == -36? 2: 1;
                P.peek().inFunc = AUX.getFromTop(desp).inFunc;
                break;
            case -25: // SentC -> switch ( Val ) { BloqSw } {}
                syntSym = AUX.getFromTop(7);
                auxType1 = AUX.getFromTop(4).getTipo(); // Val.tipo
                auxSym1 = AUX.getFromTop(1); // BloqSw
                if(auxType1.typeValIn(TCodes.entero.id))
                    syntSym.setTipo(auxSym1.getTipo());
                else {
                    pushInvalidTypeError(88, auxType1);
                    syntSym.setTipo(TCodes.tipo_err.id);
                }
                syntSym.setTipoRet(auxSym1.getTipoRet());
                AUX.pop(7); break;
            case -27: // SentC -> SentS {}
            case -37: // BloqSw -> default : SentSw {}
                desp = semCode == -27? 1: 3;
                syntSym = AUX.getFromTop(desp);
                auxSym1 = AUX.getFromTop(0);
                syntSym.setTipo(auxSym1.getTipo());
                syntSym.setTipoRet(auxSym1.getTipoRet());
                AUX.pop(desp); break;
            case -30://Type -> int {}
            case -31://Type -> string {}
            case -32://Type -> boolean {}
                syntSym = AUX.getFromTop(1);
                if(semCode == -30) {
                    auxBt = TCodes.entero.id; aux = 1;
                } else if(semCode == -31) {
                    auxBt = TCodes.cadena.id; aux = 64;
                } else {
                    auxBt = TCodes.bool.id; aux = 1;
                }
                syntSym.setTipo(auxBt);
                syntSym.setAncho(aux);
                AUX.pop(1); break;
            case -35: // BloqSw -> case Val : SentSw BloqSw1 {}
                syntSym = AUX.getFromTop(5); // BloqSw
                auxSym1 = AUX.getFromTop(1); // SentSw
                auxSym2 = AUX.getFromTop(0); // BloqSw1
                auxType1 = AUX.getFromTop(3).getTipo(); // Val.tipo
                if(auxType1.typeValIn(TCodes.entero.id))
                    if(auxSym1.getTipo().typeValIn(TCodes.tipo_ok.id))
                        syntSym.setTipo(auxSym2.getTipo()); // BloqSw1.tipo
                    else
                        syntSym.setTipo(TCodes.tipo_err.id);
                else {
                    pushInvalidTypeError(89, auxType1);
                    syntSym.setTipo(TCodes.tipo_err.id);
                }
                // SentSw.tipoRet and BloqSw1.tipoRet
                syntSym.setTipoRet(getEqualsOrVacioType(90, auxSym1.getTipoRet(), auxSym2.getTipoRet()));
                AUX.pop(5); break;
            case -38: // BloqSw -> Lambda {}
            case -41: // SentSw -> Lambda {}
            case -53: // BloqF -> Lambda {}
                syntSym = AUX.getFromTop(0);
                syntSym.setTipo(TCodes.tipo_ok.id);
                syntSym.setTipoRet(TCodes.vacio.id); break;
            case -40: // SentSw -> SentC SentSw1 {}
            case -52: // BloqF -> SentC BloqF1 {}
                syntSym = AUX.getFromTop(2); // SentSw o BloqF
                auxSym1 = AUX.getFromTop(1); // SentC
                auxSym2 = AUX.getFromTop(0); // SentSw1 o BloqF1
                if(auxSym1.getTipo().typeValIn(TCodes.tipo_ok.id))
                    syntSym.setTipo(auxSym2.getTipo());
                else
                    syntSym.setTipo(TCodes.tipo_err.id);
                syntSym.setTipoRet(getEqualsOrVacioType(91, auxSym1.getTipoRet(), auxSym2.getTipoRet()));
                AUX.pop(2); break;
            case -42: // F -> function Id {} TypeE ( Args ) { BloqF }
                auxSym1 = AUX.getFromTop(0); // Id
                TS.createTable(TS.getIdName(auxSym1.getPosTS()));
                TS.zona_dec = true; break;
            case -43: // F -> function Id TypeE ( Args ) {} { BloqF }
                aux = AUX.getFromTop(4).getPosTS(); // Id.posTS
                auxType1 = AUX.getFromTop(3).getTipo(); // TypeE.tipo
                auxType2 = AUX.getFromTop(1).getTipo(); // Args.tipo
                TS.insertType(aux, new Type(auxType2, auxType1));
                TS.insertEtq(aux);
                TS.zona_dec = false; break;
            case -44: // F -> function Id TypeE ( Args ) { BloqF } {}
                syntSym = AUX.getFromTop(9); // F
                auxSym1 = AUX.getFromTop(1); // BloqF
                auxType1 = AUX.getFromTop(6).getTipo(); // TypeE.tipo
                if(auxSym1.getTipoRet().equals(auxType1))
                    syntSym.setTipo(auxSym1.getTipo());
                else {
                    if(!auxSym1.getTipoRet().typeValIn(TCodes.tipo_err.id)){
                        aux = AUX.getFromTop(7).getPosTS();
                        String out = String.format("En la funcion '%s' se espera que se devuelva %s, pero esta tratando de devolver %s",
                                TS.getIdName(aux), auxType1, auxSym1.getTipoRet());
                        errorHandler.pushSemError(92, out);
                    }
                    syntSym.setTipo(TCodes.tipo_err.id);
                }
                TS.deleteActualTable(); AUX.pop(9); break;
            case -45: // TypeE -> Type {}
            case -54: // ValRet -> Val {}
                syntSym = AUX.getFromTop(1);
                syntSym.setTipo(AUX.getFromTop(0).getTipo());
                AUX.pop(1); break;
            case -46: // TypeE -> Lambda {}
            case -55: // ValRet -> Lambda {}
            case -58: // ValE -> Lambda {}
            case -61: // AndE -> Lambda {}
            case -65: // CompE -> Lambda {}
            case -68: // SumE -> Lambda {}
            case -71: // ProdE -> Lambda {}
            case -79: // UnitE -> Lambda {}
                AUX.getFromTop(0).setTipo(TCodes.vacio.id); break;
            case -81: // Args -> Type Id {} ArgsE
            case -82: // ArgsE -> , Type Id {} ArgsE1
                aux = AUX.getFromTop(0).getPosTS();
                auxSym1 = AUX.getFromTop(1);
                if(TS.searchType(aux).typeValIn(TCodes.tipo_err.id))
                    TS.insertTypeAndDesp(aux, auxSym1.getTipo(), auxSym1.getAncho());
                else
                    errorHandler.pushSemError(101, TS.getIdName(aux));
                break;
            case -47: // Args -> Type Id ArgsE {}
            case -49: // ArgsE -> , Type Id ArgsE1 {}
                desp = semCode == -47? 3: 4;
                syntSym = AUX.getFromTop(desp);
                auxType1 = AUX.getFromTop(0).getTipo(); // ArgsE.tipo
                auxType2 = AUX.getFromTop(2).getTipo(); // Type.tipo
                auxType1.pushTypeVal(auxType2);
                syntSym.setTipo(auxType1);
                AUX.pop(desp); break;
            case -51: // BloqF -> {} SentC BloqF1
                P.peek().inFunc = true; break;
            case -56: // Val -> And ValE
            case -59: // And -> Comp AndE
            case -62: // Comp -> Sum CompE
            case -66: // Sum -> Prod SumE
            case -69: // Prod -> Unit ProdE
                syntSym = AUX.getFromTop(2);
                auxType1 = AUX.getFromTop(0).getTipo(); // E
                auxType2 = AUX.getFromTop(1).getTipo();
                boolean t1eqt2 = auxType1.equals(auxType2);
                if(semCode == -62 && t1eqt2 && auxType1.typeValIn(TCodes.entero.id))
                    syntSym.setTipo(TCodes.bool.id);
                else if((semCode != -62 && t1eqt2) || auxType1.typeValIn(TCodes.vacio.id))
                    syntSym.setTipo(auxType2);
                else {
                    pushInvalidOperation(semCode, auxType2, auxType1);
                    syntSym.setTipo(TCodes.tipo_err.id);
                }
                AUX.pop(2); break;
            case -57: // ValE -> || And ValE1 {}
            case -60: // AndE -> && Comp AndE1 {}
            case -67: // SumE -> + Prod SumE1 {}
            case -70: // ProdE -> * Unit ProdE1 {}
                syntSym = AUX.getFromTop(3);
                auxType1 = AUX.getFromTop(0).getTipo(); // ValE1.tipo
                auxType2 = AUX.getFromTop(1).getTipo(); // And.tipo
                auxBt = semCode >= -60 ? TCodes.bool.id : TCodes.entero.id;
                if (auxType2.typeValIn(auxBt) && auxType1.typeValIn(auxBt, TCodes.vacio.id))
                    syntSym.setTipo(auxType2);
                else {
                    pushInvalidOperation(semCode, auxType2, auxType1);
                    syntSym.setTipo(TCodes.tipo_err.id);
                }
                AUX.pop(3); break;

            case -63: // CompE -> < Sum CompE1 {}
            case -64: // CompE -> > Sum CompE1 {}
                syntSym = AUX.getFromTop(3);
                auxType1 = AUX.getFromTop(0).getTipo(); // CompE1.tipo
                auxType2 = AUX.getFromTop(1).getTipo(); // And.tipo
                if(auxType2.typeValIn(TCodes.entero.id) && auxType1.typeValIn(TCodes.vacio.id))
                    syntSym.setTipo(auxType2);
                else {
                    pushInvalidOperation(semCode, auxType2, auxType1);
                    syntSym.setTipo(TCodes.tipo_err.id);
                }
                AUX.pop(3); break;
            case -72: // Unit -> ( Val ) {}
                syntSym = AUX.getFromTop(3);
                syntSym.setTipo(AUX.getFromTop(1).getTipo());
                AUX.pop(3); break;
            case -73: // Unit -> Cte_int
            case -74: // Unit -> Cte_bool
            case -75: // Unit -> Cte_str
                auxBt = semCode == -73? TCodes.entero.id:
                    semCode == -74? TCodes.bool.id: TCodes.cadena.id;
                syntSym = AUX.getFromTop(1);
                syntSym.setTipo(auxBt);
                AUX.pop(1); break;
            case -77: // Unit -> Id UnitE {}
                syntSym = AUX.getFromTop(2);
                aux = AUX.getFromTop(1).getPosTS();
                auxType1 = AUX.getFromTop(0).getTipo();
                if(auxType1.typeValIn(TCodes.vacio.id))
                    syntSym.setTipo(TS.searchType(aux));
                else
                    syntSym.setTipo(auxType1);
                AUX.pop(2); break;
        }
    }

    /**
     * pushInvalidTypeError si el tipo no es ya un tipo_err, lo emitimos a la pila del
     * gestor de errores semanticos.
     * @param errorCode codigo del error
     * @param invalType tipo invalido
     */
    private void pushInvalidTypeError(int errorCode, Type invalType) {
        if(!invalType.typeValIn(TCodes.tipo_err.id))
            errorHandler.pushSemError(errorCode, invalType.toString());
    }

    /**
     * getEqualsOrVacioType realiza la accion semantica
     * if NT1.tipo = NT2.tipo or NT2.tipoRet = vacio then NT1.tipo
     * 		elif NT1.tipo = vacio then NT2.tipo else tipo_err
     * @param errorCode codigo del error a emitir si no se cumplen las condiciones
     * @param type1 tipo de NT1
     * @param type2 tipo de NT2
     * @return el tipo
     */
    private Type getEqualsOrVacioType(int errorCode, Type type1, Type type2) {
        if(type1 == null || type2 == null)
            throw new IllegalArgumentException("Check as this shouldn't be happening in any case\n");
        if(type1.equals(type2) || type2.typeValIn(TCodes.vacio.id))
            return type1;
        else if(type1.typeValIn(TCodes.vacio.id))
            return type2;
        if(!type1.typeValIn(TCodes.tipo_err.id) && !type2.typeValIn(TCodes.tipo_err.id))
            errorHandler.pushSemError(errorCode, String.format("%s y %s", type1, type2));
        return new Type(TCodes.tipo_err.id);
    }

    /**
     * pushInvalidOperation emite un codigo de error si no ha sido emitido antes
     * @param actionCode codigo de accion
     * @param type tipo NT
     * @param typeE tipo NTE
     */
    private void pushInvalidOperation(byte actionCode, Type type, Type typeE) {
        if(type.typeValIn(TCodes.tipo_err.id) || typeE.typeValIn(TCodes.tipo_err.id))
            return;
        int errorCode = 0;
        String typeString = "";
        switch(actionCode) {
            case -56: // Val -> And ValE -> bool || bool
            case -57: // ValE -> || And ValE1 {}
                errorCode = 93; typeString = type.toString();break;
            case -59: // And -> Comp AndE -> bool && bool
            case -60: // AndE -> && Comp AndE1 {}
                errorCode = 94; typeString = type.toString(); break;
            case -62: // Comp -> Sum CompE -> int < int o int > int
                errorCode = 95; typeString = "bool"; break;
            case -63: // CompE -> < Sum CompE1 {}
                errorCode = 96; typeString = "bool"; break;
            case -64: // CompE -> > Sum CompE1 {}
                errorCode = 97; typeString = type.toString();break;
            case -66: // Sum -> Prod SumE -> int + int
            case -67: // SumE -> + Prod SumE1
                errorCode = 98; typeString = type.toString();break;
            case -69: // Prod -> Unit ProdE -> int * int
            case -70: // ProdE -> * Unit ProdE1
                errorCode = 99; typeString = type.toString();break;
        }
        errorHandler.pushSemError(errorCode, typeString);
    }
}
