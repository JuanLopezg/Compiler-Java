package AnLexico;

import Codes.LexActCodes;

public class Lex_AFD {
	private static final int NUM_INIT_STATES = 9;
	private static final int NUM_STATES = 27;
	private static final int NUM_CHARS = 21;
	private static final int[][] stateMatrix = rellenarMatEstados();
	/**
	 * Rellena la matriz de estados, como tenemos hecha la matriz de transiciones
	 * Si no tiene un estado al que transita, se pone -1
	 */
	private static int[][] rellenarMatEstados() {
		int[][] matrix = new int[NUM_INIT_STATES][NUM_CHARS];
		for(int i = 0; i < NUM_INIT_STATES; i++)
			for(int j = 0; j < NUM_CHARS; j++)
				matrix[i][j] = -1;
		int j, m;

		for(int i = 0; i < NUM_INIT_STATES; i = i == 3? 7: i + 1) {
			m = i == 0? 0: 
				i == 1? 9: 
				i == 2? 10:
				i == 3? 3:
				7;
			for(j = 0; j < NUM_CHARS; j++) {
				matrix[i][j] = m;
				if(i == 0) 
					m = m == 6? 15: m + 1;
			}
			
		}
		matrix[0][20] = -1; 	//(0, o.c.)
		matrix[1][1] = 1; 		//(1, letraO_)
		matrix[1][2] = 1; 		//(1, digito)
		matrix[2][2] = 2; 		//(2, digito)
		matrix[3][3] = 11; 		//(3, ")
		matrix[3][19] = -1;		//(3, EOF)
		matrix[4][4] = 12;		//(4, &)
		matrix[5][5] = 13;		//(5, |)
		matrix[6][8] = 7; 		//(6, *)
		matrix[6][11] = 14;		//(6, =)
		matrix[7][8] = 8;		//(7, *)
		matrix[7][19] = -1;		//(7, EOF)
		matrix[8][6] = 0;		//(8, /)
		matrix[8][8] = 8;		//(8, *)
		matrix[8][19] = -1;		//(8, EOF)
		return matrix;
	}
	
	/**
	 * getEstado devuelve el estado al que transita el AFD
	 * @param actualState estado actual
	 * @param c caracter c
	 * @return	nextState (Si es -1, no tiene sigEstado)
	 */
	public int getNextState(int actualState, Character c) {
		return stateMatrix[actualState][getColumn(c)];
	}

	private final static LexActCodes[] acc0_15_27 = new LexActCodes[] {LexActCodes.AC_0_15, LexActCodes.AC_0_16, LexActCodes.AC_0_17, LexActCodes.AC_0_18,
		LexActCodes.AC_0_19, LexActCodes.AC_0_20, LexActCodes.AC_0_21, LexActCodes.AC_0_22, LexActCodes.AC_0_23, LexActCodes.AC_0_24, LexActCodes.AC_0_25, 
		LexActCodes.AC_0_26, LexActCodes.AC_0_27};
	/**
	 * getAccion devuelve la accion que debe realizar segun su estado actual y su siguiente estado
	 * @param actualState estado actual
	 * @param nextState siguiente estado
	 * @return acc (null si no tiene)
	 */
	public LexActCodes getAction(int actualState, int nextState) {
		if(actualState < 0 || nextState < 0 || actualState > NUM_STATES || nextState > NUM_STATES)
			return LexActCodes.AC_ERROR;
		switch(actualState) {
			case 0:
				switch(nextState) {
					case 0: case 4: case 5: case 6:
						return LexActCodes.AC_LEER;
					case 1:
						return LexActCodes.AC_0_1;
					case 2:
						return LexActCodes.AC_0_2;
					case 3:
						return LexActCodes.AC_0_3;
					default:
						return nextState >= 15 && nextState <= 27? acc0_15_27[nextState - 15]: LexActCodes.AC_ERROR;
				}
			case 1:
				return nextState == 1? LexActCodes.AC_1_1: LexActCodes.AC_1_9;
			case 2:
				return nextState == 2? LexActCodes.AC_2_2: LexActCodes.AC_2_10;
			case 3: 
				return nextState == 11? LexActCodes.AC_3_11: LexActCodes.AC_3_3;
			case 4:
				return nextState == 12? LexActCodes.AC_4_12: LexActCodes.AC_ERROR;
			case 5:
				return nextState == 13? LexActCodes.AC_5_13: LexActCodes.AC_ERROR;
			case 6:
				return nextState == 7? LexActCodes.AC_LEER: nextState == 14? LexActCodes.AC_6_14: LexActCodes.AC_ERROR;
			case 7:
			case 8:
				return LexActCodes.AC_LEER; 
		}
		return null;
	}

	/**
	 * getColumn devuelve la columna asociada con el caracter char
	 * 		del 	-> 0
	 * 		letter 	-> 1
	 * 		digit 	-> 2
	 * 		'"'		-> 3
	 * 		'&'		-> 4
	 * 		'|'		-> 5
	 * 		'/'		-> 6
	 * 		'+'		-> 7
	 * 		'*'		-> 8
	 * 		'<'		-> 9
	 * 		'>'		-> 10
	 * 		'='		-> 11
	 * 		'('		-> 12
	 * 		')'		-> 13
	 *		'{'		-> 14
	 *		'}'		-> 15
	 *      ':'		-> 16
	 * 		','		-> 17
	 * 		';'		-> 18
	 * 		'EOF'	-> 19
	 * 		o.c		-> 20 / cualquier otro caracter
	 * @param car caracter que se pasa
	 * @return la columna a la que se asocia ese caracter
	 */
    private int getColumn(Character car) {
		if(car == null)
			return 19;
		if(isADelimiter(car))
			return 0;
		if(isALetterOr_(car))
			return 1;
		if(isADigit(car))
			return 2;

		switch(car) {
			case '"': return 3;
			case '&': return 4;
			case '|': return 5;
			case '/': return 6;
			case '+': return 7;
			case '*': return 8;
			case '<': return 9;
			case '>': return 10;
			case '=': return 11;
			case '(': return 12;
			case ')': return 13;
			case '{': return 14;
			case '}': return 15;
			case ':': return 16;
			case ',': return 17;
			case ';': return 18;
		}
    	return 20;
    }

	/**
	 * Comprueba si es una letra o un '_'
	 * @param c caracter
	 * @return true si lo es
	 */
	public boolean isALetterOr_(char c) {
		int low = c - 'a';
		int high = c - 'A';
		return (c == '_') || (low >= 0 && low <= 'z' - 'a') || (high >= 0 && high <= 'z' - 'a');
	}

	/**
	 * Comprueba si es un digito
	 * @param c
	 * @return true si lo es
	 */
	public boolean isADigit(char c) {
		int val = c - '0';
		return val >= 0 && val <= '9' - '0';
	}

	/**
	 * Comprueba si es un delimitador
	 * 9 -> '/t' tab
	 * 10 -> '\n' nL
	 * 13 -> '\r'
	 * 32 -> ' '
	 * @param c
	 * @return
	 */
	public boolean isADelimiter(char c) {
        return c == 9 || c == 10 || c == 13 || c == 32;
	}

	/**
	 * Usado para comprobar que no habia errores
	 * @return la matriz de estados
	 */
	@Override
	public String toString() {
		String actual = "Mat_Estados:\n";

		for(int i = 0; i < NUM_INIT_STATES; i++) {
			for(int j = 0; j < NUM_CHARS; j++) {
				int nextState = stateMatrix[i][j];
				LexActCodes acc = getAction(i, nextState);
				String accName = acc == null? "N": acc.name();
				actual = String.format("%s|%d->%s", actual, nextState, accName);
			}
			actual = String.format("%s\n", actual);
		}
		return actual;
		
	}
}
