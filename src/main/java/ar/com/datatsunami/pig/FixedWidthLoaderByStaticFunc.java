package ar.com.datatsunami.pig;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.linehandler.LineHandler;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class FixedWidthLoaderByStaticFunc extends FixedWidthLoader {

	/**
	 * UDF for Pig to load data and to reuse the format from a CobolDumpParser
	 * instance.
	 * 
	 * This UDF doesn't support all the datatypes that CobolDumpParser supports.
	 * 
	 * @param staticFunction
	 * @param fields
	 */
	public FixedWidthLoaderByStaticFunc(String staticFunction, String fields) {
		this(get(staticFunction, fields));
	}

	/**
	 * Instantiates CobolDumpParser
	 * 
	 * @param staticFunction
	 *            Reference to static functions that creates the CobolDumpParser
	 *            instance
	 * @return
	 */
	public static String[] get(String staticFunction, String fields) {

		/*
		 * Call static method
		 */
		Object ret = StaticMethodCaller.call(staticFunction);

		if (ret == null)
			throw new RuntimeException("The method referenced by '" + staticFunction
					+ "' didn't returned anything");

		if (!CobolDumpParser.class.isAssignableFrom(ret.getClass())) {
			throw new RuntimeException("The method referenced by '" + staticFunction
					+ "' returned an instance of typ '" + ret.getClass().getCanonicalName()
					+ "', which is not a CobolDumpParser instance.");
		}

		/*
		 * Check received object & validate CobolDumpParser
		 */

		CobolDumpParser cobolDumpParser = (CobolDumpParser) ret;
		LineHandler lineHandler = cobolDumpParser.getLineHandler();

		if (!PositionalLineHandler.class.isAssignableFrom(lineHandler.getClass())) {
			throw new RuntimeException("The lineHandler if of type '"
					+ lineHandler.getClass().getCanonicalName() + "', which is not a PositionalLineHandler.");
		}

		/*
		 * Parse field indexes
		 */
		int fieldIndexes[];
		if ("*".equals(fields.trim())) {
			fieldIndexes = new int[cobolDumpParser.getFieldCount()];
			for (int i = 0; i < fieldIndexes.length; i++)
				fieldIndexes[i] = i;
		} else {
			String fieldTokens[] = fields.split(",");
			fieldIndexes = new int[fieldTokens.length];
			for (int i = 0; i < fieldTokens.length; i++) {
				try {
					fieldIndexes[i] = Integer.valueOf(fieldTokens[i]);
				} catch (NumberFormatException nfe) {
					fieldIndexes[i] = cobolDumpParser.getFieldIndexFromFieldName(fieldTokens[i]);
				}
			}
		}

		PositionalLineHandler positionalLineHandler = (PositionalLineHandler) lineHandler;

		return positionalLineHandler.getFixedWidthLoaderSpec(fieldIndexes);
	}

	/**
	 * DON'T USE THIS! Used internally.
	 */
	protected FixedWidthLoaderByStaticFunc(String[] columnSpecAndSchemaStr) {
		super(columnSpecAndSchemaStr[0], "", columnSpecAndSchemaStr[1]);
	}

}
