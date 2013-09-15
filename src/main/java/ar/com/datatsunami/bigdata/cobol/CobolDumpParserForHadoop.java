package ar.com.datatsunami.bigdata.cobol;

import org.apache.hadoop.io.Text;

import ar.com.datatsunami.bigdata.cobol.linehandler.LineHandler;

public class CobolDumpParserForHadoop extends CobolDumpParser {

	public CobolDumpParserForHadoop() {
		super();
	}

	public CobolDumpParserForHadoop(LineHandler lineHandler) {
		super(lineHandler);
	}

	/**
	 * The same than {@link getItemsValues(String, String[])} , but receives a
	 * {@link Text} instead of a String.
	 * 
	 * @param text
	 * @param fieldsNames
	 * @return
	 * @throws ParserException
	 */
	public Object[] getValuesFromText(Text text, int[] fieldIndexes) throws ParserException {
		this.lineHandler.prepareText(text);
		Object[] ret = new Object[fieldIndexes.length];

		for (int i = 0; i < fieldIndexes.length; i++) {
			String string = this.lineHandler.getValueForField(fieldIndexes[i]);
			ret[i] = getObjectFromString(string, fields.get(fieldIndexes[i]));
		}
		return ret;
	}

	@Deprecated
	public Object[] getItemsValuesFromText(Text text, int[] fieldIndexes) throws ParserException {
		return getValuesFromText(text, fieldIndexes);
	}

	public void copyValuesToTextArray(Text text, int[] fieldIndexes, Text[] out) throws ParserException {
		this.lineHandler.prepareText(text);
		for (int i = 0; i < fieldIndexes.length; i++) {
			this.lineHandler.copyValue(fieldIndexes[i], out[i]);
		}
	}

	public void copyValuesToObjectArray(Text text, int[] fieldIndexes, Object[] out) throws ParserException {
		this.lineHandler.prepareText(text);
		for (int i = 0; i < fieldIndexes.length; i++) {
			out[i] = getObjectFromString(this.lineHandler.getValueForField(fieldIndexes[i]),
					fields.get(fieldIndexes[i]));
		}
	}

}
