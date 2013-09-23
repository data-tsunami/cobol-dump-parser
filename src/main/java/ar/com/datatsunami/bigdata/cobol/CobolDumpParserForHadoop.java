package ar.com.datatsunami.bigdata.cobol;

import org.apache.hadoop.io.Text;

import ar.com.datatsunami.bigdata.cobol.linehandler.LineHandler;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class CobolDumpParserForHadoop extends CobolDumpParser {

	/**
	 * Creates a CobolDumpParserForHadoop instance, with an instance of
	 * 'PositionalLineHandler' as line handler.
	 */
	public CobolDumpParserForHadoop() {
		super(new PositionalLineHandler());
	}

	public CobolDumpParserForHadoop(LineHandler lineHandler) {
		super(lineHandler);
	}

	/**
	 * Get the values of the fields referenced by <code>fieldIndexes</code> from
	 * the Text instance. This avoid the conversion of the entire line to
	 * String.
	 */
	public Object[] getValuesFromText(Text text, int... fieldIndexes) throws ParserException {
		this.lineHandler.prepareText(text);
		Object[] ret = new Object[fieldIndexes.length];

		for (int i = 0; i < fieldIndexes.length; i++) {
			String string = this.lineHandler.getValueForField(fieldIndexes[i]);
			ret[i] = getObjectFromString(string, fields.get(fieldIndexes[i]));
		}
		return ret;
	}

	/**
	 * Like <code>getValuesFromText()</code> but only for a field.
	 */
	public Object getValueFromText(Text text, int fieldIndex) throws ParserException {
		this.lineHandler.prepareText(text);
		String string = this.lineHandler.getValueForField(fieldIndex);
		return getObjectFromString(string, fields.get(fieldIndex));
	}

	public void copyValuesToTextArray(Text text, int[] fieldIndexes, Text[] out) throws ParserException {
		this.lineHandler.prepareText(text);
		for (int i = 0; i < fieldIndexes.length; i++) {
			this.lineHandler.copyValue(fieldIndexes[i], out[i]);
		}
	}

	/**
	 * Copies the value of the field from one text instance to another.
	 */
	public void copyValueToText(Text textIn, int fieldIndex, Text out) throws ParserException {
		this.lineHandler.prepareText(textIn);
		this.lineHandler.copyValue(fieldIndex, out);
	}

	/**
	 * Copy the values of the fields referenced by <code>fieldIndexes</code>
	 * from the Text instance to the specified Object[] array. This avoid the
	 * conversion of the entire line to String, and allows the reuse of the
	 * Object[] array.
	 */
	public void copyValuesToObjectArray(Text text, int[] fieldIndexes, Object[] out) throws ParserException {
		this.lineHandler.prepareText(text);
		for (int i = 0; i < fieldIndexes.length; i++) {
			out[i] = getObjectFromString(this.lineHandler.getValueForField(fieldIndexes[i]),
					fields.get(fieldIndexes[i]));
		}
	}

}
