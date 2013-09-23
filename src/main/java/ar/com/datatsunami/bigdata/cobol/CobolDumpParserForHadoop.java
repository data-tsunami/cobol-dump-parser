package ar.com.datatsunami.bigdata.cobol;

import org.apache.hadoop.io.BytesWritable;
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

	/**
	 * Creates a CobolDumpParserForHadoop instance.
	 */
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
		return getValuesFromTextFast(fieldIndexes);
	}

	/*
	 * Fast version
	 */
	public Object[] getValuesFromTextFast(int... fieldIndexes) throws ParserException {
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
		return getValueFromTextFast(fieldIndex);
	}

	/*
	 * Fast version
	 */
	public Object getValueFromTextFast(int fieldIndex) throws ParserException {
		String string = this.lineHandler.getValueForField(fieldIndex);
		return getObjectFromString(string, fields.get(fieldIndex));
	}

	/**
	 * Copies the value of the fields to the Text instances.
	 */
	public void copyValuesToTextArray(Text text, int[] fieldIndexes, Text[] out) throws ParserException {
		this.lineHandler.prepareText(text);
		copyValuesToTextArrayFast(fieldIndexes, out);
	}

	/*
	 * Fast version
	 */
	public void copyValuesToTextArrayFast(int[] fieldIndexes, Text[] out) throws ParserException {
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

	/*
	 * Fast version
	 */
	public void copyValueToTextFast(int fieldIndex, Text out) throws ParserException {
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
		copyValuesToObjectArrayFast(fieldIndexes, out);
	}

	/*
	 * Fast version
	 */
	public void copyValuesToObjectArrayFast(int[] fieldIndexes, Object[] out) throws ParserException {
		for (int i = 0; i < fieldIndexes.length; i++) {
			out[i] = getObjectFromString(this.lineHandler.getValueForField(fieldIndexes[i]),
					fields.get(fieldIndexes[i]));
		}
	}

	/**
	 * Copy the values of the fields referenced by <code>fieldIndexes</code>
	 * from the Text instance to the specified BytesWritables.
	 */
	public void writeValuesToBytesWritableArray(Text text, int[] fieldIndexes, BytesWritable[] out)
			throws ParserException {
		this.lineHandler.prepareText(text);
		writeValuesToBytesWritableArrayFast(fieldIndexes, out);
	}

	/*
	 * Fast version
	 */
	public void writeValuesToBytesWritableArrayFast(int[] fieldIndexes, BytesWritable[] out)
			throws ParserException {
		for (int i = 0; i < fieldIndexes.length; i++) {
			this.lineHandler.copyBytes(fieldIndexes[i], out[i]);
		}
	}

}
