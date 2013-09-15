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

	public Object[] getValuesFromText(Text text, int[] fieldIndexes) throws ParserException {
		this.lineHandler.prepareText(text);
		Object[] ret = new Object[fieldIndexes.length];

		for (int i = 0; i < fieldIndexes.length; i++) {
			String string = this.lineHandler.getValueForField(fieldIndexes[i]);
			ret[i] = getObjectFromString(string, fields.get(fieldIndexes[i]));
		}
		return ret;
	}

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

	public void copyValueToText(Text textIn, int fieldIndex, Text out) throws ParserException {
		this.lineHandler.prepareText(textIn);
		this.lineHandler.copyValue(fieldIndex, out);
	}

	public void copyValuesToObjectArray(Text text, int[] fieldIndexes, Object[] out) throws ParserException {
		this.lineHandler.prepareText(text);
		for (int i = 0; i < fieldIndexes.length; i++) {
			out[i] = getObjectFromString(this.lineHandler.getValueForField(fieldIndexes[i]),
					fields.get(fieldIndexes[i]));
		}
	}

}
