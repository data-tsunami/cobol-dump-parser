package ar.com.datatsunami.bigdata.cobol;

import java.util.LinkedHashMap;
import java.util.Map;

import ar.com.datatsunami.bigdata.cobol.linehandler.LineHandler;

/**
 * Class to get values from a line of the dump.
 * 
 * This is a simpler version, not optimized for speed.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class CobolDumpParser extends BaseCobolDumpParser {

	public CobolDumpParser() {
		super();
	}

	public CobolDumpParser(LineHandler lineHandler) {
		super(lineHandler);
	}

	/**
	 * Parses the line and returns a map with all the field found.
	 * 
	 * @param line
	 * @return
	 * @throws ParserException
	 */
	public Map<String, Object> getValuesAsMap(String line) throws ParserException {
		this.lineHandler.prepareLine(line);

		Map<String, Object> map = new LinkedHashMap<String, Object>();
		for (int i = 0; i < this.fields.size(); i++) {

			final Field<?> item = this.fields.get(i);
			final String fieldString = this.lineHandler.getValueForField(i);

			String label = item.label;
			while (map.containsKey(label))
				label += "@";

			map.put(label, getObjectFromString(fieldString, item));

		}
		return map;
	}

	@Deprecated
	public Map<String, Object> getItemsWithLabels(String line) throws ParserException {
		return getValuesAsMap(line);
	}

	/**
	 * Returns the values of the requested fields
	 * 
	 * @param line
	 * @param fieldsNames
	 * @return
	 * @throws ParserException
	 */
	public Object[] getValues(String line, String[] fieldsNames) throws ParserException {
		this.lineHandler.prepareLine(line);
		Object[] ret = new Object[fieldsNames.length];
		for (int i = 0; i < fieldsNames.length; i++) {
			int fieldIndex = getFieldNameToIndexMap().get(fieldsNames[i]);
			String string = this.lineHandler.getValueForField(fieldIndex);
			ret[i] = getObjectFromString(string, fields.get(fieldIndex));
		}
		return ret;
	}

	/**
	 * Returns the values of the requested fields
	 * 
	 * @param line
	 * @param fieldsNames
	 * @return
	 * @throws ParserException
	 */
	public Object[] getValues(String line, int[] fields) throws ParserException {
		this.lineHandler.prepareLine(line);
		Object[] ret = new Object[fields.length];
		for (int i = 0; i < fields.length; i++) {
			String string = this.lineHandler.getValueForField(fields[i]);
			ret[i] = getObjectFromString(string, this.fields.get(fields[i]));
		}
		return ret;
	}

	/**
	 * Returns the value of the requested field
	 * 
	 * @param line
	 * @param fieldName
	 * @return
	 * @throws ParserException
	 */
	public Object getValue(String line, String fieldName) throws ParserException {
		return getValues(line, new String[] { fieldName })[0];
	}

	@Deprecated
	public Object[] getItemsValues(String line, String[] fieldsNames) throws ParserException {
		return getValues(line, fieldsNames);
	}

}
