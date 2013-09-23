package ar.com.datatsunami.bigdata.cobol;

import java.util.LinkedHashMap;
import java.util.Map;

import ar.com.datatsunami.bigdata.cobol.field.Field;
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
	 * Parses the line and returns a map with all the field.
	 * 
	 * This method is very handy, but parses all the fields. If only a subset of
	 * the fields are required, it's recomended to use one of the
	 * <code>getValues()</code> methods.
	 * 
	 * @param line
	 * @return
	 * @throws ParserException
	 */
	public Map<String, Object> getValuesAsMap(String line) throws ParserException {
		this.lineHandler.prepareLine(line);

		Map<String, Object> map = new LinkedHashMap<String, Object>();
		for (int i = 0; i < this.fields.size(); i++) {

			final Field<?, ?> item = this.fields.get(i);
			final String fieldString = this.lineHandler.getValueForField(i);

			String label = item.label;
			while (map.containsKey(label))
				label += "@";

			map.put(label, getObjectFromString(fieldString, item));

		}
		return map;
	}

	/**
	 * Returns the values of the requested fields
	 * 
	 * @param line
	 * @param fieldsNames
	 * @return
	 * @throws ParserException
	 */
	public Object[] getValues(String line, String... fieldsNames) throws ParserException {
		this.lineHandler.prepareLine(line);
		return this.getValuesFast(fieldsNames);
	}

	public Object[] getValuesFast(String... fieldsNames) throws ParserException {
		Object[] ret = new Object[fieldsNames.length];
		for (int i = 0; i < fieldsNames.length; i++) {
			int fieldIndex = this.fieldNameToIndexMap.get(fieldsNames[i]);
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
	public Object[] getValues(String line, int... fields) throws ParserException {
		this.lineHandler.prepareLine(line);
		return getValuesFast(fields);
	}

	public Object[] getValuesFast(int... fields) throws ParserException {
		Object[] ret = new Object[fields.length];
		for (int i = 0; i < fields.length; i++) {
			String string = this.lineHandler.getValueForField(fields[i]);
			ret[i] = getObjectFromString(string, this.fields.get(fields[i]));
		}
		return ret;
	}

	/**
	 * Returns the value of one field
	 * 
	 * @param line
	 * @param fieldName
	 * @return
	 * @throws ParserException
	 */
	public Object getValue(String line, String fieldName) throws ParserException {
		return getValues(line, fieldName)[0];
	}

	public Object getValueFast(String fieldName) throws ParserException {
		return getValuesFast(fieldName)[0];
	}

	/**
	 * Returns the value of one field
	 * 
	 * @param line
	 * @param fieldName
	 * @return
	 * @throws ParserException
	 */
	public Object getValue(String line, int field) throws ParserException {
		return getValues(line, field)[0];
	}

	public Object getValue(int field) throws ParserException {
		return getValuesFast(field)[0];
	}

}
