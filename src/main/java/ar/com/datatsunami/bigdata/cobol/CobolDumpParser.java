package ar.com.datatsunami.bigdata.cobol;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ar.com.datatsunami.bigdata.cobol.format.InvalidFormatException;
import ar.com.datatsunami.bigdata.cobol.format.StringFormat;
import ar.com.datatsunami.bigdata.cobol.linehandler.LineHandler;
import ar.com.datatsunami.bigdata.cobol.linehandler.RegexLineHandler;

/**
 * Class to parse a line and returns a map with the values.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class CobolDumpParser {

	/**
	 * The fields to be found in each line. This list is shared with
	 * LineHandlers.
	 */
	private final List<Field<?>> fields = new ArrayList<Field<?>>();

	boolean useRegex = true;

	LineHandler lineHandler = null;

	protected Map<String, Integer> fieldNameToIndexMap = null;

	public CobolDumpParser() {
		this.lineHandler = new RegexLineHandler(fields);
	}

	public CobolDumpParser(LineHandler lineHandler) {
		this.lineHandler = lineHandler;
		this.lineHandler.setFields(this.fields);
	}

	public CobolDumpParser add(Field<?> item) {
		this.fields.add(item);
		return this;
	}

	protected Map<String, Integer> getFieldNameToIndexMap() {
		if (fieldNameToIndexMap == null) {
			Map<String, Integer> tmp = new HashMap<String, Integer>();
			for (int i = 0; i < this.fields.size(); i++) {
				String fieldName = this.fields.get(i).label;
				while (tmp.containsKey(fieldName))
					fieldName += "@";
				tmp.put(fieldName, Integer.valueOf(i));
			}
			this.fieldNameToIndexMap = tmp;
		}
		return this.fieldNameToIndexMap;
	}

	/**
	 * Parses the line, using regular expresions, and returns a map with the
	 * field found.
	 * 
	 * @param line
	 * @return
	 * @throws ParserException
	 */
	public Map<String, Object> getItemsWithLabels(String line) throws ParserException {

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

	protected Object getObjectFromString(String string, Field<?> field) throws ParserException {
		try {
			if (field.format == null)
				return StringFormat.DEFAULT.format(string);
			else
				return field.format.format(string);
		} catch (InvalidFormatException ifv) {
			throw new ParserException("No se pudo formatear field", ifv, field, string);
		}
	}

	public Object[] getItemsValues(String line, String[] fieldsNames) throws ParserException {
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
	 * Returns a sorted set with the headers names.
	 * 
	 * @return
	 */
	public Set<String> getHeader() {
		Map<String, String> map = new LinkedHashMap<String, String>();
		for (int i = 0; i < this.fields.size(); i++) {
			String label = this.fields.get(i).label;
			while (map.containsKey(label))
				label += "@";
			map.put(label, null);
		}
		return map.keySet();
	}

	/**
	 * Returns the list of fields which had at least one error.
	 * 
	 * @return
	 */
	public List<Field<?>> getFieldsWithError() {
		List<Field<?>> withErrors = new ArrayList<Field<?>>();
		for (Field<?> field : this.fields)
			if (field.errorCount > 0)
				withErrors.add(field);
		return withErrors;
	}
}
