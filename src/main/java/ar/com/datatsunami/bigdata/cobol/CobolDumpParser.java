package ar.com.datatsunami.bigdata.cobol;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.hadoop.io.Text;

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
	 * Parses the line and returns a map with all the field found.
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

	/**
	 * Same than {@link #getItemsWithLabels(String)}, but receives a
	 * {@link Text} instead of a String.
	 * 
	 * @param line
	 * @return
	 * @throws ParserException
	 */
	public Map<String, Object> getItemsWithLabels(Text line) throws ParserException {
		return getItemsWithLabels(line.toString());
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

	/**
	 * Returns the values of the fields requested
	 * 
	 * @param line
	 * @param fieldsNames
	 * @return
	 * @throws ParserException
	 */
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
	 * The same than {@link getItemsValues(String, String[])} , but receives a
	 * {@link Text} instead of a String.
	 * 
	 * @param line
	 * @param fieldsNames
	 * @return
	 * @throws ParserException
	 */
	public Object[] getItemsValues(Text line, int[] fieldIndexes) throws ParserException {
		this.lineHandler.prepareText(line);
		Object[] ret = new Object[fieldIndexes.length];

		for (int i = 0; i < fieldIndexes.length; i++) {
			String string = this.lineHandler.getValueForField(fieldIndexes[i]);
			ret[i] = getObjectFromString(string, fields.get(fieldIndexes[i]));
		}
		return ret;
	}

	/**
	 * Copies to <code>ret</code> the values of the fields referenced by
	 * <code>indexes</code>
	 * 
	 * @param line
	 * @param fieldIndexes
	 * @param ret
	 * @throws ParserException
	 */
	public void copyItemsValuesByFieldIndexes(String line, int[] fieldIndexes, Object[] ret)
			throws ParserException {
		this.lineHandler.prepareLine(line);
		for (int i = 0; i < fieldIndexes.length; i++) {
			ret[i] = getObjectFromString(this.lineHandler.getValueForField(fieldIndexes[i]),
					fields.get(fieldIndexes[i]));
		}
	}

	public void copyItemsValuesByFieldIndexes(Text text, int[] fieldIndexes, Text[] out)
			throws ParserException {
		this.lineHandler.prepareText(text);
		for (int i = 0; i < fieldIndexes.length; i++) {
			this.lineHandler.copyValue(fieldIndexes[i], out[i]);
		}
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

	public int getFieldIndexFromFieldName(String fieldName) {
		return getFieldNameToIndexMap().get(fieldName);
	}

}
