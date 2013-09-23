package ar.com.datatsunami.bigdata.cobol;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ar.com.datatsunami.bigdata.cobol.converter.InvalidFormatException;
import ar.com.datatsunami.bigdata.cobol.field.Field;
import ar.com.datatsunami.bigdata.cobol.linehandler.LineHandler;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

/**
 * Base class for CobolDumpParsers.
 * 
 * The just created instance is in state "OPEN". While is open, the fields can
 * be added. When finish to add fields, a call to 'close()' must be done.
 * 
 * @author Horacio G. de Oro
 * 
 */
public abstract class BaseCobolDumpParser {

	protected boolean freezed = false;

	/**
	 * The fields to be found in each line. This list is shared with
	 * LineHandlers.
	 */
	protected final List<Field<?, ?>> fields = new ArrayList<Field<?, ?>>();

	protected LineHandler lineHandler = null;

	protected Map<String, Integer> fieldNameToIndexMap = null;

	public BaseCobolDumpParser() {
		this.lineHandler = new PositionalLineHandler();
		this.lineHandler.setFields(this.fields);
	}

	public BaseCobolDumpParser(LineHandler lineHandler) {
		this.lineHandler = lineHandler;
		this.lineHandler.setFields(this.fields);
	}

	public BaseCobolDumpParser add(Field<?, ?> item) {
		if (freezed)
			throw new RuntimeException("Instance is closed");
		this.fields.add(item);
		return this;
	}

	/**
	 * Freeze the instance and preare all the internal structures.
	 * 
	 * @return
	 */
	public BaseCobolDumpParser freeze() {
		if (freezed)
			throw new RuntimeException("Instance is closed");
		// Set the internal var
		this.freezed = true;

		// Freeze the line handler
		this.lineHandler.freeze();

		// Populate 'fieldNameToIndexMap'
		this.fieldNameToIndexMap = new HashMap<String, Integer>();
		for (int i = 0; i < this.fields.size(); i++) {
			String fieldName = this.fields.get(i).label;
			while (this.fieldNameToIndexMap.containsKey(fieldName))
				fieldName += "@";
			this.fieldNameToIndexMap.put(fieldName, Integer.valueOf(i));
		}

		return this;
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
	public List<Field<?, ?>> getFieldsWithError() {
		List<Field<?, ?>> withErrors = new ArrayList<Field<?, ?>>();
		for (Field<?, ?> field : this.fields)
			if (field.getErrorCount() > 0)
				withErrors.add(field);
		return withErrors;
	}

	/**
	 * Returns the index for a field
	 * 
	 * @param fieldName
	 * @return
	 */
	public int getFieldIndexFromFieldName(String fieldName) {
		return this.fieldNameToIndexMap.get(fieldName);
	}

	/**
	 * Transform the string in an object, raising a ParserException if an error
	 * is detected.
	 * 
	 * @param string
	 * @param field
	 * @return
	 * @throws ParserException
	 */
	protected Object getObjectFromString(String string, Field<?, ?> field) throws ParserException {
		try {
			return field.converter.convert(string);
		} catch (InvalidFormatException ifv) {
			throw new ParserException("Couldn't convert field", ifv, field, string);
		}
	}

	/**
	 * Returns an array of int, which represents the position of the passed
	 * fields.
	 * 
	 */
	public int[] getFieldIndexesFromNames(String... fieldsNames) {
		int indexes[] = new int[fieldsNames.length];
		for (int i = 0; i < fieldsNames.length; i++) {
			indexes[i] = this.fieldNameToIndexMap.get(fieldsNames[i]).intValue();
		}
		return indexes;
	}

	/*
	 * Getter
	 */

	public LineHandler getLineHandler() {
		return lineHandler;
	}

}
