package ar.com.datatsunami.bigdata.cobol;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ar.com.datatsunami.bigdata.cobol.format.InvalidFormatException;
import ar.com.datatsunami.bigdata.cobol.format.StringFormat;

/**
 * Class to parse a line and returns a map with the values.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class CobolDumpParser {

	/**
	 * The fields to be found in each line
	 */
	List<Field<?>> fields = new ArrayList<Field<?>>();

	boolean useRegex = true;

	LineHandler line = null;

	public CobolDumpParser() {
		this.line = new LineHandler(fields);
	}

	public CobolDumpParser add(Field<?> item) {
		this.fields.add(item);
		return this;
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

		this.line.prepareLine(line);

		Map<String, Object> map = new LinkedHashMap<String, Object>();
		for (int i = 0; i < this.fields.size(); i++) {

			final Field<?> item = this.fields.get(i);
			final String fieldString = this.line.getValueForField(i);

			String label = item.label;
			while (map.containsKey(label))
				label += "@";

			try {

				Object value;
				if (item.format == null)
					value = StringFormat.DEFAULT.format(fieldString);
				else
					value = item.format.format(fieldString);

				map.put(label, value);
			} catch (InvalidFormatException ifv) {
				throw new ParserException("No se pudo formatear field", ifv, item, fieldString);
			}
		}
		return map;
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
