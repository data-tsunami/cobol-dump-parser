package ar.com.datatsunami.bigdata.cobol;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import ar.com.datatsunami.bigdata.cobol.format.InvalidFormatException;
import ar.com.datatsunami.bigdata.cobol.format.StringFormat;

/**
 * Class to parse a line and returns a map with the values.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class CobolParser {

	List<Field<?>> items = new ArrayList<Field<?>>();
	Pattern pattern = null;
	int anchoDeLinea = 0;

	public CobolParser() {
	}

	public CobolParser(Field<?>... items) {
		for (Field<?> item : items) {
			this.items.add(item);
		}
	}

	public void add(Field<?> item) {
		this.items.add(item);
	}

	public Pattern getPattern() {
		if (this.pattern == null) {
			String pat = "^";
			for (Field<?> item : items) {
				pat += item.genRegex();
				this.anchoDeLinea += item.cantidadLugares;
			}
			pat += ".*$";
			this.pattern = Pattern.compile(pat);
		}
		return this.pattern;
	}

	public Map<String, Object> getItemsWithLabels(String line) throws ParserException {

		Matcher matcher = this.getPattern().matcher(line);
		if (!matcher.matches()) {
			String msg = "No matcheo!\n";
			msg += " - Valor: '" + line + "'\n";
			msg += " - RegEx: '" + this.getPattern().pattern() + "'\n";
			msg += " - Valor (ancho): '" + line.length() + "'\n";
			msg += " - RegEx (ancho): " + this.anchoDeLinea + "\n";
			throw new IllegalArgumentException(msg);
		}

		Map<String, Object> map = new LinkedHashMap<String, Object>();
		for (int i = 0; i < this.items.size(); i++) {

			Field<?> item = this.items.get(i);
			String label = item.label;
			while (map.containsKey(label))
				label += "@";

			try {
				Object value;
				if (item.format == null)
					value = StringFormat.DEFAULT.format(matcher.group(i + 1));
				else
					value = item.format.format(matcher.group(i + 1));

				map.put(label, value);
			} catch (InvalidFormatException ifv) {
				throw new ParserException("No se pudo formatear field", ifv, item, matcher.group(i + 1));
			}
		}
		return map;
	}

	public Set<String> getHeader() {
		Map<String, String> map = new LinkedHashMap<String, String>();
		for (int i = 0; i < this.items.size(); i++) {
			String label = this.items.get(i).label;
			while (map.containsKey(label))
				label += "@";
			map.put(label, null);
		}
		return map.keySet();
	}

}
