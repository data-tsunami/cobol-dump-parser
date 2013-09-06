package ar.com.datatsunami.bigdata.cobol;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Handles the parsing and extracting of data from a line
 * 
 * @author Horacio G. de Oro
 * 
 */
public class RegexLineHandler {

	/**
	 * Regular expresion pattern to use (if activated).
	 */
	Pattern pattern = null;

	/**
	 * How many characters this field 'consumes'.
	 */
	int lineWidth = 0;

	Matcher matcher = null;

	List<Field<?>> fields;

	public RegexLineHandler(List<Field<?>> fields) {
		this.fields = fields;
	}

	public Pattern getPattern() {
		if (this.pattern != null)
			return this.pattern;

		String pat = "^";
		for (Field<?> item : this.fields) {
			pat += item.genRegex();
			this.lineWidth += item.width;
		}
		pat += ".*$";
		this.pattern = Pattern.compile(pat);

		return this.pattern;
	}

	public void prepareLine(String line) {
		matcher = this.getPattern().matcher(line);
		if (!matcher.matches()) {
			String msg = "Line didn't matched!\n";
			msg += " - Line: '" + line + "'\n";
			msg += " - Pattern: '" + this.getPattern().pattern() + "'\n";
			msg += " - Line width: '" + line.length() + "'\n";
			msg += " - Expected line width: " + this.lineWidth + "\n";
			throw new IllegalArgumentException(msg);
		}
	}

	public String getValueForField(int field) {
		return matcher.group(field + 1);
	}
}