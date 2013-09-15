package ar.com.datatsunami.bigdata.cobol.linehandler;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.NotImplementedException;
import org.apache.hadoop.io.Text;

import ar.com.datatsunami.bigdata.cobol.field.Field;

/**
 * Handles the parsing and extracting of data from a line using regular
 * expressions.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class RegexLineHandler implements LineHandler {

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

	public RegexLineHandler() {
		this.fields = null;
	}

	public RegexLineHandler(List<Field<?>> fields) {
		this.fields = fields;
	}

	private Pattern getPattern() {
		if (this.pattern != null)
			return this.pattern;

		String pat = "^";
		for (Field<?> item : this.fields) {
			pat += item.genRegex();
			this.lineWidth += item.getWidth();
		}
		pat += ".*$";
		this.pattern = Pattern.compile(pat);

		return this.pattern;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * ar.com.datatsunami.bigdata.cobol.LineHandler#prepareLine(java.lang.String
	 * )
	 */
	@Override
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see ar.com.datatsunami.bigdata.cobol.LineHandler#getValueForField(int)
	 */
	@Override
	public String getValueForField(int field) {
		return matcher.group(field + 1);
	}

	public void setFields(List<Field<?>> fields) {
		this.fields = fields;
	}

	@Override
	public void prepareText(Text line) {
		throw new NotImplementedException();
	}

	@Override
	public void copyValue(int field, Text output) {
		throw new NotImplementedException();
	}
}