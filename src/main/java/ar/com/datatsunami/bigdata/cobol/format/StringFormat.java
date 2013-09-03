package ar.com.datatsunami.bigdata.cobol.format;

public class StringFormat extends Format<String> {

	public static final StringFormat DEFAULT = new StringFormat();

	@Override
	public String format(String value) throws InvalidFormatException {
		if (value == null)
			throw new InvalidFormatException("value no debe ser null");
		return value.trim();
	}

}