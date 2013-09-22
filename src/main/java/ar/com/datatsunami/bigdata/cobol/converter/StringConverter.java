package ar.com.datatsunami.bigdata.cobol.converter;

public class StringConverter extends CobolFieldToJavaConverter<String> {

	@Override
	public String format(String value) throws InvalidFormatException {
		if (value == null)
			throw new InvalidFormatException("value no debe ser null");
		return value.trim();
	}

}