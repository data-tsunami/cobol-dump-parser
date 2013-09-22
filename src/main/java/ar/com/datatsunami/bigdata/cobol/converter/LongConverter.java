package ar.com.datatsunami.bigdata.cobol.converter;

/**
 * Parses strings to Long. Valid values are '1', '-1', ' 1 ', ' -1 '.
 * 
 * Is NOT valid: '+1' or '1.0'
 * 
 * @author Horacio G. de Oro
 * 
 */
public class LongConverter extends CobolFieldToJavaConverter<Long> {

	public LongConverter() {
		super();
	}

	public LongConverter(Long valueForEmpty) {
		super(valueForEmpty);
	}

	@Override
	public Long convert(String value) throws InvalidFormatException {
		if (value == null)
			throw new InvalidFormatException("Value is null");

		value = value.trim();
		try {
			return Long.parseLong(value);
		} catch (NumberFormatException nfe) {
			try {
				return this.checkEmpty(value);
			} catch (NoDefaultDefinedOrValueNotEmptyException e) {
			}
			throw new InvalidFormatException(nfe);
		}

	}
}