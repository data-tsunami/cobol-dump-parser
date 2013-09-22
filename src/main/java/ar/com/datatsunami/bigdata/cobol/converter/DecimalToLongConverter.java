package ar.com.datatsunami.bigdata.cobol.converter;

/**
 * Parses strings to a Long. You must specify how many places occupy the
 * decimals, and if the string contains the sign or not.
 * 
 * Assuming 2 position for decimals and including the sign, valid values are:
 * '123+', '123-' (both equals to +/-1.23).
 * 
 * Assuming 3 position for decimals and NOT including the sign, valid values
 * are: '7654' (equals to 7.654).
 * 
 * @author Horacio G. de Oro
 * 
 */
public class DecimalToLongConverter extends BaseDecimalConverter<Long> {

	public static double toDouble(long value, int decimalPlaces) {
		return ((double) value) / Math.pow(10, decimalPlaces);
	}

	/**
	 * Creates a DecimalFormat instance.
	 * 
	 * @param cantidadDecimales
	 * @param withSign
	 */
	public DecimalToLongConverter(int decimalPlaces, boolean withSign) {
		super(decimalPlaces, withSign);
	}

	/**
	 * Creates a DecimalFormat instance, by default, with sign.
	 * 
	 * @param decimalPlaces
	 */
	public DecimalToLongConverter(int decimalPlaces) {
		// Default: WITH SIGN
		super(decimalPlaces, true);
	}

	@Override
	public Long convert(String value) throws InvalidFormatException {

		if (value == null)
			throw new InvalidFormatException("Value can't be null");

		value = value.trim();

		boolean isNegative = false;
		if (this.withSign) {
			if (value.endsWith("-"))
				isNegative = true;
			value = value.substring(0, value.length() - 1);
		}

		try {
			if (isNegative)
				return Long.parseLong(value) * -1;
			else
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