package ar.com.datatsunami.bigdata.cobol.format;

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
public class LongBasedDecimalFormat extends Format<Long> {

	public static double toDouble(long value, int decimalPlaces) {
		return ((double) value) / Math.pow(10, decimalPlaces);
	}

	/** Whenever the field includes a + or - sign at the end */
	public final boolean withSign;

	/** How many places (at the end of the string) are for the decimals */
	public final int decimalPlaces;

	/**
	 * Creates a DecimalFormat instance.
	 * 
	 * @param cantidadDecimales
	 * @param withSign
	 */
	public LongBasedDecimalFormat(int cantidadDecimales, boolean withSign) {
		this.withSign = withSign;
		this.decimalPlaces = cantidadDecimales;
		if (this.decimalPlaces <= 0)
			throw new IllegalArgumentException("cantidadDecimales must be greater than 0");
	}

	/**
	 * Creates a DecimalFormat instance, by default, with sign.
	 * 
	 * @param decimalPlaces
	 */
	public LongBasedDecimalFormat(int decimalPlaces) {
		// Default: WITH SIGN
		this(decimalPlaces, true);
	}

	@Override
	public Long format(String value) throws InvalidFormatException {

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