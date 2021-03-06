package ar.com.datatsunami.bigdata.cobol.converter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses a Cobol 'decimal' field to a Float. You must specify how many places
 * occupy the decimals, and if the string contains the sign or not.
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
public class DecimalToFloatConverter extends BaseDecimalConverter<Float> {

	Pattern pattern;

	/**
	 * Creates a DecimalToFloatConverter instance.
	 * 
	 * @param cantidadDecimales
	 * @param withSign
	 */
	public DecimalToFloatConverter(int decimalPlaces, boolean withSign) {
		super(decimalPlaces, withSign);
		if (this.withSign)
			this.pattern = Pattern.compile("^(\\d+)(\\d{" + this.decimalPlaces + "})(.)$");
		else
			this.pattern = Pattern.compile("^(\\d+)(\\d{" + this.decimalPlaces + "})$");
	}

	/**
	 * Creates a DecimalToFloatConverter instance, by default, with sign.
	 * 
	 * @param decimalPlaces
	 */
	public DecimalToFloatConverter(int decimalPlaces) {
		// Default: WITH SIGN
		this(decimalPlaces, true);
	}

	@Override
	public Float convert(String value) throws InvalidFormatException {

		if (value == null)
			throw new InvalidFormatException("Value can't be null");

		value = value.trim();

		Matcher matcher = this.pattern.matcher(value);
		if (!matcher.matches()) {
			try {
				return this.checkEmpty(value);
			} catch (NoDefaultDefinedOrValueNotEmptyException e) {
			}
			throw new InvalidFormatException("The value isn't a valid decimal with sign: '" + value + "'");
		}

		try {
			if (this.withSign)
				return Float.valueOf("" + matcher.group(3) + "" + matcher.group(1) + "." + matcher.group(2));
			else
				return Float.valueOf("" + matcher.group(1) + "." + matcher.group(2));
		} catch (NumberFormatException nfe) {
			try {
				return this.checkEmpty(value);
			} catch (NoDefaultDefinedOrValueNotEmptyException e) {
			}
			throw new InvalidFormatException(nfe);
		}
	}

}