package ar.com.datatsunami.bigdata.cobol.format;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses strings to Float. You must specify how many places occupy the
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
public class DecimalFormat extends Format<Float> {

	boolean withSign;
	int cantidadDecimales;
	Pattern pattern;

	/**
	 * Creates a DecimalFormat instance.
	 * 
	 * @param cantidadDecimales
	 * @param conSigno
	 */
	public DecimalFormat(int cantidadDecimales, boolean conSigno) {
		this.withSign = conSigno;
		this.cantidadDecimales = cantidadDecimales;
		if (this.cantidadDecimales <= 0)
			throw new IllegalArgumentException("cantidadDecimales must be greater than 0");

		if (this.withSign)
			this.pattern = Pattern.compile("^(\\d+)(\\d{" + this.cantidadDecimales + "})(.)$");
		else
			this.pattern = Pattern.compile("^(\\d+)(\\d{" + this.cantidadDecimales + "})$");
	}

	/**
	 * Creates a DecimalFormat instance, by default, with sign.
	 * 
	 * @param cantidadDecimales
	 */
	public DecimalFormat(int cantidadDecimales) {
		// Default: con signo
		this(cantidadDecimales, true);
	}

	@Override
	public Float format(String value) throws InvalidFormatException {

		if (value == null)
			throw new InvalidFormatException("value no debe ser null");

		value = value.trim();

		Matcher matcher = this.pattern.matcher(value);
		if (!matcher.matches()) {
			try {
				return this.checkEmpty(value);
			} catch (NoDefaultDefinedOrValueNotEmptyException e) {
			}
			throw new InvalidFormatException("No es un decimal con signo valido: " + value);
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