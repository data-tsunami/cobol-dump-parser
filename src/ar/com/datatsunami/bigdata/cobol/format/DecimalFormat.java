package ar.com.datatsunami.bigdata.cobol.format;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DecimalFormat extends Format<Float> {

	boolean conSigno;
	int cantidadDecimales;
	Pattern pattern;

	public DecimalFormat(int cantidadDecimales, boolean conSigno) {
		this.conSigno = conSigno;
		this.cantidadDecimales = cantidadDecimales;
		if (this.conSigno)
			this.pattern = Pattern.compile("^(\\d+)(\\d{" + this.cantidadDecimales + "})(.)$");
		else
			this.pattern = Pattern.compile("^(\\d+)(\\d{" + this.cantidadDecimales + "})$");
	}

	public DecimalFormat(int cantidadDecimales) {
		// Default: con signo
		this(cantidadDecimales, true);
	}

	@Override
	public Float format(String value) throws InvalidFormatException {

		if (value == null)
			throw new InvalidFormatException("value no debe ser null");

		Matcher matcher = this.pattern.matcher(value);
		if (!matcher.matches())
			throw new InvalidFormatException("No es un decimal con signo valido: " + value);
		try {
			if (this.conSigno)
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