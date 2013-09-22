package ar.com.datatsunami.bigdata.cobol.converter;

public abstract class BaseDecimalConverter<I> extends CobolFieldToJavaConverter<I> {

	/** Whenever the field includes a + or - sign at the end */
	public final boolean withSign;

	/** How many places (at the end of the string) are for the decimals */
	public final int decimalPlaces;

	public BaseDecimalConverter(int decimalPlaces, boolean withSign) {
		if (decimalPlaces <= 0)
			throw new IllegalArgumentException("decimalPlaces must be greater than 0");
		this.decimalPlaces = decimalPlaces;
		this.withSign = withSign;
	}

}
