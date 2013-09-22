package ar.com.datatsunami.bigdata.cobol.converter;

/**
 * Base class for classes that convert a field from a Cobol dump (txt) to a Java
 * object.
 * 
 * @author Horacio G. de Oro
 * 
 * @param <T>
 */
public abstract class CobolFieldToJavaConverter<T> {

	/**
	 * Default parser
	 */
	public static final CobolFieldToJavaConverter<String> DEFAULT_FORMAT = new StringConverter();

	protected T valueForEmpty = null;

	public CobolFieldToJavaConverter() {
	}

	public CobolFieldToJavaConverter(T valueForEmpty) {
		this.valueForEmpty = valueForEmpty;
	}

	/**
	 * Checks if the received value to parse is an empty string. If it is, and
	 * there is a default value configured, returns that default value.
	 * 
	 * In any other case (no default configured or the value received is not an
	 * empty string) the NoDefaultDefinedOrValueNotEmptyException is thrown
	 * 
	 * @param value
	 * @return
	 * @throws NoDefaultDefinedOrValueNotEmptyException
	 */
	public T checkEmpty(String value) throws NoDefaultDefinedOrValueNotEmptyException {
		if (this.valueForEmpty == null)
			throw new NoDefaultDefinedOrValueNotEmptyException();
		if (value.trim().length() == 0)
			return this.valueForEmpty;
		throw new NoDefaultDefinedOrValueNotEmptyException();
	}

	public abstract T convert(String value) throws InvalidFormatException;

	/*
	 * Setter
	 */
	public void setValueForEmpty(T valueforEmpty) {
		this.valueForEmpty = valueforEmpty;
	}

}