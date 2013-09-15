package ar.com.datatsunami.bigdata.cobol.format;

public abstract class Format<T> {

	public static final Format<String> DEFAULT_FORMAT = new StringFormat();

	protected T valueForEmpty = null;

	public Format() {
	}

	public Format(T valueForEmpty) {
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

	public abstract T format(String value) throws InvalidFormatException;

	public void setValueForEmpty(T valueforEmpty) {
		this.valueForEmpty = valueforEmpty;
	}

}