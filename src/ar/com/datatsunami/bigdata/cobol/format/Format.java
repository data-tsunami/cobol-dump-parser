package ar.com.datatsunami.bigdata.cobol.format;

public abstract class Format<T> {

	protected T valueForEmpty = null;

	public Format() {
	}

	public Format(T valueForEmpty) {
		this.valueForEmpty = valueForEmpty;
	}

	/**
	 * Chequea si el valor es empty string, y si lo es, y hay configurado un
	 * valor por default, devuelve el valor por default.
	 * 
	 * En cualquier otro caso lanza excepcion, por lo tanto, el contrato es: si
	 * devuelve valor, es el que se debe utilizar. Sino: lanza excepcion
	 * NoDefaultDefinedOrValueNotEmptyException
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

}