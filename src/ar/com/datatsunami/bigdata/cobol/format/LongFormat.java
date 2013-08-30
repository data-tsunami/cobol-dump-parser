package ar.com.datatsunami.bigdata.cobol.format;

public class LongFormat extends Format<Long> {

	public LongFormat() {
		super();
	}

	public LongFormat(Long valueForEmpty) {
		this.valueForEmpty = valueForEmpty;
	}

	@Override
	public Long format(String value) throws InvalidFormatException {
		if (value == null)
			throw new InvalidFormatException("value no debe ser null");

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