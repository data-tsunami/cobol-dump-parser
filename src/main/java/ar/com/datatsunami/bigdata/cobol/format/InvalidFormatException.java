package ar.com.datatsunami.bigdata.cobol.format;

public class InvalidFormatException extends Exception {

	private static final long serialVersionUID = 5088113420235418928L;

	public InvalidFormatException() {
		super();
	}

	public InvalidFormatException(String message) {
		super(message);
	}

	public InvalidFormatException(String message, Throwable cause) {
		super(message, cause);
	}

	public InvalidFormatException(Throwable cause) {
		super(cause);
	}
}
