package ar.com.datatsunami.bigdata.cobol.field;

/**
 * Indicates the requested field doens't exists.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class FieldNotFoundException extends RuntimeException {

	private static final long serialVersionUID = -2672565579303796029L;

	public static FieldNotFoundException forField(String fieldName) {
		return new FieldNotFoundException("The field with name '" + fieldName + "' does not exists");
	}

	public FieldNotFoundException() {
		super();
	}

	public FieldNotFoundException(String msg) {
		super(msg);
	}
}
