package ar.com.datatsunami.bigdata.cobol;

public class DumperOpts {

	/**
	 * Set this environment variable to 'true' to print debug messages for each
	 * line.
	 */
	public static final String DEBUG = "DEBUG";

	/**
	 * Set this environment variable to 'true' to avoid printing information,
	 * and only reports the errors.
	 */
	public static final String REPORT_ERRORS_ONLY = "REPORT_ERRORS_ONLY";

	/**
	 * Set this environment variable to 'true' to sleep 2 seconds after
	 * reporting an error.
	 */
	public static final String SLEEP_AFTER_ERROR = "SLEEP_AFTER_ERROR";

	boolean debug = "true".equals(System.getenv().get(DEBUG));

	boolean reportErrorOnly = "true".equals(System.getenv().get(REPORT_ERRORS_ONLY));

	boolean sleepAfterError = "true".equals(System.getenv().get(SLEEP_AFTER_ERROR));

	public DumperOpts() {
	}

}
