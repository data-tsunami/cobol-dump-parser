package ar.com.datatsunami.bigdata.cobol;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;

public class Dumper {

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

	/** Got from environment */
	boolean debug = "true".equals(System.getenv().get(DEBUG));

	/** Got from environment */
	boolean reportErrorOnly = "true".equals(System.getenv().get(REPORT_ERRORS_ONLY));

	/** Got from environment */
	boolean sleepAfterError = "true".equals(System.getenv().get(SLEEP_AFTER_ERROR));

	/** Reader for the file */
	BufferedReader bufferedReader = null;

	/** The parser to use with each line */
	CobolDumpParser cobolParser = null;

	/**
	 * Line number been processed. Used to enhance error reports
	 * 
	 * First line is number '1'
	 */
	long lineNum = 0; // First line is 1

	/** The current line been processed */
	String line = null;

	public void setBufferedReader(BufferedReader br) {
		if (this.bufferedReader != null)
			throw new RuntimeException("You have setted bufferedReader, and can't be overiden");

		this.bufferedReader = br;
	}

	public void setInputFileName(String filename) throws FileNotFoundException {
		this.setBufferedReader(new BufferedReader(new FileReader(new File(filename))));
	}

	public void setInputStream(InputStream is) {
		this.setBufferedReader(new BufferedReader(new InputStreamReader(is)));
	}

	protected void reportError(ParserException pe) throws InterruptedException {
		String msg = "# ERROR at line " + lineNum;
		if (pe.field != null)
			msg += " - " + pe.field;

		if (pe.value != null)
			msg += " - Value: '" + pe.value + "'";

		System.out.println(msg);
		pe.printStackTrace(System.out);
		if (sleepAfterError)
			Thread.sleep(2000);
	}

	/**
	 * Parses the data and prints it as a CSV
	 * 
	 * @throws IllegalArgumentException
	 * @throws IOException
	 * @throws InterruptedException
	 */
	public void printCsv() throws IllegalArgumentException, IOException, InterruptedException {

		lineNum = 0;
		while ((line = this.bufferedReader.readLine()) != null) {
			lineNum++;

			if (debug)
				System.out.println("# linea: " + line.substring(0, 15) + "...");

			try {
				Map<String, Object> map = cobolParser.getItemsWithLabels(line);

				StringBuffer sb = new StringBuffer();
				for (String key : map.keySet()) {
					String value = map.get(key).toString();

					if (debug)
						System.out.println("#  - " + key + ": " + value);

					if (!this.reportErrorOnly) {
						if (sb.length() > 0)
							sb.append(",");

						sb.append(value);
					}
				}

				if (!this.reportErrorOnly)
					System.out.println(sb.toString());

			} catch (ParserException pe) {
				reportError(pe);
				continue;
			}
		}

	}

	/* -------------------- Getters & setters -------------------- */

	public CobolDumpParser getCobolParser() {
		return cobolParser;
	}

	public void setCobolParser(CobolDumpParser cobolParser) {
		this.cobolParser = cobolParser;
	}

}
