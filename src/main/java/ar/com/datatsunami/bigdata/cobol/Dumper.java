package ar.com.datatsunami.bigdata.cobol;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.Map;

import ar.com.datatsunami.bigdata.cobol.field.Field;

public class Dumper {

	/** Options to use for this execution */
	DumperOpts dumperOpts = new DumperOpts();

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

	/**
	 * Errors found
	 */
	long lineErrorsCount = 0;

	/** The current line been processed */
	String line = null;

	PrintStream out = System.out;

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

	protected void handleError(ParserException pe) throws InterruptedException {

		lineErrorsCount++;

		String msg = "# ERROR at line " + lineNum;
		if (pe.field != null) {
			msg += " - " + pe.field;
			pe.field.incrErrorCount();
		}

		if (pe.value != null)
			msg += " - Value: '" + pe.value + "'";

		out.println(msg);
		pe.printStackTrace(out);
		if (dumperOpts.sleepAfterError)
			Thread.sleep(2000);
	}

	/**
	 * Parses the data and prints it as a CSV
	 * 
	 * Returns: line read
	 * 
	 * @throws IllegalArgumentException
	 * @throws IOException
	 * @throws InterruptedException
	 * 
	 */
	public long printCsv() throws IllegalArgumentException, IOException, InterruptedException {

		lineNum = 0;
		while ((line = this.bufferedReader.readLine()) != null) {
			lineNum++;

			if (dumperOpts.debug)
				out.println("# linea: " + line.substring(0, 15) + "...");

			try {
				Map<String, Object> map = cobolParser.getValuesAsMap(line);

				StringBuffer sb = new StringBuffer();
				for (String key : map.keySet()) {
					String value = map.get(key).toString();

					if (dumperOpts.debug)
						out.println("#  - " + key + ": " + value);

					if (!dumperOpts.reportErrorOnly) {
						if (sb.length() > 0)
							sb.append(",");

						sb.append(value);
					}
				}

				if (!dumperOpts.reportErrorOnly)
					out.println(sb.toString());

			} catch (ParserException pe) {
				handleError(pe);
				continue;
			}
		}

		return lineNum;

	}

	/**
	 * Prints a header
	 */
	public void printCsvHeader() {
		boolean first = true;
		for (String head : this.cobolParser.getHeader()) {
			if (!first) {
				out.print(",");
			}
			out.print(head);
			first = false;
		}
		out.println("");
	}

	/**
	 * Prints a report about lines and errors.
	 */
	public void printReport() {
		out.println("" + this.lineNum + " lines read");
		out.println("" + this.lineErrorsCount + " lines with errors");
		out.println("" + (this.lineNum - this.lineErrorsCount) + " lines were valid");
		out.println("" + (((this.lineErrorsCount * 1.0) / (this.lineNum * 1.0)) * 100.0)
				+ "% of lines are non-parseables.");

		for (Field<?> field : cobolParser.getFieldsWithError()) {
			out.println("  * " + field.getErrorCount() + " errors -> " + field);
		}
	}

	/* -------------------- Getters & setters -------------------- */

	public CobolDumpParser getCobolParser() {
		return cobolParser;
	}

	public void setCobolParser(CobolDumpParser cobolParser) {
		this.cobolParser = cobolParser;
	}

	public void setOutput(PrintStream out) {
		this.out = out;
	}
}
