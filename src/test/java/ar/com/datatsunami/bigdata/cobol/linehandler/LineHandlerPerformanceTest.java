package ar.com.datatsunami.bigdata.cobol.linehandler;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class LineHandlerPerformanceTest {

	@Test
	public void comparePerformance() throws ParserException {

		long iterations;
		if (System.getenv().containsKey("ITERATIONS"))
			iterations = Long.parseLong(System.getenv("ITERATIONS"));
		else
			iterations = 100000;

		/*
		 * Default
		 */
		CobolDumpParser cdpDefault = new CobolDumpParser();
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cdpDefault);

		final long startDefault = System.currentTimeMillis();
		for (int i = 0; i < iterations; i++)
			cdpDefault.getItemsWithLabels(LineHandlerTestUtils.line1).get("Code");
		final long endDefault = System.currentTimeMillis();

		/*
		 * Positional
		 */
		CobolDumpParser cdpPositional = new CobolDumpParser(new PositionalLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cdpPositional);

		final long startPositional = System.currentTimeMillis();
		String[] fieldsNamed = new String[] { "Code" };
		for (int i = 0; i < iterations; i++)
			cdpDefault.getItemsValues(LineHandlerTestUtils.line1, fieldsNamed);
		final long endPositional = System.currentTimeMillis();

		/*
		 * Print results
		 */
		System.out.println(" - Default: " + ((endDefault - startDefault) / 1000.0) + " secs.");
		System.out.println(" - Positional: " + ((endPositional - startPositional) / 1000.0) + " secs.");
	}

}
